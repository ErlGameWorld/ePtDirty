-module(ePtDirty).
-author("Erlang Parse Transform Generator").
-export([parse_transform/2]).

%% ===================================================================
%% 配置宏定义
%% ===================================================================
%% 需要自动维护脏标记的字段名
-define(DirtyField, dirtyFlag).
%% 生成临时变量的前缀，防止与用户变量冲突
-define(T_VAR_PREFIX, "__PtDirtyT").

%% ===================================================================
%% 模块说明
%% ===================================================================
%% 功能：
%%   这是一个 Erlang Parse Transform（语法转换器）。
%%   它在编译期间自动扫描包含 `dirtyFlag` 字段的 Record 定义。
%%   当代码中出现 Record 更新操作（`Var#rec{...}`）时，自动注入脏标记维护逻辑。
%%
%% 脏标记逻辑：
%%   每个字段对应一个 Bit（位）。
%%   NewDirtyFlag = OldDirtyFlag bor ModifiedFieldsMask.
%%
%% 安全性策略（Strategy B）：
%%   本模块采取“折中且安全”的策略：
%%   1. 仅拦截 Record Update (Var#rec{...})。
%%   2. 忽略 Record Creation (#rec{...}) 和 Pattern Matching (case #rec{}...)。
%%   原因：在 Erlang AST 中，Creation 和 Matching 结构完全一致，无法通过类型区分。
%%        若错误拦截 Matching，会导致模式匹配逻辑失效。
%%        而 Update 操作只能出现在表达式中，因此拦截 Update 是绝对安全的。
%%   3. dirtyFlag 必须是整数（且建议 record 定义中默认值为 0）否则运行时可能出现：undefined bor Mask -> badarg
%% ===================================================================

%% ===================================================================
%% 1. 入口函数 (Entry Point)
%% ===================================================================
%% 工作流程：
%%   1) 扫描所有 -record(...) 定义，筛选出包含 dirtyFlag 的 record
%%   2) 建立元数据：RecMeta = #{RecName => #{FieldName => Bit, ...}}
%%   3) 将 Forms 转为 syntax tree，然后全树遍历并对 record update 注入 dirtyFlag 逻辑
%%   4) revert 回 Forms
parse_transform(Forms, _Options) ->
   try
      %% Step 1: 扫描所有 Record 定义，找出包含 dirtyFlag 的 Record，并计算字段位掩码
      RecMeta = scan_records(Forms),

      case maps:size(RecMeta) of
         0 ->
            %% 如果没有符合条件的 Record，直接返回原代码，不做任何改动
            Forms;
         _ ->
            %% 初始化临时变量计数器（用于处理复杂表达式更新时的临时变量命名）
            put(pt_var_counter, 9966),

            %% Step 2: 将 Erlang Forms 转换为 Syntax Tree (AST)，便于操作
            Tree = erl_syntax:form_list(Forms),

            %% Step 3: 遍历 AST，执行核心转换逻辑
            %% 使用 erl_syntax_lib:map/2 进行深度遍历
            NewTree = erl_syntax_lib:map(fun(Node) -> transform_node(Node, RecMeta) end, Tree),

            %% Step 4: 将修改后的 AST 还原为 Erlang Forms
            erl_syntax:revert_forms(NewTree)
      end
   catch
      Class:Reason:Stack ->
         %% 容错处理：
         %% 如果转换插件本身发生 crash，打印警告并降级返回原始代码。
         %% 这样可以避免因为辅助工具的 bug 导致整个项目无法编译。
         ModName = find_module_name(Forms),
         io:format("~n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~n"),
         io:format("Warning: ePtDirty ParseTransform Failed~n"),
         io:format("Module: ~p~n", [ModName]), %% 这里会打印模块名
         io:format("Reason: ~p:~p~n", [Class, Reason]),
         io:format("Stack : ~p~n", [Stack]),
         io:format("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~n~n"),
         Forms

   end.

%% 查找模块名
find_module_name(Forms) ->
   case lists:keyfind(module, 3, Forms) of
      {attribute, _, module, Mod} -> Mod;
      _ -> unknown_module
   end.
%% ===================================================================
%% 2. 核心节点遍历 (AST Traversal)
%% ===================================================================

transform_node(Node, RecMeta) ->
   case erl_syntax:type(Node) of
      record_expr ->
         %% 这是一个 Record 表达式 (可能是创建、匹配、或更新)

         %% 获取 Record 的参数部分 (即 Var#rec{...} 中的 Var)
         Arg = erl_syntax:record_expr_argument(Node),

         %% 【关键安全检查】
         %% erl_syntax:record_expr_argument/1 的返回值决定了操作类型：
         %% - none : 对应 #rec{...}。这既可能是创建(Expr)，也可能是匹配(Pattern)。
         %%          为了防止破坏 Pattern Matching，我们绝对不能修改此类节点。
         %% - Val  : 对应 Val#rec{...}。这是 Record 更新操作。
         %%          Erlang 语法禁止在 Pattern 中使用更新操作，因此此类节点必定是 Expression。
         %%          这是我们唯一可以安全注入逻辑的地方。
         case Arg of
            none ->
               %% 创建或匹配 -> 跳过
               Node;
            _ ->
               %% 更新操作 -> 检查是否是受控 Record
               TypeAtom = erl_syntax:record_expr_type(Node),
               RecName = erl_syntax:atom_value(TypeAtom),

               case maps:find(RecName, RecMeta) of
                  {ok, FieldBits} ->
                     %% 是受控 Record，准备注入
                     Fields = erl_syntax:record_expr_fields(Node),
                     inject_update_logic(Node, Arg, RecName, Fields, FieldBits);
                  error ->
                     %% 普通 Record -> 跳过
                     Node
               end
         end;
      _ ->
         %% 其他类型节点 -> 原样返回
         Node
   end.

%% ===================================================================
%% 3. 注入逻辑 (Injection Logic)
%% ===================================================================

inject_update_logic(OriginalNode, Arg, RecName, Fields, FieldBits) ->
   Pos = erl_syntax:get_pos(OriginalNode),

   %% 分析当前更新语句中包含哪些字段，计算掩码
   %% 解析本次更新的字段列表：
   %%   MaybeDirtyExpr: 是否出现 dirtyFlag=...
   %%   OtherFields:    除 dirtyFlag 外的字段节点（保持原样）
   %%   MaskVal:        本次更新字段对应 bit 的 OR 结果
   {MaybeDirtyExpr, OtherFields, MaskVal} = analyze_fields(Fields, FieldBits),

   case MaybeDirtyExpr of
      {value, _} ->
         %% 情况 A：用户在代码中手动给 dirtyFlag 赋值了 (例如 P#player{dirtyFlag=0})
         %% 策略：尊重用户意图，不进行自动注入
         OriginalNode;
      undefined ->
         if
            MaskVal == 0 ->
               %% 情况 B：用户修改的字段全都不在监控列表中 (或者只修改了通配符 _)
               %% 策略：无需注入
               OriginalNode;
            true ->
               %% 情况 C：检测到受控字段修改
               %% 策略：注入自动维护代码  追加 dirtyFlag = Old bor Mask
               do_update_inject(Pos, Arg, RecName, OtherFields, MaskVal)
         end
   end.

do_update_inject(Pos, RecExpr, RecName, OtherFields, MaskVal) ->
   case erl_syntax:type(RecExpr) of
      variable ->
         %% 情况 C-1：简单变量更新
         %% 源码：P#player{hp = 1}
         %% 目标：P#player{hp = 1, dirtyFlag = P#player.dirtyFlag bor Mask}
         build_update_ops(Pos, RecExpr, RecName, OtherFields, MaskVal);
      _ ->
         %% 情况 C-2：复杂表达式更新
         %% 源码：(get_player())#player{hp = 1}
         %% 风险：如果直接替换为 (Expr)#player{..., dirtyFlag = (Expr)#player.dirtyFlag...}
         %%      会导致 Expr 被执行两次。如果 Expr 有副作用(side effect)，逻辑会出错。
         %% 目标：
         %%      begin
         %%         Tmp = get_player(),
         %%         Tmp#player{hp = 1, dirtyFlag = Tmp#player.dirtyFlag bor Mask}
         %%      end
         TmpVar = gen_temp_var(Pos),
         MatchExpr = set_pos(erl_syntax:match_expr(TmpVar, RecExpr), Pos),
         UpdateExpr = build_update_ops(Pos, TmpVar, RecName, OtherFields, MaskVal),
         set_pos(erl_syntax:block_expr([MatchExpr, UpdateExpr]), Pos)
   end.

build_update_ops(Pos, VarExpr, RecName, OtherFields, MaskVal) ->
   %% 1. 生成获取旧标记的代码: Var#rec.dirtyFlag
   OldFlagExpr = set_pos(erl_syntax:record_access(VarExpr, atom(Pos, RecName), atom(Pos, ?DirtyField)), Pos),

   %% 2. 生成计算新标记的代码: OldFlag bor Mask
   MaskExpr = set_pos(erl_syntax:integer(MaskVal), Pos),
   NewFlagExpr = set_pos(erl_syntax:infix_expr(OldFlagExpr, erl_syntax:operator('bor'), MaskExpr), Pos),

   %% 3. 构建新的 dirtyFlag 字段赋值 AST
   NewDirtyField = set_pos(erl_syntax:record_field(atom(Pos, ?DirtyField), NewFlagExpr), Pos),

   %% 4. 合并所有字段
   AllFields = OtherFields ++ [NewDirtyField],

   %% 5. 生成最终的 Record 更新表达式
   set_pos(erl_syntax:record_expr(VarExpr, atom(Pos, RecName), AllFields), Pos).

%% ===================================================================
%% 4. 字段分析工具 (Field Analysis)
%% ===================================================================

%% analyze_fields/2
%% 功能：遍历本次更新的所有字段，计算总掩码，并检查用户是否手动操作了 dirtyFlag
%% 返回：{MaybeDirtyExpr, FieldsWithoutDirty, TotalMask}
analyze_fields(Fields, FieldBits) ->
   lists:foldr(fun(F, {DirtyExpr, FieldsAcc, MaskAcc}) ->
      NameNode = erl_syntax:record_field_name(F),
      Name = erl_syntax:atom_value(NameNode),

      case Name of
         ?DirtyField ->
            %% 发现用户显式赋值了 dirtyFlag
            Val = erl_syntax:record_field_value(F),

            %% 如果有多次赋值（虽然语法不允许），取最后一次
            NewDirty = case DirtyExpr of
               undefined -> {value, Val};
               _ -> DirtyExpr
            end,
            %% 既然用户手动处理了，我们就不累加 Mask，也不在 FieldsAcc 中保留原有的 dirtyFlag 字段
            %% (因为外层逻辑会决定是否直接返回 OriginalNode)
            {NewDirty, FieldsAcc, MaskAcc};
         _ ->
            %% 普通字段：查表获取对应的 Bit
            %% maps:get/3 的默认值 0 处理了通配符 `_` 的情况
            Bit = maps:get(Name, FieldBits, 0),
            {DirtyExpr, [F | FieldsAcc], MaskAcc bor Bit}
      end
   end, {undefined, [], 0}, Fields).

%% ===================================================================
%% 5. Record 定义扫描与元数据构建 (Metadata Scanning)
%% ===================================================================

%% scan_records/1
%% 功能：扫描 AST 中的 attribute，寻找包含 dirtyFlag 的 record 定义
scan_records(Forms) ->
   lists:foldl(fun
      ({attribute, _, record, {Name, Fields}}, Acc) ->
         %% 计算该 Record 所有字段的位分布
         case calc_field_bits(Fields, 2, #{}, false) of
            {true, BitMap} ->
               %% 只有包含 dirtyFlag 的 Record 才会被记录
               Acc#{Name => BitMap};
            _ ->
               Acc
         end;
      (_, Acc) ->
         Acc
   end, #{}, Forms).

%% calc_field_bits/4
%% 参数：
%%   Bit: 当前字段分配的位，从 2 开始 (1 往往保留或避免混淆)
%%   HasDirty: 标记是否找到了 dirtyFlag 字段
calc_field_bits([], _Bit, Map, HasDirty) ->
   {HasDirty, Map};
calc_field_bits([F | Rest], Bit, Map, HasDirty) ->
   case get_field_name(F) of
      {error, bad_field} ->
         %% 遇到无法解析的字段（可能是语法错误），跳过但保持位移正确
         calc_field_bits(Rest, Bit bsl 1, Map, HasDirty);
      ?DirtyField ->
         %% 找到目标字段，标记 HasDirty = true
         calc_field_bits(Rest, Bit bsl 1, Map#{?DirtyField => Bit}, true);
      Name ->
         %% 普通字段，记录映射关系 Name -> Bit
         calc_field_bits(Rest, Bit bsl 1, Map#{Name => Bit}, HasDirty)
   end.

%% get_field_name/1
%% 功能：健壮地从 record_field AST 中提取字段名
%% 支持：无默认值、有默认值、带类型说明的字段
get_field_name({record_field, _, {atom, _, Name}}) -> Name;
get_field_name({record_field, _, {atom, _, Name}, _Def}) -> Name;
get_field_name({typed_record_field, Field, _}) -> get_field_name(Field);
get_field_name(_) -> {error, bad_field}.

%% ===================================================================
%% 6. 辅助函数 (Helpers)
%% ===================================================================

%% 生成唯一的临时变量名 生成临时变量：__PtDirtyT9966, __PtDirtyT9967, ...
gen_temp_var(Pos) ->
   Count = get(pt_var_counter),
   put(pt_var_counter, Count + 1),
   NameStr = ?T_VAR_PREFIX ++ integer_to_list(Count),
   set_pos(erl_syntax:variable(list_to_atom(NameStr)), Pos).

%% 构造 atom 节点的简便方法
atom(Pos, A) -> set_pos(erl_syntax:atom(A), Pos).

%% 设置节点行号（位置信息），这对编译器报错准确性至关重要
set_pos(Node, Pos) -> erl_syntax:set_pos(Node, Pos).