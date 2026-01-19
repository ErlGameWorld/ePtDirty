# ePtDirty - Erlang Record 脏标记自动维护工具

[简体中文](README.md) | [English](README_EN.md)

`ePtDirty` 是一个 Erlang 语法转换器（Parse Transform）。它能够在编译期间自动扫描代码，拦截 **Record Update**
（记录更新）表达式，并自动注入维护 `dirtyFlag`（脏字段位图）的逻辑。

该工具主要用于游戏开发或高频数据交互场景，便于实现数据的**增量存储**（回写数据库）或**增量同步**（发送给客户端）。

---

## 1. 快速接入 (Quick Start)

### 1.1 引入编译选项

在需要自动维护脏标记的模块中，添加以下编译选项。

**方式 A：在源文件中添加**

```erlang
-module(my_player).
-compile({parse_transform, ePtDirty}).
```

**方式 B：在 Rebar3 配置 (`rebar.config`) 中全局添加**

```erlang
{erl_opts, [
debug_info,
{parse_transform, ePtDirty}
]}.
```

### 1.2 定义 Record

你的 Record 定义中必须包含名为 `dirtyFlag` 的字段。

> **注意**：如果没有此字段，ePtDirty 会忽略该 Record，不会产生任何副作用。

```erlang
-record(player, {
   id,
   hp,
   mp,
   %% [必要配置] 必须指定默认值为整数（建议为 0）
   dirtyFlag = 0
}).
```

### 1.3 编写代码

你只需要编写标准的 Erlang 更新代码，编译器会自动处理剩下的工作：

```erlang
%% 源码
update_hp(P0) ->
   P0#player{hp = 100}.

%% 编译后的等效代码 (伪代码)
update_hp(P0) ->
   P0#player{hp = 100, dirtyFlag = P0#player.dirtyFlag bor Mask}.
```

其中 `Mask` 根据 record 定义时字段顺序为每个字段分配一个 bit，本次 update 中出现的字段对应 bit 做 `bor` 得到。

---

## 2. 核心限制与注意事项 (Critical Limitations)

为了保证代码的安全性和性能，本工具在设计上存在以下明确的限制，使用时请务必遵守。

### ⚠️ 1. `dirtyFlag` 必须有整数默认值

* **规则**：Record 定义中 `dirtyFlag` 必须指定默认值，且为整数。
* **错误示例**：`-record(bad, {id, dirtyFlag}).` (默认值为 `undefined`)
* **后果**：运行时执行 `undefined bor Mask` 会抛出 **`badarith`** 错误导致进程崩溃。

### ⚠️ 2. 不支持通配符 `_` 的脏标记捕获

* **规则**：使用 `_ = Val` 批量重置字段时，**不会**触发被重置字段的脏标记更新。
* **示例**：`P1 = P0#player{_ = 0, id = 5}.`
* **结果**：只有 `id` 会被标记脏，其他被重置为 0 的字段**不会**被标记。
* **原因**：语法转换器在编译期无法推断 `_` 具体覆盖了哪些字段。

### ⚠️ 3. 不支持 `setelement` 动态更新

* **规则**：使用 `setelement/3` 更新 Record 字段时，**不会**触发脏标记。
* **原因**：`setelement` 是运行时函数调用，Parse Transform 只能处理编译期的 `#rec{}` 语法结构。

### ⚠️ 4. 仅处理“更新”，不处理“创建”

* **规则**：只有 `Var#rec{...}` (Update) 会被注入，`#rec{...}` (Create) **不会**被注入。
* **示例**：`P = #player{hp = 100}.` -> `dirtyFlag` 仍为默认值 0。
* **原因**：Erlang 中“创建”和“模式匹配”的 AST 结构完全一致。强制注入会导致模式匹配（如 `f(#player{hp=1}) -> ...`）逻辑被破坏。
* **建议**：如果创建时需要标记，请手动指定：`#player{hp=100, dirtyFlag=4}`。

### ⚠️ 5. 临时变量名冲突风险（极低）

* **场景**：当更新的主体是复杂表达式时（如 `(get_player())#player{hp=1}`），工具会生成临时变量以防止表达式重复执行。
* **机制**：生成的变量名为 `__PtDirtyT` + `Counter`（如 `__PtDirtyT9966`）。
* **风险**：除非用户故意定义了同名变量，否则冲突概率几乎为零。

### ⚠️ 6. 编译期 Atom 消耗

* **场景**：上述临时变量名是通过 `list_to_atom/1` 生成的。
* **影响**：
    * **正常编译**：无影响。编译结束后，编译器进程结束，Atom 内存释放。
    * **动态编译**：如果你的系统在**运行时**（Runtime）频繁调用编译器模块动态生成并加载大量不同的代码，可能会有 Atom
      累积风险。对于常规的 CI/CD 构建或热更流程，此影响可忽略。

### ⚠️ 7. 故障降级策略

* **机制**：如果在转换过程中发生未捕获的异常（Crash），工具会捕获错误并**降级返回原始代码**。
* **后果**：编译能通过，但**脏标记功能会静默失效**。
* **监控**：请务必关注编译日志中是否有以 `Warning: ePtDirty ParseTransform Failed` 开头的警告。

---

## 3. 支持的场景 (Supported Scenarios)

以下复杂场景均经过测试，可正确处理：

### ✅ 1. 复杂表达式更新 (推荐加括号)

插件会自动生成临时变量，确保左值表达式只执行一次。

```erlang
(get_player(Id))#player{hp = 0}.
%% 等效于:
%% T = get_player(Id),
%% T#player{hp=0, dirtyFlag = ...}
```

### ✅ 2. 嵌套结构更新

支持在 `fun`、`case`、`receive`、`try...catch` 以及列表推导（List Comprehension）中进行更新。

```erlang
[P#player{hp = Max} || P <- AllPlayers].
```

### ✅ 3. 宏 (Macros)

支持包含在宏定义中的 Record 更新。Parse Transform 运行在宏展开之后。

```erlang
-define(HEAL(P), P#player{hp = 100}).
f(P) -> ?HEAL(P).
```

### ✅ 4. 手动覆盖

如果用户代码中手动指定了 `dirtyFlag`，插件会**尊重用户意图**，跳过自动注入。

```erlang
P#player{hp = 100, dirtyFlag = 0}. %% dirtyFlag 最终结果为 0
```

---

## 4. 字段 Bit 分配规则 (Technical Details)

位掩码（Bitmask）的计算完全依赖于 **Record 字段的定义顺序**。

* **起始位**：从 `2` 开始（即 `1 bsl 1`）。通常保留 Bit 1 用于特殊用途或避免混淆。
* **算法**：每遇到一个字段（包括 `dirtyFlag` 自身），位移一次 `Bit = Bit bsl 1`。

**示例：**

```erlang
-record(player, {
   id,          %% Bit: 2
   name,        %% Bit: 4
   level,       %% Bit: 8
   dirtyFlag,   %% Bit: 16 (自身通常不标记，但占位)
   gold         %% Bit: 32
}).
```

> **🚨 警告**：
> **调整 Record 字段的顺序会直接改变 Bit 映射值！**
> 如果你的系统将 dirtyFlag 用于持久化存储或跨版本协议同步，调整字段顺序可能导致数据含义错乱。请务必小心。

---

## 5. 最佳实践 (Best Practices)

1. **数据库回写策略**：
   在持久化时，检查 `dirtyFlag`。
   ```erlang
   %% 伪代码：只保存 hp 和 gold
   NeedSave = (P#player.dirtyFlag band (4 bor 32)) > 0.
   ```

2. **标记重置**：
   数据保存或同步完成后，务必将内存中的 `dirtyFlag` 重置。
   ```erlang
   P_Clean = P_Dirty#player{dirtyFlag = 0}.
   ```

3. **字段数量控制**：
   虽然 Erlang 的 Integer 支持任意大小，但作为位掩码使用时，建议 Record 字段数量控制在合理范围（例如 1000
   以内），以保持高效的位运算性能和代码可读性。
