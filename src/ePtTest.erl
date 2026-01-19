-module(ePtTest).
-compile({parse_transform, ePtDirty}).
-include_lib("eunit/include/eunit.hrl").
-export([test/0]).

test() -> eunit:test(?MODULE).

%% ===================================================================
%% Record Definitions
%% ===================================================================

%% Record 1: Controlled record (from ptdirty_sample.erl)
-record(player, {
    id = 0,            %% bit=2
    name = <<"n">>,    %% bit=4
    level = 1,         %% bit=8
    hp = 10,           %% bit=16
    dirtyFlag = 0,     %% bit=32
    misc = undefined   %% bit=64
}).

%% Record 2: Another controlled record (renamed to player2)
-record(player2, {
    id :: integer(),           %% bit=2
    hp :: integer(),           %% bit=4
    mp :: integer(),           %% bit=8
    name :: string(),          %% bit=16
    dirtyFlag = 0 :: integer() %% bit=32
}).

%% Record 3: Uncontrolled record (no dirtyFlag)
-record(item, {
    id,
    count
}).

%% ===================================================================
%% Macros
%% ===================================================================
-define(BIT_ID, 2).
-define(BIT_NAME, 4).
-define(BIT_LEVEL, 8).
-define(BIT_HP, 16).
-define(BIT_MISC, 64).

%% ===================================================================
%% Helpers
%% ===================================================================

mk() ->
    #player{}.

reset_counter() ->
    put(ptdirty_counter, 0),
    ok.

get_counter() ->
    case get(ptdirty_counter) of
        undefined -> 0;
        V -> V
    end.

get_player() ->
    put(ptdirty_counter, get_counter() + 1),
    mk().

upd_var_name() ->
    P0 = mk(),
    P0#player{name = <<"a">>}.

upd_var_multi() ->
    P0 = mk(),
    P0#player{name = <<"a">>, level = 2}.

upd_chain() ->
    P0 = mk(),
    P1 = P0#player{name = <<"a">>},
    P1#player{hp = 99}.

upd_manual_dirty() ->
    P0 = mk(),
    P0#player{name = <<"x">>, dirtyFlag = 7}.

upd_only_dirty() ->
    P0 = mk(),
    P0#player{dirtyFlag = 5}.

upd_hp(P) ->
    P#player{hp = P#player.hp + 1}.

upd_expr_once() ->
    reset_counter(),
    P = (get_player())#player{name = <<"a">>},
    {P, get_counter()}.

upd_expr_multi_once() ->
    reset_counter(),
    P = (get_player())#player{name = <<"a">>, hp = 20},
    {P, get_counter()}.

upd_nested_expr_once() ->
    reset_counter(),
    P = (
     begin
         put(ptdirty_counter, get_counter() + 1),
         mk()
     end
    )#player{level = 99},
    {P, get_counter()}.

upd_update_as_arg_once() ->
    reset_counter(),
    _ = some_fun((get_player())#player{hp = 1}),
    get_counter().

some_fun(_) -> ok.

list_comp_inc_hp(List) ->
    [P#player{hp = P#player.hp + 1} || P <- List].

map_inc_level(List) ->
    lists:map(fun(P) -> P#player{level = P#player.level + 1} end, List).

anon_inc_level(P) ->
    F = fun(X) -> X#player{level = X#player.level + 1} end,
    F(P).

pattern_name(#player{name = N}) -> N.

%% ===================================================================
%% Tests
%% ===================================================================

upd_var_single_field_sets_dirty_test() ->
    P = upd_var_name(),
    ?assertEqual(<<"a">>, P#player.name),
    ?assertEqual(?BIT_NAME, P#player.dirtyFlag).

upd_var_multi_fields_sets_or_mask_test() ->
    P = upd_var_multi(),
    ?assertEqual(<<"a">>, P#player.name),
    ?assertEqual(2, P#player.level),
    ?assertEqual(?BIT_NAME bor ?BIT_LEVEL, P#player.dirtyFlag).

upd_chain_updates_accumulate_dirty_test() ->
    P = upd_chain(),
    ?assertEqual((?BIT_NAME bor ?BIT_HP), P#player.dirtyFlag).

manual_dirtyflag_skips_injection_test() ->
    P = upd_manual_dirty(),
    ?assertEqual(<<"x">>, P#player.name),
    ?assertEqual(7, P#player.dirtyFlag).

only_dirtyflag_update_skips_injection_test() ->
    P = upd_only_dirty(),
    ?assertEqual(5, P#player.dirtyFlag).

or_with_existing_dirtyflag_test() ->
    P0 = mk(),
    P1 = P0#player{dirtyFlag = 3},
    P2 = upd_hp(P1),
    ?assertEqual(19, P2#player.dirtyFlag).

expr_update_evaluated_once_test() ->
    {P, Cnt} = upd_expr_once(),
    ?assertEqual(1, Cnt),
    ?assertEqual(?BIT_NAME, P#player.dirtyFlag).

expr_multi_update_evaluated_once_test() ->
    {P, Cnt} = upd_expr_multi_once(),
    ?assertEqual(1, Cnt),
    ?assertEqual((?BIT_NAME bor ?BIT_HP), P#player.dirtyFlag).

nested_expr_update_evaluated_once_test() ->
    {P, Cnt} = upd_nested_expr_once(),
    ?assertEqual(1, Cnt),
    ?assertEqual(?BIT_LEVEL, P#player.dirtyFlag).

update_used_as_function_argument_evaluated_once_test() ->
    Cnt = upd_update_as_arg_once(),
    ?assertEqual(1, Cnt).

list_comprehension_update_test() ->
    P0 = mk(),
    P1 = P0#player{dirtyFlag = 1},
    [A, B] = list_comp_inc_hp([P0, P1]),
    ?assertEqual(?BIT_HP, A#player.dirtyFlag),
    ?assertEqual(1 bor ?BIT_HP, B#player.dirtyFlag).

lists_map_fun_update_test() ->
    P0 = mk(),
    [P1] = map_inc_level([P0]),
    ?assertEqual(2, P1#player.level),
    ?assertEqual(?BIT_LEVEL, P1#player.dirtyFlag).

anonymous_fun_update_test() ->
    P0 = mk(),
    P1 = anon_inc_level(P0),
    ?assertEqual(2, P1#player.level),
    ?assertEqual(?BIT_LEVEL, P1#player.dirtyFlag).

patterns_not_modified_by_transform_test() ->
    P0 = mk(),
    P = P0#player{dirtyFlag = 123},
    ?assertEqual(<<"n">>, pattern_name(P)).

%% New Tests

uncontrolled_record_test() ->
    I0 = #item{id = 1, count = 10},
    I1 = I0#item{count = 20},
    ?assertEqual(20, I1#item.count).

identity_update_test() ->
    P0 = mk(),
    P1 = P0#player{},
    ?assertEqual(P0, P1),
    ?assertEqual(0, P1#player.dirtyFlag).

manual_dirty_reset_test() ->
    P0 = mk(),
    %% Update name (bit 4), but set dirtyFlag to 0 manually
    %% Logic: if dirtyFlag is explicitly set, automatic injection is skipped.
    P1 = P0#player{name = <<"b">>, dirtyFlag = 0},
    ?assertEqual(<<"b">>, P1#player.name),
    ?assertEqual(0, P1#player.dirtyFlag).

%% ===================================================================
%% Tests using player2
%% ===================================================================

basic_update_test() ->
    P0 = #player2{id = 101, hp = 100, mp = 100, dirtyFlag = 0},
    %% Update hp (bit: 4)
    P1 = P0#player2{hp = 90},
    ?assertEqual(90, P1#player2.hp),
    ?assertEqual(4, P1#player2.dirtyFlag),
    %% Update mp (bit: 8) -> 4 bor 8 = 12
    P2 = P1#player2{mp = 50},
    ?assertEqual(12, P2#player2.dirtyFlag).

multi_field_update_test() ->
    P0 = #player2{id = 101, hp = 100, mp = 100, dirtyFlag = 0},
    %% id(2) + name(16) -> 18
    P1 = P0#player2{id = 102, name = "Hero"},
    ?assertEqual(18, P1#player2.dirtyFlag).

creation_safety_test() ->
    P = #player2{hp = 100},
    ?assertEqual(0, P#player2.dirtyFlag).

pattern_safety_test() ->
    P = #player2{hp = 0, dirtyFlag = 0},
    Result = case P of
                 #player2{hp = 0} -> dead;
                 _ -> alive
             end,
    ?assertEqual(dead, Result).

manual_override_test() ->
    P0 = #player2{hp = 100, dirtyFlag = 255},
    P1 = P0#player2{hp = 50, dirtyFlag = 0},
    ?assertEqual(0, P1#player2.dirtyFlag).

complex_expr_test() ->
    put(eval_count, 0),
    GetPlayer = fun() ->
        C = get(eval_count),
        put(eval_count, C + 1),
        #player2{hp = 100}
                end,
    P = (GetPlayer())#player2{hp = 50},
    ?assertEqual(1, get(eval_count)),
    ?assertEqual(4, P#player2.dirtyFlag).

%% ===================================================================
%% Advanced Pattern Matching Tests
%% ===================================================================

%% Helper for pattern matching in function head
match_in_head(#player{name = <<"target">>}) -> matched;
match_in_head(_) -> not_matched.

advanced_pattern_match_test() ->
    P0 = mk(),
    P_Target = P0#player{name = <<"target">>},
    
    %% 1. Match in Function Head
    ?assertEqual(matched, match_in_head(P_Target)),
    ?assertEqual(not_matched, match_in_head(P0)),

    %% 2. Match in Case Expression
    Res = case P_Target of
              #player{name = <<"target">>, level = 1} -> ok;
              _ -> fail
          end,
    ?assertEqual(ok, Res),

    %% 3. Match in Fun Head
    F = fun(#player{hp = 10}) -> hp_10;
           (_) -> other
        end,
    ?assertEqual(hp_10, F(P0)),

    %% 4. Verify no dirtyFlag check was injected (implicit)
    %% If injection happened, it would likely be syntactically invalid or logic would fail
    %% e.g., if it tried to match dirtyFlag against an expression.
    
    %% Explicitly matching a record that HAS dirty flags set, but we ignore them in pattern
    P_Dirty = P0#player{dirtyFlag = 999},
    Res2 = case P_Dirty of
               #player{id = 0} -> id_0; %% Should match regardless of dirtyFlag
               _ -> fail
           end,
    ?assertEqual(id_0, Res2).

