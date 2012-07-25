-module(builtins).
-export([range/3]).

range(Memory, Low, High) ->
    LowState = common:read_memory(Memory, Low),
    LowValue = orddict:fetch("__value__", LowState),
    HighState = common:read_memory(Memory, High),
    HighValue = orddict:fetch("__value__", HighState) - 1,
    List = lists:seq(LowValue, HighValue),
    {M, IntList} = build_ints(Memory, List, []),
    RIntList = lists:reverse(IntList),
    base:list___new__(M, RIntList).

build_ints(M, [], IntList) ->
    {M, IntList};
build_ints(M, [I|List], IntList) ->
    {M2, NewInt} = base:int___new__(M, I),
    build_ints(M2, List, [NewInt|IntList]).