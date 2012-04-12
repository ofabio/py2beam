-module(common).
-export([init_memory/0, assign/4, test/0, to_memory/2, get_from_context/2, call_method/4, read_memory/2, destroy_locals/2]).

assign(Memory, Context, Var, Obj) ->
	%io:format("Memory: ~p~n", [Memory]),
	%io:format("Context: ~p~n", [Context]),
	%io:format("Var: ~p~n", [Var]),
	%io:format("Obj: ~p~n", [Obj]),
	
    [Local|ContextRest] = Context,
    case orddict:find(Var, Local) of
        {ok, OldObj} ->
            M = decrease_ref_del(Memory, OldObj);
        error -> M = Memory
        end,
    
    M2 = increase_ref(M, Obj),
    
    % salva l'oggetto nella variabile
    L = orddict:store(Var, Obj, Local),
    C = [L|ContextRest],
    {M2, C}.

init_memory() ->
    Memory = orddict:new(),
    orddict:store(next_key, 0, Memory).

to_memory(Memory, State) ->
    N = orddict:fetch(next_key, Memory),
    M2 = orddict:store(N, {0,State}, Memory),
    M3 = orddict:store(next_key, N+1, M2),
    {M3, N}.

increase_ref(Memory, Obj) ->
    {RefCounter, State} = orddict:fetch(Obj, Memory),
    orddict:store(Obj, {RefCounter+1, State}, Memory).

decrease_ref_del(Memory, Obj) ->
    {RefCounter, State} = orddict:fetch(Obj, Memory),
    if RefCounter > 1 ->
           orddict:store(Obj, {RefCounter-1, State}, Memory);
       RefCounter =:= 1 ->
           orddict:erase(Obj, Memory)
    end.

decrease_ref(Memory, Obj) ->
	{RefCounter, State} = orddict:fetch(Obj, Memory),
	orddict:store(Obj, {RefCounter-1, State}, Memory).

destroy_locals(Memory, [Local|_]) ->
	M = destroy_locals_aux(orddict:fetch_keys(Local), Local, Memory),
	%io:format("~p~n~n", [M]),
	M.
	
destroy_locals_aux([], _, Memory) ->
	Memory;
destroy_locals_aux([Key|Rest], Local, Memory) ->
	Obj = orddict:fetch(Key, Local),
	M = decrease_ref(Memory, Obj),
	destroy_locals_aux(Rest, Local, M).

read_memory(Memory, Obj) ->
	{_, State} = orddict:fetch(Obj, Memory),
	State.

call_method(Memory, Obj, Method, Params) ->
	{_, State} = orddict:fetch(Obj, Memory),
    Class = orddict:fetch("__class__", State),
    C_M = erlang:list_to_atom(Class ++ "_" ++ Method),
    erlang:apply(base, C_M, [Memory, Obj | Params]).

get_from_context(Context, Var) ->
    Union = dict_list_merge(Context),
    orddict:fetch(Var, Union).

dict_list_merge(L) ->
    dict_list_merge_aux(L, orddict:new()).

dict_list_merge_aux([], Dict) ->
    Dict;
dict_list_merge_aux([D|L], Dict) ->
    A = orddict:merge(fun get_first_of_three/3, D, Dict),
    dict_list_merge_aux(L, A).

get_first_of_three(_, Value1, _) -> Value1.


test() ->
    io:format("~p~n", ["test"]),
    M = init_memory(),
    C = [orddict:new()],

    {M2, Obj} = base:int___new__(M, 5),
    {M3, C2} = assign(M2, C, "pippo", Obj),
    io:format("~200p    ~200p~n", [M3, C2]),

    {M4, Obj2} = base:int___new__(M3, 7),
    {M5, C3} = assign(M4, C2, "ciccio", Obj2),
    io:format("~200p    ~200p~n", [M5, C3]),

    Pippo = get_from_context(C3, "ciccio"),
    io:format("~p~n", [Pippo]),

    ok.
    %Val = read(Memory, Context, Var),
    %io:format("~w~n", [Val]).
