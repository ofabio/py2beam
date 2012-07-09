-module(common).
-export([init_memory/0, assign/4, test/0, to_memory/2, get_from_context/2, get_from_context/3, call_method/4, read_memory/2, destroy_locals/2, dot/5, get_attribute/3, bind_method/3, call/4]).

assign(Memory, [Local|ContextRest], Var, Obj) when not is_tuple(Local) ->
    {M, L} = assign_aux(Memory, Local, Var, Obj),
    {M, [L|ContextRest]};
assign(Memory, ClassContext, Var, Obj) ->
    %io:format("Memory: ~p~n", [Memory]),
    assign_aux(Memory, ClassContext, Var, Obj).

assign_aux(Memory, Local, Var, Obj) ->
    %io:format("Memory: ~p~n", [Memory]),
    %io:format("Context: ~p~n", [Local]),
    %io:format("Var: ~p~n", [Var]),
    %io:format("Obj: ~p~n", [Obj]),
    M = case orddict:find(Var, Local) of
        {ok, OldObj} ->
            decrease_ref_del(Memory, OldObj);
        error -> Memory
    end,

    M2 = increase_ref(M, Obj),

    % salva l'oggetto nella variabile
    L = orddict:store(Var, Obj, Local),
    {M2, L}.

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
    Type = orddict:fetch("__type__", State),
    M = if Type == "class"; Type == "instance" ->
            ClassContext = orddict:fetch("__context__", State),
            Values = [orddict:fetch(X, ClassContext) || X <- orddict:fetch_keys(ClassContext)],
            decrease_ref_del_aux(Values, Memory);
        true ->
            Memory
    end,

    if RefCounter > 1 ->
           orddict:store(Obj, {RefCounter-1, State}, M);
       RefCounter =:= 1 ->
           orddict:erase(Obj, M)
    end.

decrease_ref_del_aux([], Memory) ->
    Memory;
decrease_ref_del_aux([Value|Rest], Memory) ->
    M = decrease_ref_del(Memory, Value),
    decrease_ref_del_aux(Rest, M).

decrease_ref(Memory, Obj) ->
    {RefCounter, State} = orddict:fetch(Obj, Memory),
    orddict:store(Obj, {RefCounter-1, State}, Memory).

%destroy_class_locals() ->
%    destroy_locals_aux(orddict:fetch_keys(Local), Local, Memory).

% destroy_locals(Memory, [Local|_]) ->
%     io:format("Dizionario: ~p", [Local]),
%   destroy_locals_aux(orddict:fetch_keys(Local), Local, Memory);
% destroy_locals(Memory, Local) ->
%   destroy_locals_aux(orddict:fetch_keys(Local), Local, Memory).

% destroy_locals(Memory, Local) when is_list(Local) ->
%     %io:format("Dizionario: ~p", [Local]),
%     destroy_locals_aux(orddict:fetch_keys(Local), Local, Memory);
destroy_locals(Memory, [Local|_]) ->
    % io:format("Dictionary 1: ~p", [Local]),
    destroy_locals_aux(orddict:fetch_keys(Local), Local, Memory).

destroy_locals_aux([], _, Memory) ->
    Memory;
destroy_locals_aux([Key|Rest], Local, Memory) ->
    Obj = orddict:fetch(Key, Local),
    {_, State} = orddict:fetch(Obj, Memory),
    Type = orddict:fetch("__type__", State),
    M = if Type == "class"; Type == "instance" ->
            ClassContext = orddict:fetch("__context__", State),
            destroy_locals(Memory, [ClassContext]);
        true ->
            Memory
    end,
    M2 = decrease_ref(M, Obj),
    destroy_locals_aux(Rest, Local, M2).

read_memory(Memory, Obj) ->
    {_, State} = orddict:fetch(Obj, Memory),
    State.

call_method(Memory, Obj, Method, Params) ->
    {_, State} = orddict:fetch(Obj, Memory),
    Class = orddict:fetch("__type__", State),
    C_M = erlang:list_to_atom(Class ++ "_" ++ Method),
    erlang:apply(base, C_M, [Memory, Obj | Params]).

% get_from_class_context(ClassContext, Context, Var) ->
%     get_from_context([ClassContext|Context], Var).

get_from_context(ClassContext, Context, Var) ->
    get_from_context([ClassContext|Context], Var).

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


get_attribute(_, Obj, _) when is_list(Obj) ->
    Obj;
get_attribute(Memory, Obj, Name) ->
    {_, State} = orddict:fetch(Obj, Memory),
    % io:format("Obj:~p~nMemory:~p~n", [Obj, Memory]),
    try orddict:fetch(Name, orddict:fetch("__context__", State))
    catch
        error:_ ->
            Sup = try orddict:fetch("__class__", State)
                  catch error:_ -> "object"
                  end,
            get_attribute(Memory, Sup, Name)
    end.

bind_method(M, FuncRef, InstRef) ->
    A = orddict:new(),
    B = orddict:store("__type__", "bound_method", A),
    C = orddict:store("__func__", FuncRef, B),
    State = orddict:store("__inst__", InstRef, C),
    common:to_memory(M, State).

call(M, C, ModuleName, Parameters) ->
    [Obj | Args] = Parameters,
    ObjState = common:read_memory(M, Obj),
    Type = orddict:fetch("__type__", ObjState),
    case Type of
        "class" ->
            base:object___new__(M, Obj);
            % R = base:object___new__(M, Obj),
            % io:format("R: ~p~n", [R]),
        % R;
        "instance" ->
            [M1, Ref] = base:object___getattribute__(M, Obj, "__call__"),
            FuncState = common:read_memory(M1, Ref),
            FuncType = orddict:fetch("__type__", FuncState),
            Params = [Ref, Obj | Args],
            if FuncType == "builtin_function" ->
                    user_defined_call(M1, C, base, Params);
            true ->
                    user_defined_call(M1, C, ModuleName, Params)
                end;
        "bound_method" ->
            FuncRef = orddict:fetch("__func__", ObjState),
            InstRef = orddict:fetch("__inst__", ObjState),
            FuncState = common:read_memory(M, FuncRef),
            FuncType = orddict:fetch("__type__", FuncState),
            if FuncType == "builtin_function" ->
                    % user_defined_call(M, C, base, Params);
                    Params = [InstRef | Args],
                    builtin_call(M, FuncRef, Params);
                    % io:format("PARAMS: ~p~n", [R]),
                    % R;
            true ->
                    Params = [FuncRef, InstRef | Args],
                    user_defined_call(M, C, ModuleName, Params)
                end;
        "builtin_function" ->
            builtin_call(M, Obj, Args);
        _ ->
            Params = [Obj | Args],
        % io:format("PARAMS: ~p~n", [Params]),
            user_defined_call(M, C, ModuleName, Params)
    end.

dot(M, C, ModuleName, Obj, Attribute) ->
    Res = get_attribute(M, Obj, "__getattribute__"),
    if is_list(Res) ->
        C_M = erlang:list_to_atom("object" ++ "_" ++ "__getattribute__"),
        erlang:apply(base, C_M, [M, Obj, Attribute]);
    % io:format("GA: ~p~n", [R]),
        % R;
        % io:format("~nDot:~p~n", [R]),
        % base:object___getattribute__(M, C, )
        % chiama la built-in,
    is_integer(Res) ->
        {M2, AttrState} = base:str___new__(M, Attribute),
        % base:function___call__(M2, C, ModuleName, [Res, [Obj, AttrState]])
        user_defined_call(M2, C, ModuleName, [Res, Obj, AttrState])
    % io:format("~nResponse:~p~n", [Resp]),
    end.

builtin_call(Memory, Obj, Params) ->
    {_, State} = orddict:fetch(Obj, Memory),
    FuncName = orddict:fetch("func_name", State),
    erlang:apply(base, FuncName, [Memory | Params]).

user_defined_call(Memory, Context, ModuleName, Args) ->
    [Target | P] = Args,
    TargetState = common:read_memory(Memory, Target),
    % io:format("ARGS: ~p~n", [TargetState]),
    FuncType = orddict:fetch("__type__", TargetState),
    if FuncType == "bound_method" ->
            RealTarget = orddict:fetch("__func__", TargetState),
            TargetState1 = common:read_memory(Memory, RealTarget);
    true -> TargetState1 = TargetState
    end,
    FuncName = orddict:fetch("func_name", TargetState1),
    Deep = orddict:fetch("deep", TargetState1),
    % calcola il contesto da passare in base alla profondità
    C = lists:reverse(Context),
    if Deep >= 0 ->
        C2 = lists:sublist(C, Deep);
    true ->
    C2 = []
    end,
    C3 = lists:reverse(C2),
    Parameters = [Memory, C3, P],
    % Parameters = [Memory, C3 | [P]],
    % io:format("SONO QUI~nPAR: ~p~n", [Parameters]),
    erlang:apply(ModuleName, FuncName, Parameters).
    % io:format("FINE: ~p~n", [R]),
    % R.

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

