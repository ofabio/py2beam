-module(common).
-export([init_memory/0, assign/4, test/0, to_memory/2, get_from_context/2, get_from_context/3, 
         print/2, read_memory/2, destroy_locals/2, dot/5, get_attribute/3, call/5, is_builtin_or_intance/1]).

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

% call_method(Memory, Obj, Method, Params) ->
%     {_, State} = orddict:fetch(Obj, Memory),
%     Class = orddict:fetch("__type__", State),
%     C_M = erlang:list_to_atom(Class ++ "_" ++ Method),
%     erlang:apply(base, C_M, [Memory, Obj | Params]).

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


% get_attribute(_, Obj, _) when is_list(Obj) ->
%     Obj;
get_attribute(Memory, Obj, Name) ->
    {_, State} = orddict:fetch(Obj, Memory),
    % io:format("Obj:~p~nMemory:~p~n", [Obj, Memory]),
    try orddict:fetch(Name, orddict:fetch("__context__", State))
    catch
        error:_ ->
            % non esiste l'attributo nel contesto!
            % se l'attributo __class__ non c'è oppure contiene una
            % stringa allora ritorna l'attributo __type__
            Type = orddict:fetch("__type__", State),
            try 
                Sup = orddict:fetch("__class__", State),
                if 
                    is_list(Sup) -> Type;
                    true -> get_attribute(Memory, Sup, Name)
                end
              catch error:_ -> 
                  Type
              end
                
            % Sup = try orddict:fetch("__class__", State)
            %       catch error:_ -> 
            %           Type
            %       end,
            % get_attribute(Memory, Sup, Name)
    end.

% bind_method(M, FuncRef, InstRef) ->
%     A = orddict:new(),
%     B = orddict:store("__type__", "bound_method", A),
%     C = orddict:store("__func__", FuncRef, B),
%     State = orddict:store("__inst__", InstRef, C),
%     common:to_memory(M, State).

call(M, C, ModuleName, Obj, Args) ->
    ObjState = common:read_memory(M, Obj),
    Type = orddict:fetch("__type__", ObjState),
    case Type of
        "class" ->
            base:object___new__(M, Obj);
        
        "instance" ->
            Res = common:get_attribute(M, Obj, "__call__"),
            if 
                is_list(Res) ->
                    ObjState = read_memory(M, Obj),
                    ClassObj = orddict:fetch("__class__", ObjState),
                    ClassState = read_memory(M, ClassObj),
                    ClassName = orddict:fetch("beauty_name", ClassState),
                    io:format("TypeError: '" ++ ClassName ++ "' object is not callable~n", []),
                    halt();
                is_integer(Res) ->
                    Params = [Res, Obj | Args],
                    user_defined_call(M, C, ModuleName, Params)
            end;
                
        "methodwrapper" ->
            FuncName = orddict:fetch("func_name", ObjState),
            Self = orddict:fetch("__self__", ObjState),
            % io:format("FuncName: ~p~n", [FuncName]),
            % io:format("Self: ~p~n", [Self]),
            % io:format("Argums: ~p~n", [[M, Self | Args]]),
            
            erlang:apply(base, FuncName, [M, Self | Args]);
            
        % "bound_method" ->
        %     FuncRef = orddict:fetch("__func__", ObjState),
        %     InstRef = orddict:fetch("__inst__", ObjState),
        %     FuncState = common:read_memory(M, FuncRef),
        %     FuncType = orddict:fetch("__type__", FuncState),
        %     if FuncType == "builtin_function" ->
        %             % user_defined_call(M, C, base, Params);
        %             Params = [InstRef | Args],
        %             builtin_call(M, FuncRef, Params);
        %             % io:format("PARAMS: ~p~n", [R]),
        %             % R;
        %     true ->
        %             Params = [FuncRef, InstRef | Args],
        %             user_defined_call(M, C, ModuleName, Params)
        %         end;
        % "builtin_function" ->
        %     builtin_call(M, Obj, Args);
        "function" ->
            Params = [Obj | Args],
        % io:format("PARAMS: ~p~n", [Params]),
            user_defined_call(M, C, ModuleName, Params)
    end.

dot(M, C, ModuleName, Obj, Attribute) ->
    Res = get_attribute(M, Obj, "__getattribute__"),
    if 
        is_list(Res) ->
            base:object___getattribute__(M, Obj, Attribute);
        is_integer(Res) ->
            {M2, Attribute} = base:str___new__(M, Attribute),
            user_defined_call(M2, C, ModuleName, [Res, Obj, Attribute])
    end.
    
print(M, Obj) ->
    Res = get_attribute(M, Obj, "__repr__"),
    if 
        is_list(Res) ->
            Print = erlang:apply(base, list_to_atom(Res ++ "___print__"), [M, Obj]),
            io:format("~p~n", [Print])
            
            % case with_value(Res) of
            %     true ->
            %         ObjState = common:read_memory(M, Obj),
            %         Val = orddict:fetch("__value__", ObjState),
            %         io:format("~p~n", [Val])
            % end
        % is_integer(Res) ->
        %     user_defined_call(M, C, ModuleName, [Res, Obj])
    end.
    
% with_value("int") ->
%     true;
% with_value("str") ->
%     true;
% with_value(_) ->
%     false.
    
is_builtin_or_intance(A) ->
    case is_builtin(A) of
        true ->
            true;
        false ->
            case is_instance(A) of
                true ->
                    true;
                false ->
                    false
            end
    end.

is_builtin("object") ->
    true;
is_builtin("int") ->
    true;
is_builtin("str") ->
    true;
is_builtin(_) ->
    false.

is_instance("instance") ->
    true;
is_instance(_) ->
    false.

% builtin_call(Memory, Obj, Params) ->
%     {_, State} = orddict:fetch(Obj, Memory),
%     FuncName = orddict:fetch("func_name", State),
%     erlang:apply(base, FuncName, [Memory | Params]).

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

