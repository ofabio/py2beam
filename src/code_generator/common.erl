-module(common).
-export([init_memory/0, assign/4, test/0, to_memory/2, get_from_context/2, get_from_context/3, 
         print_standard_or_overwrited/2, read_memory/2, destroy_locals/2, dot/5, dot2/6,  
         get_attribute/3, call/5, is_base_or_intance/1, links_methods_to_class/2, 
         check_arity/2, set_object_attribute/4, call_keyword/6, throw_except/1, is_base/1]).

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

read_memory(M, Obj) ->
    {_, State} = orddict:fetch(Obj, M),
    State.
    
write_memory(M, Obj, State) ->
    {RC, _} = orddict:fetch(Obj, M),
    orddict:store(Obj, {RC, State}, M).
    
set_object_property(M, Obj, Key, Value) ->
    State = read_memory(M, Obj),
    NewState = orddict:store(Key, Value, State),
    write_memory(M, Obj, NewState).
    
set_object_attribute(M, Obj, Key, Value) ->
    State = read_memory(M, Obj),
    Context = orddict:fetch("__context__", State),
    NewContext = orddict:store(Key, Value, Context),
    set_object_property(M, Obj, "__context__", NewContext).

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
    A = orddict:merge(fun get_newest_dict/3, D, Dict),
    dict_list_merge_aux(L, A).

get_newest_dict(_, _, Value2) -> Value2.


get_attribute(Memory, Obj, Name) ->
    State = read_memory(Memory, Obj),
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
                    is_list(Sup) -> Sup;
                    is_integer(Sup) -> get_attribute(Memory, Sup, Name)
                end
            catch error:_ ->
                Type
            end
    end.

user_defined_call(Memory, Context, ModuleName, Target, P) ->
    TargetState = common:read_memory(Memory, Target),
    FuncName = orddict:fetch("func_name", TargetState),
    Deep = orddict:fetch("deep", TargetState),
    % calcola il contesto da passare in base alla profondità
    C = lists:reverse(Context),
    if 
        Deep >= 0 ->
            C2 = lists:sublist(C, Deep);
        true ->
            C2 = []
    end,
    C3 = lists:reverse(C2),
    Parameters = [Memory, C3, P],
    
    check_arity(TargetState, P),
    
    erlang:apply(ModuleName, FuncName, Parameters).
    
check_arity(TargetState, P) ->
    % fa controllo dei parametri passati alla funzione perchè deve sollevare un'eccezione anzichè andare in seg fault;
    Arity = orddict:fetch("arity", TargetState),
    BeautyName = orddict:fetch("beauty_name", TargetState),
    NParams = length(P),
    if
        NParams =:= Arity ->
            ok;
        true ->
            ArityRepr = if
                Arity =:= 0 ->
                    % TypeError: hello() takes no arguments (1 given)
                    "no arguments";
                Arity =:= 1 ->
                    % TypeError: hello() takes exactly 1 argument (2 given)
                    "exactly 1 argument";
                Arity > 1 ->
                    % TypeError: hello() takes exactly 2 arguments (1 given)
                    "exactly " ++ integer_to_list(Arity) ++ " arguments"
            end,
            throw_except("TypeError: " ++ BeautyName ++ "() takes " ++ ArityRepr ++ " (" ++ integer_to_list(NParams) ++ " given)~n")
    end.

call(M, C, ModuleName, Obj, Args) ->
    ObjState = common:read_memory(M, Obj),
    Type = orddict:fetch("__type__", ObjState),
    case Type of
        "class" ->
            base:instance___new__(M, Obj);
        
        "instance" ->
            Res = common:get_attribute(M, Obj, "__call__"),
            if 
                is_list(Res) ->
                    ObjState = read_memory(M, Obj),
                    ClassObj = orddict:fetch("__class__", ObjState),
                    ClassState = read_memory(M, ClassObj),
                    ClassName = orddict:fetch("beauty_name", ClassState),
                    throw_except("TypeError: '" ++ ClassName ++ "' object is not callable~n");
                is_integer(Res) ->
                    user_defined_call(M, C, ModuleName, Res, [Obj | Args])
            end;
                
        "methodwrapper" ->
            FuncName = orddict:fetch("func_name", ObjState),
            Self = orddict:fetch("__self__", ObjState),
            erlang:apply(base, FuncName, [M, Self | Args]);
            
        "instancemethod" ->
            case orddict:is_key("__self__", ObjState) of
                true ->
                    % è bound
                    Self = orddict:fetch("__self__", ObjState),
                    user_defined_call(M, C, ModuleName, Obj, [Self | Args]);
                false ->
                    % è unbound
                    check_first_arg_is_ok(M, ObjState, Args),
                    user_defined_call(M, C, ModuleName, Obj, Args)
            end;

        "function" ->
            user_defined_call(M, C, ModuleName, Obj, Args);
            
        "builtin_function_or_method" ->
            Name = orddict:fetch("__value__", ObjState),
            erlang:apply(builtins, erlang:list_to_atom(Name), [M | Args]);
        "int" ->
            throw_except("TypeError: 'int' object is not callable~n");
        "str" ->
            throw_except("TypeError: 'str' object is not callable~n")
    end.

check_first_arg_is_ok(M, ObjState, []) ->
    % solleva l'eccezione perchè bisogna passare almeno un'argomento
    ClassObj = orddict:fetch("class", ObjState),
    ClassState = common:read_memory(M, ClassObj),
    BeautyClassName = orddict:fetch("beauty_name", ClassState),

    BeautyName = orddict:fetch("beauty_name", ObjState),
    throw_except("TypeError: unbound method " ++ BeautyName ++ "() must be called with " ++ BeautyClassName ++ " instance as " ++ 
                "first argument (got nothing instead)~n");
check_first_arg_is_ok(M, ObjState, Args) ->
    % solleva l'eccezione se il tipo del primo argomento non coincide con la classe del metodo unbound.
    % il primo argomento di args dovrebbe essere un oggetto instanza della classe indicata
    % dentro il campo "class" di Obj.
    % esempio eccezione:
    % TypeError: unbound method hello() must be called with Pippo instance as first argument (got nothing instead)
    ClassObj = orddict:fetch("class", ObjState),
    [InstanceObj | _] = Args,
    InstanceState = common:read_memory(M, InstanceObj),
    InstanceClass = try
        orddict:fetch("__class__", InstanceState)
    catch
        error:_ ->
            error
    end,
    
    if 
        ClassObj =:= InstanceClass ->
            ok;
        true ->
            first_arg_not_ok(M, ObjState, InstanceState)
    end.
    
first_arg_not_ok(M, ObjState, InstanceState) ->
    ClassObj = orddict:fetch("class", ObjState),
    ClassState = common:read_memory(M, ClassObj),
    BeautyClassName = orddict:fetch("beauty_name", ClassState),
    InstanceType = get_beauty_type(M, InstanceState),
    BeautyName = orddict:fetch("beauty_name", ObjState),
    throw_except("TypeError: unbound method " ++ BeautyName ++ "() must be called with " ++ BeautyClassName ++ " instance as " ++ 
                "first argument (got " ++ InstanceType ++ " instance instead)~n").

    
links_methods_to_class(M, ClassObj) ->
    ClassState = common:read_memory(M, ClassObj),
    Context = orddict:fetch("__context__", ClassState),
    iterate_on_attributes(M, Context, ClassObj).
    
iterate_on_attributes(M, [], _) ->
    M;
iterate_on_attributes(M, [{_, AttributeObj} | Rest], ClassObj) ->
    AttributeState = read_memory(M, AttributeObj),
    M2 = case orddict:fetch("__type__", AttributeState) of
        "instancemethod" ->
            set_object_property(M, AttributeObj, "class", ClassObj);
        _ ->
            M
    end,
        
    iterate_on_attributes(M2, Rest, ClassObj).
    
get_beauty_type(M, State) ->
    Type = orddict:fetch("__type__", State),
    case Type of
        "class" ->
            "type";
        "instance" ->
            ClassObj = orddict:fetch("__class__", State),
            ClassState = common:read_memory(M, ClassObj),
            orddict:fetch("beauty_name", ClassState);
        _ ->
            Type
    end.
    

dot(M, C, ModuleName, Obj, Attribute) ->
    ObjState = common:read_memory(M, Obj),
    Type = orddict:fetch("__type__", ObjState),
    case Type of
        "instance" ->
            ClassObj = orddict:fetch("__class__", ObjState),
            Res = get_attribute(M, ClassObj, "__getattribute__"),
            if 
                is_list(Res) ->
                    base:object___getattribute__(M, Obj, Attribute);
                is_integer(Res) ->
                    {M2, Attribute2} = base:str___new__(M, Attribute),
                    user_defined_call(M2, C, ModuleName, Res, [Obj, Attribute2])
            end;
        _ ->
            base:object___getattribute__(M, Obj, Attribute)
    end.
    
dot2(M, C, ModuleName, Obj, Attribute, ObjVal) ->
    % io:format("~p ~p ~p ~p ~p ~p ~n", [M, C, ModuleName, Obj, Attribute, ObjVal]),
    ObjState = common:read_memory(M, Obj),
    Type = orddict:fetch("__type__", ObjState),
    case Type of
        "instance" ->
            ClassObj = orddict:fetch("__class__", ObjState),
            Res = get_attribute(M, ClassObj, "__setattr__"),
            if 
                is_list(Res) ->
                    base:object___setattr__(M, Obj, Attribute, ObjVal);
                is_integer(Res) ->
                    {M2, Attribute2} = base:str___new__(M, Attribute),
                    user_defined_call(M2, C, ModuleName, Res, [Obj, Attribute2, ObjVal])
            end;
        _ ->
            base:object___setattr__(M, Obj, Attribute, ObjVal)
    end.

get_primitive_type(Memory, Obj) ->
    State = read_memory(Memory, Obj),
    Type = orddict:fetch("__type__", State),
    try
        Sup = orddict:fetch("__class__", State),
        if 
            is_list(Sup) -> Type;
            is_integer(Sup) -> get_primitive_type(Memory, Sup)
        end
    catch error:_ -> 
        Type
    end.
    
call_keyword(M, C, ModuleName, Keyword, Obj, Args) ->
    ObjState = common:read_memory(M, Obj),
    Type = orddict:fetch("__type__", ObjState),
    case Type of
        "instance" ->
            ClassObj = orddict:fetch("__class__", ObjState),
            Res = get_attribute(M, ClassObj, Keyword),
            if 
                is_list(Res) ->
                    erlang:apply(base, erlang:list_to_atom(Res ++ "_" ++ Keyword), [M, Obj | Args]);
                is_integer(Res) ->
                    user_defined_call(M, C, ModuleName, Res, [M, Obj | Args])
            end;
        _ ->
            Res = get_primitive_type(M, Obj),
            erlang:apply(base, erlang:list_to_atom(Res ++ "_" ++ Keyword), [M, Obj | Args])
    end.
    
print_standard_or_overwrited(M, Obj) ->
    % guarda tra qualche superclasse in cerca di __repr__,
    % se non lo trova chiama il metodo __print__ del tipo
    % dell'oggetto che ha fatto la chiamata passando l'oggetto stesso.
    ObjState = common:read_memory(M, Obj),
    Type = orddict:fetch("__type__", ObjState),
    case Type of
        "instance" ->
            ClassObj = orddict:fetch("__class__", ObjState),
            Res = get_attribute(M, ClassObj, "__repr__"),
            if 
                is_integer(Res) ->
                    % chiamare il metodo __repr__ trovato e stampare
                    % il valore di ritorno
                    % user_defined_call(M, C, ModuleName, [Res, Obj])
                    todo;
                is_list(Res) ->
                    print_standard(Type, M, Obj)
            end;
        _ ->
            print_standard(Type, M, Obj)
    end.
    
print_standard(Type, M, Obj) ->
    Print = erlang:apply(base, list_to_atom(Type ++ "___print__"), [M, Obj]),
    io:format(Print ++ "~n", []).
    


throw_except(Msg) ->
    io:format(Msg, []),
    % erlang:exit(0).
    halt().
                
    
is_base_or_intance(A) ->
    case is_base(A) of
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

is_base("object") ->
    true;
is_base("int") ->
    true;
is_base("str") ->
    true;
is_base(_) ->
    false.

is_instance("instance") ->
    true;
is_instance(_) ->
    false.


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

