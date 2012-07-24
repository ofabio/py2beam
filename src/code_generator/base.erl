-module(base).
-export([object___getattribute__/3, object___setattr__/4,
         instance___new__/2, instance___call__/3, instance___print__/2,
         int___new__/2, int___add__/3, int___gt__/3, int___repr__/2, int_attr__doc__/1, int___print__/2,
         function___new__/5, function___call__/4, function___repr__/2, 
         instancemethod___new__/5, instancemethod___print__/2,
         str___new__/2, str___add__/3, str___repr__/2, str___print__/2,
         list___new__/2, list_value/2, 
         class___new__/4, class___print__/2,
         range/3]).

object___getattribute__(M, Obj, Attribute) ->
    Res = common:get_attribute(M, Obj, Attribute),
    {_, State} = orddict:fetch(Obj, M),
    if 
        is_list(Res) ->
            % methodwrapper o wrapperdescriptor a seconda che l'istanza sia
            % rispettivamente (instanza, int, str, etc.), oppure (classe, type)
            FakeC_M = erlang:list_to_atom(Res ++ "_attr" ++ Attribute),
            C_M = erlang:list_to_atom(Res ++ "_" ++ Attribute),
            try erlang:apply(base, FakeC_M, [M])
            catch
                error:_ ->
                    List = base:module_info(exports),
                    case lists:keymember(C_M, 1, List) of
                        true ->
                            case common:is_builtin_or_intance(orddict:fetch("__type__", State)) of
                                true -> methodwrapper___new__(M, C_M, Obj);
                                false -> wrapperdescriptor___new__(M, C_M)
                            end;
                        false -> erlang:error("AttributeError: '" ++ Res ++ "' object has no attribute '" ++ Attribute ++ "'")
                    end
            end;
        is_integer(Res) ->
            % è stato trovato l'attributo definito dall'utente: se non si tratta di una funzione (p.e. intero) oppure se
            % è una funzione e l'oggetto in cui è stato trovato è una classe, allora ritorna l'oggetto così com'è.
            % se è una funzione ed è stato trovato in un'istanza allora costruisci e ritorna un oggetto bound.
            AttributeObj = common:read_memory(M, Res),
            try orddict:fetch("func_name", AttributeObj),
                % instance method (bound oppure unbound)
                case orddict:fetch("__type__", State) of
                    "class" -> {M, Res};
                    "instance" -> instancemethodBound___new__(M, Res, Obj)
                end
            catch
                error:_ ->
                    {M, Res}
            end
    end.
    
object___setattr__(M, Obj, Attribute, ObjVal) ->
    M2 = common:set_object_attribute(M, Obj, Attribute, ObjVal),
    {M2}.

% ----- instance -----

instance___new__(M, ClassObj) ->
    A = orddict:new(),
    B = orddict:store("__type__", "instance", A),
    C = orddict:store("__context__", orddict:new(), B),
    % eredità il beauty_name dalla classe (es. 'Pippo')
    ClassState = common:read_memory(M, ClassObj),
    BeautyName = orddict:fetch("beauty_name", ClassState),
    D = orddict:store("beauty_class_name", BeautyName, C),    
    State = orddict:store("__class__", ClassObj, D),
    common:to_memory(M, State).

% instance___init__() ->
%     ok.

instance___call__(_, _, _) ->
    erlang:error("TypeError: object is not callable").
    
instance___print__(Memory, Self) ->
    % <__main__.Pippo object at 0x104eb83d0>
    SelfState = common:read_memory(Memory, Self),
    BeautyName = orddict:fetch("beauty_class_name", SelfState),
    "<__main__." ++ BeautyName ++ " object at " ++ integer_to_list(Self) ++ ">".

% ----- int -----

int___new__(Memory, N) ->
    A = orddict:new(),
    B = orddict:store("__type__", "int", A),
    State = orddict:store("__value__", N, B),
    common:to_memory(Memory, State).

int___add__(Memory, Self, Other) ->
    SelfState = common:read_memory(Memory, Self),
    OtherState = common:read_memory(Memory, Other),
    TypeOther = orddict:fetch("__type__", OtherState),
    case TypeOther of
        "int" ->
            A = orddict:fetch("__value__", SelfState),
            B = orddict:fetch("__value__", OtherState),
            int___new__(Memory, A + B);
        _ ->
            erlang:error("TypeError: unsupported operand type(s) for +: 'int' and '" ++
                TypeOther ++ "'")
    end.

int_attr__doc__(M) ->
    str___new__(M, "int(x[, base]) -> integer
Convert a string or number to an integer, if possible.  A floating point
argument will be truncated towards zero (this does not include a string
representation of a floating point number!)  When converting a string, use
the optional base.  It is an error to supply a base when converting a
non-string.  If base is zero, the proper base is guessed based on the
string content.  If the argument is outside the integer range a
long object will be returned instead.").

int___gt__(Memory, Self, Other) ->
    SelfState = common:read_memory(Memory, Self),
    OtherState = common:read_memory(Memory, Other),
    TypeOther = orddict:fetch("__type__", OtherState),
    case TypeOther of
        "int" ->
            A = orddict:fetch("__value__", SelfState),
            B = orddict:fetch("__value__", OtherState),
            A > B;
        _ ->
            erlang:error("TypeError: unsupported operand type(s) for >: 'int' and '" ++
                TypeOther ++ "'")
    end.

int___repr__(Memory, Self) ->
    SelfState = common:read_memory(Memory, Self),
    Val = orddict:fetch("__value__", SelfState),
    str___new__(Memory, erlang:integer_to_list(Val)).

int___print__(Memory, Self) ->
    SelfState = common:read_memory(Memory, Self),
    erlang:integer_to_list(orddict:fetch("__value__", SelfState)).

% ----- str -----

str___new__(Memory, N) ->
    A = orddict:new(),
    B = orddict:store("__type__", "str", A),
    State = orddict:store("__value__", N, B),
    common:to_memory(Memory, State).

str___add__(Memory, Self, Other) ->
    SelfState = common:read_memory(Memory, Self),
    OtherState = common:read_memory(Memory, Other),
    TypeOther = orddict:fetch("__type__", OtherState),
    case TypeOther of
        "str" ->
            A = orddict:fetch("__value__", SelfState),
            B = orddict:fetch("__value__", OtherState),
            int___new__(Memory, A ++ B);
        _ ->
            erlang:error("TypeError: unsupported operand type(s) for +: 'str' and '" ++
                TypeOther ++ "'")
    end.

str___repr__(Memory, Self) ->
    SelfState = common:read_memory(Memory, Self),
    Val = orddict:fetch("__value__", SelfState),
    str___new__(Memory, Val).
    
str___print__(Memory, Self) ->
    SelfState = common:read_memory(Memory, Self),
    orddict:fetch("__value__", SelfState).

% ----- function -----

function___new__(Memory, FuncName,  Deep, Arity, BeautyName) ->
    A = orddict:new(),
    B = orddict:store("__type__", "function", A),
    C = orddict:store("func_name", FuncName, B),
    D = orddict:store("deep", Deep, C),
    E = orddict:store("arity", Arity, D),
    State = orddict:store("beauty_name", BeautyName, E),
    common:to_memory(Memory, State).

function___call__(Memory, Context, ModuleName, Args) ->
    [Target | P] = Args,
    TargetState = common:read_memory(Memory, Target),
    FuncName = orddict:fetch("func_name", TargetState),
    Deep = orddict:fetch("deep", TargetState),
    % calcola il contesto da passare in base alla profondità
    C = lists:reverse(Context),
    C2 = lists:sublist(C, Deep),
    C3 = lists:reverse(C2),
    Parameters = [Memory, C3 | [P]],
    %io:format("~p~n~p~n~p~n~n", [Memory, C3, Parameters]),
    erlang:apply(ModuleName, FuncName, Parameters).

function___repr__(Memory, Self) ->
    SelfState = common:read_memory(Memory, Self),
    Type = orddict:fetch("__type__", SelfState),
    Fn = orddict:fetch("func_name", SelfState),
    io:format("<bound ~p ~p at ~p>~n", [Type, Fn, Self]).

% ----- instancemethod -----
instancemethod___new__(Memory, FuncName, Deep, Arity, BeautyName) ->
    A = orddict:new(),
    B = orddict:store("__type__", "instancemethod", A),
    C = orddict:store("func_name", FuncName, B),
    D = orddict:store("deep", Deep, C),
    E = orddict:store("arity", Arity, D),
    State = orddict:store("beauty_name", BeautyName, E),
    common:to_memory(Memory, State).

instancemethodBound___new__(Memory, UnboundObj, Self) ->
    UnboundState = common:read_memory(Memory, UnboundObj),
    A = orddict:new(),
    B = orddict:store("__type__", "instancemethod", A),
    C = orddict:store("__self__", Self, B),
    D = orddict:store("func_name", orddict:fetch("func_name", UnboundState), C),
    E = orddict:store("deep", orddict:fetch("deep", UnboundState), D),
    F = orddict:store("class", orddict:fetch("class", UnboundState), E),
    G = orddict:store("arity", orddict:fetch("arity", UnboundState), F),
    H = orddict:store("beauty_name", orddict:fetch("beauty_name", UnboundState), G),
    State = orddict:store("beauty_instance_name", instance___print__(Memory, Self), H),
    common:to_memory(Memory, State).

instancemethod___print__(Memory, Self) ->
    % <unbound method Pippo.hello>
    % <bound method Pippo.hello of <__main__.Pippo object at 0x10bcea3d0>>
    SelfState = common:read_memory(Memory, Self),
    ClassObj = orddict:fetch("class", SelfState),
    ClassState = common:read_memory(Memory, ClassObj),
    BeautyClassName = orddict:fetch("beauty_name", ClassState),
    BeautyName = orddict:fetch("beauty_name", SelfState),
    case orddict:is_key("__self__", SelfState) of
        true ->
            % è bound
            BeautyInstanceName = orddict:fetch("beauty_instance_name", SelfState),
            "<bound method " ++ BeautyClassName ++ "." ++ BeautyName ++ " of " ++ BeautyInstanceName ++ ">";
        false ->
            % è unbound
            "<unbound method " ++ BeautyClassName ++ "." ++ BeautyName ++ ">"
    end.
    

% ----- method-wrapper -----
methodwrapper___new__(Memory, FuncName, Self) ->
    A = orddict:new(),
    B = orddict:store("__type__", "methodwrapper", A),
    C = orddict:store("func_name", FuncName, B),
    State = orddict:store("__self__", Self, C),
    common:to_memory(Memory, State).

% ----- wrapper_descriptor -----
wrapperdescriptor___new__(Memory, FuncName) ->
    A = orddict:new(),
    B = orddict:store("__type__", "wrapperdescriptor", A),
    State = orddict:store("__name__", FuncName, B),
    common:to_memory(Memory, State).

% ----- class -----
class___new__(Memory, ClassName, ClassContext, BeautyName) ->
    A = orddict:new(),
    B = orddict:store("__type__", "class", A),
    C = orddict:store("__class__", "object", B),
    D = orddict:store("class_name", ClassName, C),
    E = orddict:store("beauty_name", BeautyName, D),
    State = orddict:store("__context__", ClassContext, E),
    common:to_memory(Memory, State).

class___print__(Memory, Self) ->
    SelfState = common:read_memory(Memory, Self),
    BeautyName = orddict:fetch("beauty_name", SelfState),
    % <class '__main__.Pippo'>
    "<class '__main__." ++ BeautyName ++ "'>".
    % <__main__.Pippo object at 0x1075b8390> ----instance
    % io:format("<__main__." ++ BeautyName ++ " object at >", []).
    
% ----- list -----
list___new__(Memory, L) ->
    A = orddict:new(),
    B = orddict:store("__type__", "list", A),
    State = orddict:store("__value__", L, B),
    common:to_memory(Memory, State).

%list___getitem__(Memory, Self, N) ->
%   SelfState = common:read_memory(Memory, Self),

%list___getslice__(Memory, Self, Low, High) ->
%   SelfState = common:read_memory(Memory, Self),

list_value(Memory, Self) ->
    SelfState = common:read_memory(Memory, Self),
    orddict:fetch("__value__", SelfState).

% ----- builtins -----
range(Memory, Low, High) ->
    LowState = common:read_memory(Memory, Low),
    LowValue = orddict:fetch("__value__", LowState),
    HighState = common:read_memory(Memory, High),
    HighValue = orddict:fetch("__value__", HighState) - 1,
    List = lists:seq(LowValue, HighValue),
    {M, IntList} = build_ints(Memory, List, []),
    RIntList = lists:reverse(IntList),
    list___new__(M, RIntList).

build_ints(M, [], IntList) ->
    {M, IntList};
build_ints(M, [I|List], IntList) ->
    {M2, NewInt} = int___new__(M, I),
    build_ints(M2, List, [NewInt|IntList]).