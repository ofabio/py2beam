-module(base).
-export([object___getattribute__/3, object___new__/2, object___init__/0,
object___call__/5, int___new__/2, int___add__/3, int___gt__/3,
int___repr__/2, int_attr__doc__/1, function___new__/3, function___call__/4,
function___repr__/2, str___new__/2, str___add__/3, str___repr__/2,
list___new__/2, list___repr__/2, range/3, class___new__/3]).

object___getattribute__(M, Obj, Attribute) ->
    Res = common:get_attribute(M, Obj, Attribute),
    if is_list(Res) ->
        FakeC_M = erlang:list_to_atom(Res ++ "_attr" ++ Attribute),
        C_M = Res ++ "_" ++ Attribute,
        Res1 = try erlang:apply(base, FakeC_M, [M])
        catch
            error:_ ->
                List = erlang:apply(base, module_info, [exports]),
                case lists:keymember(erlang:list_to_atom(C_M), 1, List) of
                    true -> function___new__aux(M, "builtin_function", C_M, -1);
                    false -> erlang:error("AttributeError: '" ++ Res ++ "' object has no attribute '" ++ Attribute ++ "'")
                end
        end,
        tuple_to_list(Res1);
    is_integer(Res) ->
        [M, Res]
    end.

object___new__(M, Obj) ->
    A = orddict:new(),
    B = orddict:store("__type__", "instance", A),
    State = orddict:store("__class__", Obj, B),
    common:to_memory(M, State).

object___init__() ->
    ok.

object___call__(_, _, _, _, _) ->
    io:format("TypeError: ~p object is not callable~n", ["generic"]),
    ok.

int___new__(Memory, N) ->
    A = orddict:new(),
    B = orddict:store("__type__", "int", A),
    C = orddict:store("__class__", "int", B),
    State = orddict:store("__value__", N, C),
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
    orddict:fetch("__value__", SelfState).

% --------------

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
    orddict:fetch("__value__", SelfState).

function___new__(Memory, FuncName, Deep) ->
    function___new__aux(Memory, "function", FuncName, Deep).

function___new__aux(Memory, Type, FuncName,  Deep) ->
    A = orddict:new(),
    B = orddict:store("__type__", Type, A),
    C = orddict:store("func_name", FuncName, B),
    State = orddict:store("deep", Deep, C),
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


% ----- class -----
class___new__(Memory, ClassName, ClassContext) ->
    A = orddict:new(),
    B = orddict:store("__type__", "class", A),
    C = orddict:store("class_name", ClassName, B),
    State = orddict:store("__context__", ClassContext, C),
    common:to_memory(Memory, State).

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

list___repr__(Memory, Self) ->
    SelfState = common:read_memory(Memory, Self),
    orddict:fetch("__value__", SelfState).

% ----- builtins -----
range(Memory, Low, High) ->
    LowState = common:read_memory(Memory, Low),
    LowValue = orddict:fetch("__value__", LowState),
    HighState = common:read_memory(Memory, High),
    HighValue = orddict:fetch("__value__", HighState) - 1,
    R = lists:seq(LowValue, HighValue),
    list___new__(Memory, R).
