-module(base).
-export([object___getattribute__/3, int___new__/2, int___add__/3, int___gt__/3, int___repr__/2, function___new__/3, function___call__/4, str___new__/2, str___add__/3, str___repr__/2, list___new__/2, list___repr__/2, range/3, class___new__/3]).

object___getattribute__(M, Obj, Attribute) ->
    Res = common:get_attribute(M, Obj, Attribute),
    if is_list(Res) ->
        case "__gt__"
            "__lt__"
            "__doc__"
            
        [{8, [1, [{"__type__", "builtin_func"}, {"func_name", "int___gt__"}]]}]
    is_integer(Res) ->
        
    end.
    %io:format("~nRes BI:~p~n", [Res]),
    [M, Res].
    
object___new__(M, C, Obj) ->
    A = orddict:new(),
    B = orddict:store("__type__", "instance", A),
    State = orddict:store("__class__", Obj, B),
    common:to_memory(Memory, State).
    
object___init__() ->
    ok.

object___call__(M, C, ModuleName, Obj, Params) ->
	io:format("TypeError: ~p object is not callable~n", [])

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
	A = orddict:new(),
    B = orddict:store("__type__", "function", A),
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
%	SelfState = common:read_memory(Memory, Self),
    
%list___getslice__(Memory, Self, Low, High) ->
%	SelfState = common:read_memory(Memory, Self),

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
