-module(temp).
-export([go/0]).


go() ->
    {B,C} = cane:ciao(),
    io:format("~p~n", B),
    io:format("~p~n", C).
