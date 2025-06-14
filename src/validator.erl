-module(validator).

-export([is_empty/1, non_empty/1, get_messages/1, to_html/1]).


-spec is_empty(string()) -> boolean().
is_empty(Value) ->
	V = string:trim(Value),
	V =:= "".

-spec non_empty(string()) -> list(tuple()).
non_empty(Values) -> non_empty(Values, []).

non_empty([], Result) -> Result;
non_empty([H|T], Result) ->
	
	{Value, Msg} = H,
	Res = case is_empty(Value) of
		true ->[{error, Msg}];
		false -> []
	end,
	non_empty(T, Res ++ Result).


-spec get_messages(list(tuple())) -> list(string()).
get_messages(List) ->
	lists:map(fun({error, Msg}) -> Msg end, List).

-spec to_html(list(tuple())) -> list(string()).
to_html(List) ->	
	Items = lists:map(fun(Msg) -> "<li>"++Msg++"</li>" end, get_messages(List)),
	"<ul>" ++ string:join(Items, "") ++ "</ul>".