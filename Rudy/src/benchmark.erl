-module(benchmark).
-export([bench/2]).


% start = erlang runtime
% then we run the application
% finish = executed once the message have been replied by the server
% then simply finish - start to calculate the time it took for the reply to come back
bench(Host, Port) ->
	Start = erlang:system_time(micro_seconds),
	run(1000, Host, Port),
	Finish = erlang:system_time(micro_seconds),
	Finish - Start.


% N == 0  then request the server

run(N, Host, Port) ->
	if
		N == 0 ->
			ok;
		true ->
			request(Host, Port),
			run(N-1, Host, Port)
	end.

request(Host, Port) ->
	Opt = [list, {active, false}, {reuseaddr, true}],
	{ok, Server} = gen_tcp:connect(Host, Port, Opt),
	gen_tcp:send(Server, http:get("foo")),
	Recv = gen_tcp:recv(Server, 0),
	case Recv of
		{ok, _} ->
			ok;
		{error, Error} ->
			io:format("test: error: ~w~n", [Error])
	end,
	gen_tcp:close(Server).