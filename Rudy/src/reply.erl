%% @author akseluhr
%% @doc @todo Add description to reply.


-module(reply).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1, handler/1, request/1, reply/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================

%initiating the server, takes a port number 
% opens a listening socket
% passes the socket to handler (nästa funktion) 
% opens a socket

init(Port) ->
	Opt = [list, {active, false}, {reuseaddr, true}],
	case gen_tcp:listen(Port, Opt) of
		{ok, Listen} ->
			io:format("rudy: listen: ~w~n", [Listen]),
			handler(Listen),
			gen_tcp:close(Listen),
			ok;
		{error, Error} ->
		error
	end.
%will listen to the socket for an incoming connection
% if client is connected, it will pass the connection to request/1 (nedan)
% when request is handled, connection is closed

handler(Listen) ->
	case gen_tcp:accept(Listen) of
		{ok, Client} ->
			% io:format("rudy: client connected. "),
			request(Client),
			handler(Listen); %works for multiple invominh requests (())clients
		{error, Error} ->
			error
	end.

% reads the request from the client connection and parse it
% it will parse request string, using http parser from last module
% then passing to reply (nedan)

request(Client) ->
	Recv = gen_tcp:recv(Client, 0), % receiving client request via the address field on the browsee 
	case Recv of
		{ok, Str} ->
			Request = http:parse_request(Str), %fundera på denna local host 8080
			Response = reply(Request),
			gen_tcp:send(Client, Response);
		{error, Error} ->
		io:format("rudy: error: ~w~n", [Error])
	end,
	gen_tcp:close(Client).

% here we decide what to reply, how to turn the reply into a well formed
% HTTP reply. 

reply({{get, URI, _}, _, _}) ->
	%timer:sleep(40),
	http:ok("hej").
