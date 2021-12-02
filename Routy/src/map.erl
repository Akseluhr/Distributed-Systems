%% @author akseluhr
%% @doc @todo Add description to map.


%Map = key value pair
-module(map).

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/0, update/3, reachable/2, all_nodes/1]).

% member / 2 finds exact value if it exists
%Returns empty list

new() ->
	[].

update(Node, Links, Map) ->
	Tuple = lists:keyfind(Node, 1, Map),
	if is_tuple(Tuple) ->
		lists:keyreplace(Node, 1, Map, {Node, Links});
	true ->
		[{Node, [Links]}]
	end.
	%maps:update(Node, Links, Map).
	
reachable(Node, Map) ->
	Tuple = lists:keyfind(Node, 1, Map),
	if is_tuple(Tuple) ->
			Tuple;
		true -> 
			[]
		end.

all_nodes(Map) ->
	case Map of
		[] -> 
			[];
		[{H, [K,V]} | T ] -> 
			[H, K, V | all_nodes(T)]
	end.
		
		
%this map only have one link





%lists:foldl(fun([H | T], [])->[H | all_nodes(T)] end, [] , Map).
		



%new()->
%	maps:new().

%experiment() ->
%	List = ["benim"],
	%lists:map(new(), List).

%Node = key, Links = value
% updates the key's value value in the map 
% m = #{berlin => [stockholm, gothenburg]}
% maps:update(berlin, [malmö, kiruna], m).
% > #{berlin, [malmö, kiruna]}
%update(Node, Links, Map) ->	
	%maps:update(Node, Links, Map).

% passing a router out of reach does not work yer
%reachable(Node, Map) ->
%maps:get(Node, Map).
	%Reachable = maps:get(Node,Map),
	%if
		%%is_list(Reachable) ->
		%	Reachable;
	%	true ->
	%		io:fwrite("reachable: router was out reahc"),
	%		[]
%	end.


	%case of maps:get(Node,Map),
	%	{ok, _}  ->
	%		io:format("it was reachable"),
	%		ok;
	%	{unreachable, _} ->
	%		io:format("reachable: node unreachable")
	%end.


%all_nodes(Map) ->
	%lists:append(Map).
	%lists:flatmap(Fun, Map).
	%case Map of
	%	[] -> [];
	%	[H | T] -> lists:append[H | all_nodes(T)].
	%end.
	
	

	