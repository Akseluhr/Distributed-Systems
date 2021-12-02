%% @author akseluhr
%% @doc @todo Add description to router.


-module(router).

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new() -> 
	[].

add(Name, Ref, Pid, Intf) ->
	lists:append([{Name, Ref, Pid}], Intf).

remove(Name, Intf) ->
	Entry = lists:keyfind(Name, 1, Intf),
	lists:delete(Entry, Intf).

lookup(Name, Intf) ->
	Tuple = lists:keyfind(Name, 1, Intf),
	if is_tuple(Tuple) ->
		   Pid = element(3, Tuple),
		   {ok, Pid};
		true ->
			notfound
	end.

ref(Name, Intf) ->
	Tuple = lists:keyfind(Name, 1, Intf),
	if is_tuple(Tuple) ->
		   Ref = element(2, Tuple),
		   {ok, Ref};
		true ->
			notfound
	end.

name(Ref, Intf) ->
Tuple = lists:keyfind(Ref, 2, Intf),
	if is_tuple(Tuple) ->
		   Name = element(1, Tuple),
		   {ok, Name};
		true ->
			notfound
	end.

list(Intf) ->
	case Intf of
		[] ->
			[];
		[{N, _, _} | T] -> [N | list(T)]
	end.

broadcast(Message, Intf) ->
	case Intf of 
		[] -> [];
		[{N, R, P} | T] -> [{N, R, P, Message} | broadcast(Message, T)]
	end.

