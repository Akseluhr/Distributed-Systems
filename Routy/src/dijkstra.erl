%% @author akseluhr
%% @doc @todo Add description to dijkstra.


-module(dijkstra).

%% ====================================================================
%% API functions
%% ====================================================================
-export([entry/2, replace/4, update/4, iterate/3, table/2, route/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================


% each entry name of node, length of path abd gateway {berlin , 2, paris}
% ska sorteras baserat på längden av path (2)
 
%reutrns lenght of shortest path to the node or 0 if not found
% sorted = sorted list
entry(Node, Sorted)->
	Record = lists:keyfind(Node, 1, Sorted),
	if is_tuple(Record) ->
		element(2, Record);
		true ->
			0
	end.
	

% replaces the entry for node in sorted with new entry having a new length N and gateway
% Resulting list should ofc be sorted.
replace(Node, N, Gateway, Sorted) ->
	Record = lists:keyfind(Node, 1, Sorted),
	if is_tuple(Record) ->
		TupleList1 = lists:keyreplace(Node, 1, Sorted, {Node, N, Gateway}),
	   	lists:keysort(2, TupleList1);
	    true ->
			io:format("replace: entry not found \n")
	end.
			

%update the list sorted given the info that node can be reached in N hops using gateway
% no entry -> no new entry added. Only if better (shorter) exists, we replace existing entry
									  
update(Node, N, Gateway, Sorted) ->
	Length = entry(Node, Sorted),
		if Length == N ->
			lists:keyfind(Node, 1, Sorted);
			true ->
				if Length > N ->
					replace(Node, N, Gateway, Sorted);
				  	true ->
						[]
				end
	end.

%returns routing table given Sorted list of nodes, a map and a table 
%iterate(Sorted, Map, Table) ->
%	case Sorted of
%				[] -> 
%					Table;
%				[{N,L,G} | T] -> 
%					if L == "inf" -> 
%							io:format("iterate: node length is infinite, iteration terminated"),
%					   		Table;
%					true ->
%						 map:reachable(N, Map)
%						 %lists:append(update(N, L, G, Sorted), Table), iterate(T,Map,Table)
%					end
%end.						
				
					
% base case -> [] -> ok;
% base case -> infinite -> ok
% case -> take first entry (HEAD), find nodes in the map rachable from this entyr, update entry in sorted list
% The entry found in the sorted list is added to routing table


%returns a table given Sorted list of nodes, a map and a table 
iterate(Sorted, Map) ->
	Table = [],
	iterate(Sorted, Map, Table).

iterate([], _, Table) -> 
	Table;

iterate([{_, inf,_}|_], _, Table ) ->
	Table;

iterate([{Node, N, Gateway}| Sorted], Map, Table) ->
	Links = map:reachable(Node, Map),
	F = fun(Link, Sorted) ->
				update(Link, N+1, Gateway, Sorted)
	end,
	UpdatedSorted = lists:foldl(F, Sorted, Links),
	UpdatedTable = [{Node, Gateway}| Table],
	iterate(UpdatedSorted, Map, UpdatedTable).


% computes routing table given gateways and map 
% Router Interfaces = Gateways
table(Gateways, Map) ->
	Nodes = map:all_nodes(Map),
	%combines nodes and gateways
	Union = lists:usort(Nodes ++ Gateways),
	% all nodes have an entry with an infinite lenght
	Node_F = fun(Node) ->
			{Node, inf, unknown}
	end,
	SortedList1 = lists:map(Node_F, Union),
	Gateway_F = fun(Gateway, Sorted) ->
	%Here we say that for each gateway, we add an entry 
		update(Gateway, 1, Gateway, Sorted)
	end,
	SortedList2 = lists:foldl(Gateway_F, SortedList1, Gateways),
	iterate(SortedList2, Map).

% search the routing table and return the gateway suitable to route messages to a node
% if a gateway is found, we return ok, Gateway
route(Node, Table) ->
	case lists:keyfind(Node, 1, Table) of
		{_, Gateway} ->
			{ok, Gateway};
		false ->
			notfound
	end.


