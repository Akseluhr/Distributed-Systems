%% @author akseluhr
%% @doc @todo Add description to time.


-module(time).

%% ====================================================================
%% API functions
%% ====================================================================
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).


% returns 0 
zero() ->
	0. 

inc(Name, T) ->
	T + 1.

merge(Ti, Tj) ->
	if Ti < Tj ->
		 Tj;
		true ->
			Ti
	end.

leq(Ti, Tj) ->
	if Ti =< Tj ->
		   true; 
	   true -> 	
		   false
	end.

%history of processes with timestamps 
clock(Nodes) ->  % {Name, timestamp}, {bob, 0}, {bob, 1}	Node = name
	lists:map((fun(Node) -> {Node, zero()}end), Nodes).

% replace timestamp (1) in the clock for a specific Node. {node, time } = tupleList
update(Node, Time, Clock) ->
	lists:keyreplace(Node, 1, Clock, {Node, Time}).



%we are only printing messages less than timestamps in history (clock)
% Time is the current message timestamp which have been retruned now
	
safe(Time, Clock) ->
	SortedClock = lists:keysort(2, Clock),
	[{Node, SmallestTimestamp} | Tail] = SortedClock,
	leq(Time, SmallestTimestamp).
	
	
	   