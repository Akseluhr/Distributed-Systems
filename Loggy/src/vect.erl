%% @author akseluhr
%% @doc @todo Add description to vect.


-module(vect).

%% ====================================================================
%% API functions
%% ====================================================================
-export([zero/1, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

% , inc/2, merge/2, leq/2, clock/1, update/3, safe/2


% zero vector time stamp
% remember: process = {Name, Time} -> {Aksel, 2}
% lamport clock, making it to vector by adding 0
zero(Nodes) ->
	lists:map((fun(Node) -> {Node, 0}end), Nodes).

% Time = vector of timestamps for a process.
% entry/process A: [{john, 1}, {benim, 7}, {alice, 2}]
% entry/process B: [{john, 1}, {aksel, 2}, {alice, 3}]
% entry/process C: [{john, 1}, {abdullah, 0}, {vasi, 5}]

% The vector keeps track of events occured in the system.
% We find the node name, e.g. john
% once found, we increment the Time-entry by one
% if we can't find the node, we simply add the node to time as a new entry.
inc(Name, Time) ->   
	case lists:keyfind(Name, 1, Time) of
		{Node,Value} -> 
			lists:keyreplace(Name, 1, Time, {Name, Value + 1});
		false ->
			[{Name, 0} | Time]
end.

% if we pass an empty list in merge, we simply return the Time vector. This is the base case.
% if not, we  want to empty the "time" list in order to get a new "updated" Time vector
% This is done by adding elements {Name, Tj}, from Time to our other Time vector. 
% if case the node of Name, Tj is found we add it and call the merge function again, as well as deleating the entry from Time-vector.
% if not case the node is not present in our other time vector, we simply add it to our new Time vector 
merge([], Time) ->
	Time;
merge([{Name, Ti}|Rest], Time) ->
	case lists:keyfind(Name, 1, Time) of
		% {Name, Tj} is the tuple found in lists:keyfind
		{Name, Tj} ->
			%passing the tail to the merge function, deleting the tuple and traversing the whole list
			[{Name, Tj} |merge(Rest, lists:keydelete(Name, 1, Time))];
		false ->
			[{Name, Ti} |merge(Rest, Time)]
	end.

% if we pass an empty list and anything, return false (base case)
leq([], _) ->
	true;
leq([{Name, Ti}|Rest],Time) ->
	case lists:keyfind(Name, 1, Time) of
		{Name, Tj} ->
				if Ti =< Tj ->
						leq(Rest, Time);
					true ->
						false
				end;
		% if node is not find, check if next node matches.
		false ->
			false
	end.

% Vector clock for a node
% Reflects that we have not seen anything yet, 
% but will be updated once a message is recieved from another node

clock(_) ->
	[].

% Remember Time is a vector, a set of entries for a certain node, [{john, 2},{paul, 3}]
% Clock is 
update(From, Time, Clock) ->
	 IsNewEvent = lists:keyfind(From, 1, Time),
	case lists:keyfind(From, 1, Clock) of
		{From, _} ->
			lists:keyreplace(From, 1, Clock, IsNewEvent);
		false ->[ IsNewEvent| Clock]
end.

safe(Time, Clock) ->
	leq(Time, Clock).