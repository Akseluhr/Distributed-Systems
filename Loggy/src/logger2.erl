%% @author akseluhr
%% @doc @todo Add description to logger.


-module(logger2).
-export([start/1, stop/1, loop/2, init/1, log/3, checkMessage/2]).

%The logger simply accepts events and prints them on the screen.  
%It will beprepared to receive time-stamps on the messages but 
%we will not do verymuch with them now.



start(Nodes) ->
	spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
	Logger ! stop.

%Put process (Node: worker, names) in clock
% open new queue, pass these to the loop
init(Nodes) ->
	Clock = time:clock(Nodes),	
	Queue = [],
	loop(Clock, Queue).

%The Logger is given a list of nodes that will send its messages


checkMessage(QueueSort, ClockUpdate) ->
	case QueueSort of
			[] ->	
				[];
			[{F, T, M} | Tail] -> 
						IsSafe = time:safe(T, ClockUpdate),
						if IsSafe ->
								log(F, T, M),
								%lists:delete({F, T, M}, QueueSort),
								checkMessage(Tail, ClockUpdate);
						true ->		
							loop(ClockUpdate, QueueSort)
						end
	end.
								

% messages with the same timestamp are considered concurent. 
% If we really want to track these down, we need process IDs
loop(Clock, Queue) ->
	receive{log, From, Time, Msg} ->
			   % Queue = 
				
			   % update the timestamp in the clock where node = name
				% updates the timestamp with the current time found in the incoming message
			   ClockUpdate = time:update(From, Time, Clock),
				% Inserts a message in a hold-back-queue with Process Name (from), timestamp, msg)
			   Queue1 = [{From, Time, Msg} | Queue], 
				% sorts the queue having the smalles timestamp first.
			   QueueSort = lists:keysort(2, Queue1),
				% checks if time (for example 1) is safe to print, compared to updated clock history
				%  where the first element in it is the smallest of the history
			   checkMessage(QueueSort, ClockUpdate);
			  % loop(ClockUpdate, HoldBackQueue);

		stop -> 
			ok
	end. 

log(From, Time, Msg) ->
	io:format("log: ~w ~w ~p~n", [Time, From, Msg]).