%% @author akseluhr
%% @doc @todo Add description to test.


-module(test).

%% ====================================================================
%% API functions
%% ====================================================================
-export([run/2]).
% cd("/Users/akseluhr/eclipse-workspace/Loggy/src").

% creating logger processes and four workers
% slepp value = how active the worker is sending messages, delay (time between recieving and sending message at a process) for sending messages
% jitter value = introduces random delay between the sending of a message and (informing the logger entry) the sending of a log entry 

run(Sleep, Jitter) ->
	Log = logger3:start([john, paul, ringo, george]),
	A = worker:start(john, Log, 13, Sleep, Jitter, [john, paul, ringo, george]),
	B = worker:start(paul, Log, 23, Sleep, Jitter, [john, paul, ringo, george]),
	C = worker:start(ringo, Log, 36, Sleep, Jitter, [john, paul, ringo, george]),
	D = worker:start(george, Log, 49, Sleep, Jitter, [john, paul, ringo, george]),
	worker:peers(A, [B, C, D]),
	worker:peers(B, [A, C, D]),
	worker:peers(C, [A, B, D]),
	worker:peers(D, [A, B, C]),
	timer:sleep(5000),logger3:stop(Log),
	worker:stop(A),
	worker:stop(B),
	worker:stop(C),
	worker:stop(D).
