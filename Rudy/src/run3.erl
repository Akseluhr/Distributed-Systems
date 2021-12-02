%% @author akseluhr
%% @doc @todo Add description to run3.


-module(run3).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, stop/0]).

%Solution to run the server in seperate rlang process.

% this is useful since if u refresh the page once, the server terminates without this class.
%with this, the server will run until stop() is called
% cd("user/eclipse-workspace/rudy/src").

start(Port) ->
	register(rudy, spawn(fun() -> reply:init(Port)end)).

stop() ->
	exit(whereis(rudy), "time to die").