%% @author akseluhr
%% @doc @todo Add description to worker.


-module(worker).
-export([start/6, stop/1, peers/2, loop/6, jitter/1]).


%worker will wait for a while and then send a message to one of its peers (other workers?)
%Prepared to receive messages
%

start(Name, Logger, Seed, Sleep, Jitter, Entries) -> %delete entries
	spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter, Entries) end). %delete entries

stop(Worker) ->
	Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter, Entries) -> %delete entries, change init to /5, change start to /5
	random:seed(Seed, Seed, Seed),
	receive
		{peers, Peers} ->
			loop(Name, Log, Peers, Sleep, Jitter, vect:zero(Entries)); 
		stop ->
			ok
	end.

peers(Wrk, Peers) ->
	Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, Timestamp)->
	Wait = random:uniform(Sleep),
	receive
		{msg, Time, Msg} ->
			TimeMax = vect:merge(Timestamp, Time), %time:merge(Timestamp, Time)
			TimeIncr = vect:inc(Name, TimeMax), %time:inc(Timestamp, TimeMax)
			Log ! {log, Name, TimeIncr, {received, Msg}},
			loop(Name, Log, Peers, Sleep, Jitter, TimeIncr);
		stop ->
			ok;
		Error ->
			Log ! {log, Name, time, {error, Error}}
		after Wait ->
			Selected = select(Peers),
		%	Time = na,
			Time = vect:inc(Name, Timestamp), %time:inc(Name, Timestamp)
			Message = {hello, random:uniform(100)},
			Selected ! {msg, Time, Message},
			jitter(Jitter),
			Log ! {log, Name, Time, {sending, Message}},
			loop(Name, Log, Peers, Sleep, Jitter, Time)
	end.

select(Peers) ->
	lists:nth(random:uniform(length(Peers)), Peers).

jitter(0)-> ok;
	jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).