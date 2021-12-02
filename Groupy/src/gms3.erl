%% @author akseluhr
%% @doc @todo Add description to gms3.


-module(gms3).


%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, start/2]).

% starting process for first node in a group 
start(Id) ->
	Rnd = random:uniform(1000),
	Self = self(),
	{ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

% initializing the first node with an empty list of peers, telling the master it is the only node in the group
init(Id, Rnd, Master) ->
	random:seed(Rnd, Rnd, Rnd),
	leader(Id, Master, 0, [], [Master]).

% starting a node joining the group 

start(Id, Grp) ->
	Rnd = random:uniform(1000),
	Self = self(),
	{ok, spawn_link(fun()-> init(Id, Rnd, Grp, Self) end)}.

% sending a message, {join, Master, self()} to a node in the group for invitation
% delivered as a view message contating everything we know - init slave is a slave
init(Id, Rnd, Grp, Master) ->
	random:seed(Rnd, Rnd, Rnd),
	Self = self(),
	Grp ! {join, Master, Self},
	%handeling messages for the slaves
	receive
		{view, N, [Leader|Slaves], Group} ->
			% The new Leader, namely the slave in the first pos of the group,
			% Holds the last message that are being broadcasted to other nodes once elected.
			% we are finding this message and passing it to the slaves for broadcasting.
			Master ! {view, Group},
			erlang:monitor(process, Leader),
			%LastMessage = {view, N, [Leader|Slaves], Group},
			slave(Id, Master, Leader, N, {view, N, [Leader|Slaves], Group}, Slaves, Group) %n+1
	after timer:sleep(1000) ->
		Master ! {error, "no reply from leader"}
	end.

crash(Id) ->
	case random:uniform(100) of
		100 ->
			io:format("leader ~w: crash~n", [Id]),
			exit(no_luck);
		_ ->
			ok
	end.

%bcast(Id, Msg, Nodes) ->
bcast(Id, Msg, Nodes) ->
	lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

% ID = unique name
% Master = process identifier
% Slaves = ordered list of process identifiers of all slaves in the group
% Group: list of all app layer processes in the group 
leader(Id, Master, N, Slaves, Group) ->
	receive % måste tänka på att nya inkomna meddelanden och noder måste inkrement
		% message from its own master or from a peer node. {msg, Msg} is multicasted to all peers and Msg is sent to application layer
		{mcast, Msg} ->
			% sends message to each process in a list
			bcast(Id, {msg, N+1, Msg}, Slaves),
			Master ! Msg,
			leader(Id, Master, N+1, Slaves, Group);
		% message from peer or the master, a request from a node to join the group
		% message conatins both the process identifier , wrk, and the process identifier of its own group process
		{join, Wrk, Peer} ->
			% add slaves to the process
			Slaves2 = lists:append(Slaves, [Peer]),
			% add slaves amster process to the slaves
			Group2 = lists:append(Group, [Wrk]),
			
			bcast(Id, {view, N + 1, [self()|Slaves2], Group2}, Slaves2),
			% master is updated and has now a new application layer
			Master ! {view, Group2},
			% adds new node at the end of the list of peers
			% this because we want to add the new node to be the last one who has seen the msg sent out
			leader(Id, Master, N+1, Slaves2, Group2); %n+1
		stop ->
			ok
	end.

% slave is forwarding messages from its master to the leader and vice verse
% same state as the leader but keeps explicit track of the leader

% only forwarding messages and joining to the leader, and the leader will take care of it
% if we recieve a message we forward it to the own master process
% if new process added, and view changes, we inform the master
% every slave and leader have its own master process. 
slave(Id, Master, Leader, N, Last, Slaves, Group) ->
	receive
		% request from its master to multicast a message, message is forwarded to the leader
		% N (sequence number) is attached to the message being broadcasted  to other nodes 
		% last, the last message, is also sent in the broadcasted message 
		{mcast, Msg} ->
			Leader ! {mcast, Msg},
		    slave(Id, Master, Leader, N, Last, Slaves, Group);
		% a request from the master to allow a new node to join the group, the msg is forwarded to the leader
		{join, Wrk, Peer} ->
			Leader ! {join, Wrk, Peer},
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		% a multicasted message from the leader. Msg is sent to the master
		{msg, I, Msg} when I < N + 1-> 
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		{msg, I, Msg} when I == N + 1 -> 
			Master ! Msg,
			slave(Id, Master, Leader, I, {msg, I, Msg}, Slaves, Group);
		% multicasted view from the leader. Delivered to the master process 
		{view, I, _, _} when I < N + 1 -> 
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		
		{view, I, [Leader|Slaves2], Group2} when I == N + 1 ->
			Master ! {view, Group2},
			slave(Id, Master, Leader, I, {view,I, [Leader|Slaves2], Group2}, Slaves2, Group2); %n+1
		{'DOWN', _Ref, process, Leader, _Reason} ->
			election(Id, Master, Slaves, N, Last, Group);
		stop ->
			ok
	end.

% "DOWN" message is recieved for all nodes, and then we start the new election of leader 
election(Id, Master, Slaves, N, Last, [_|Group]) ->
	Self = self(),
	case Slaves of
		[Self|Rest] ->
			bcast(Id, Last, Rest),
			bcast(Id, {view, N, Slaves, Group}, Rest),
			Master ! {view, Group},
			leader(Id, Master, N, Slaves, Group);
		[Leader|Rest] ->
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, N, Last, Rest, Group)
	end.
		
		
