%% @author akseluhr
%% @doc @todo Add description to gms1.


-module(gms1).

%% ====================================================================
%% API functions
%% ====================================================================
-export([leader/4, slave/5, start/1, start/2, init/2, init/3]).


% ID = unique name
% Master = process identifier
% Slaves = ordered list of process identifiers of all slaves in the group
% Group: list of all app layer processes in the group 
leader(Id, Master, Slaves, Group) ->
	receive
		% message from its own master or from a peer node. {msg, Msg} is multicasted to all peers and Msg is sent to application layer
		{mcast, Msg} ->
			% sends message to each process in a list
			bcast(Id, {msg, Msg}, Slaves),
			Master ! Msg,
			leader(Id, Master, Slaves, Group);
		% message from peer or the master, a request from a node to join the group
		% message conatins both the orocess identifier , wrk, and the process identifier of its own group process
		{join, Wrk, Peer} ->
			Slaves2 = lists:append(Slaves, [Peer]),
			Group2 = lists:append(Group, [Wrk]),
			bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
			Master ! {view, Group2},
			% adds new node at the end of the list of peers
			% this because we want to add the new node to be the last one who has seen the msg sent out
			leader(Id, Master, Slaves2, Group2);
		stop ->
			ok
	end.

% slave is forwarding messages from its master to the leader and vice verse
% same state as the leader but keeps explicit track of the leader

slave(Id, Master, Leader, Slaves, Group) ->
	receive
		% request from its master to multicast a message, message is forwarded to the leader
		{mcast, Msg} ->
			Leader ! {mcast, Msg},
		    slave(Id, Master, Leader, Slaves, Group);
		% a request from the master to allow a new node to join the group, the msg is forwarded to the leader
		{join, Wrk, Peer} ->
			Leader ! {join, Wrk, Peer},
			slave(Id, Master, Leader, Slaves, Group);
		% a multicasted message from the leader. Msg is sent to the master
		{msg, Msg} ->Master ! Msg,
					 slave(Id, Master, Leader, Slaves, Group);
		% multicasted view from the leader. Delivered to the master process 
		{view, [Leader|Slaves2], Group2} ->
			Master ! {view, Group2},
			slave(Id, Master, Leader, Slaves2, Group2);
		stop ->
			ok
	end.
% starting process for first node in a group 
start(Id) ->
	Self = self(),
	{ok, spawn_link(fun()-> init(Id, Self) end)}.

% initializing the first node with an empty list of peers, telling the master it is the only node in the group
init(Id, Master) ->
	leader(Id, Master, [], [Master]).

% starting a node joining the group 

start(Id, Grp) ->
	Self = self(),
	{ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.

% sending a message, {join, Master, self()} to a node in the group for invitation
% delivered as a view message contating everything we now - init slave is a slave
init(Id, Grp, Master) ->
	Self = self(),
	Grp ! {join, Master, Self},
	receive
		{view, [Leader|Slaves], Group} ->
			Master ! {view, Group},
			slave(Id, Master, Leader, Slaves, Group)
	end.

%bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2)
%bcast(Id, Message, Slaves) ->
	%case Slaves of
		
	crash(Id) ->
	case random:uniform(100) of
		100 ->
			io:format("leader ~w: crash~n", [Id]),
			exit(no_luck);
		_ ->
			ok
	end.

bcast(Id, Msg, Nodes) ->
	lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).
