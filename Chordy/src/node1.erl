%% @author akseluhr
%% @doc @todo Add description to node1.


-module(node1).

%% ====================================================================
%% API functions
%% ====================================================================
-export([node/3, stabilize/3, request/2, notify/3, schedule_stabilize/0, start/1, start/2, init/2]).
-define(Timeout, 10000).
-define(Stabilize, 1000).


start(Id) ->
	start(Id, nil).

start(Id, Peer) ->
	timer:start(),
	spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
	Predecessor = nil,
	% We suppose that the response from connect is our successor
	{ok, Successor} = connect(Id, Peer),
	% Then we check if it is true or not in the stabalizer
	schedule_stabilize(),
	% Afterwards we pass it to the node function.
	node(Id, Predecessor, Successor).


connect(Id, nil) ->
	{ok, {Id, self()}};
%Peer is joining
connect(_Id, Peer) ->
	Qref = make_ref(),
	% send key message to the node that we have been given and wait for reply
	Peer ! {key, Qref, self()},
	receive
		{Qref, Skey} ->
			Reply = {ok, {Skey, Peer}},
			Reply
		after ?Timeout ->
			io:format("Time out: no response~n",[])
end.
% Behaviour of a node
% Solves: Communicating our Key, 
% New Nodes joining the DHT
% Handles 
node(Id, Predecessor, Successor) ->
	receive
		% A peer needs to know our key
		{key, Qref, Peer} ->
			Peer ! {Qref, Id},
			node(Id, Predecessor, Successor);
		%	io:format("Key sent to another peer");
		% A new node informs us of its existence
		{notify, New} ->
			Pred = notify(New, Id, Predecessor),
			node(Id, Pred, Successor);
		%	io:format("A new node has joined");
		% A predecessor needs to know our predecessor. 
		{request, Peer} ->
			request(Peer, Predecessor),
			node(Id, Predecessor, Successor);
		% Our successor informs us about its predecessor. 
		{status, Pred} ->
			Succ = stabilize(Pred, Id, Successor),
			node(Id, Predecessor, Succ);
		% Message received form schedule_stabilizer
		stabilize ->
			stabilize(Successor),
			node(Id, Predecessor, Successor);
		%signal 
		probe ->
			create_probe(Id, Successor),
			node(Id, Predecessor, Successor);
		{probe, Id, Nodes, T} ->
			remove_probe(T, Nodes),
			node(Id, Predecessor, Successor);
		{probe, Ref, Nodes, T} ->
			forward_probe(Ref, T, Nodes, Id, Successor),
			node(Id, Predecessor, Successor)
		
	end.

% The periodic stabilize procedure will consist of a node sending a
% {request,self()}message to its successor and then expecting a
% {status, Pred}in return. 
% Id = "our"

% 
schedule_stabilize() ->
	timer:send_interval(?Stabilize, self(), stabilize).

% a node joins, and we need to stabalize the circuit
% ID = the node it is refering to - probably to any node. 


stabilize({_, Spid}) ->
	% sending a request to a successor node for joining, which in turn calls the stabilize function
	Spid ! {request, self()}.
% ID = current successor's ID
stabilize(Pred, Id, Successor) ->
	{Skey, Spid} = Successor,
	%looking for a key
% checking if any node in between Pred and successor
	case Pred of
		% Inform the node (Spid) about our (Id) existence Id = Key
		% If circuit is empty, Node can slide in
		nil ->
			Spid ! {status, {Id, self()}},
			Successor;
		% Pointing back to us = dont do anything
		{Id, _} ->
			Successor;
		% Pointing to itself = notify it (Pred) about our (Id) existance
		% we can only slide in once a pred and successor is next to eachother 
		% then we update the pred of the successor (send request) to update the predecessor to have us as a successor
		{Skey, _} ->
			Spid ! {notify, {Id, self()}},
			Successor;
		% Pointing to another node. Pointing to a new node. Xkey = key of the predecessor of our successor
		% updating if some one is entering
		{Xkey, Xpid} ->
			% Will Xkey = Skey ever happen?
			case key:between(Xkey, Id, Skey) of
				true ->
					% Adopt this node as our successor 
					% Run stabilization again
					stabilize(Pred, Id, {Xkey, Xpid});
				false ->
					% Adopt this node as our successor 
					% Run stabilization again
					Spid ! {notify, {Id, self()}},
					Successor

				end
	end.

% telling our successor to run stabalize
% we want to do this continiously - 
request(Peer, Predecessor) ->
	case Predecessor of
		nil ->
			Peer ! {status, nil};
		{Pkey, Ppid} ->	
			Peer ! {status, {Pkey, Ppid}}
	end.

% notify = telling pred that we are its successor

notify({Nkey, Npid}, Id, Predecessor) ->
	case Predecessor of
		nil ->
			% if no predecessor, we can use new node as predecessor
			{Nkey, Npid};
		{Pkey,  _} ->
			case key:between(Nkey, Pkey, Id) of
				true ->
						% if Nkey is in between, the new node can be our successor
						{Nkey, Npid};
				false ->
						% if not, we keep our existing predecessor
						Predecessor
				end
end.

create_probe(Id, Successor) ->
	Time = erlang:monotonic_time(),
	{_Skey, Spid} = Successor,
	Spid ! {probe, Id, [Id], Time}. 

remove_probe(T, Nodes) ->
	Time = erlang:monotonic_time(),
	New = Time - T,
	io:format("Time :  ~w, Nodes: ~w~n",[New,Nodes]).

forward_probe(Ref, T, Nodes, Id, Successor) ->
	{_Skey, Spid} = Successor,
	Spid ! {probe, Ref, Nodes++[Id], T}.