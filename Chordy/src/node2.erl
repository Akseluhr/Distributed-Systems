-module(node2).
-export([start/1,start/2]).
-define(Stabilize, 1000).
-define(Timeout, 10000).

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
  	% Afterwards we pass it to the node function where it waits for an incoming message
  node(Id, Predecessor, Successor).

connect(Id, nil) ->
  {ok, {Id, self()}};
%Peer is joining the circle!
connect(_Id, Peer) ->
  Qref = make_ref(),
  	% send key message to the node that we have been given and wait for reply
  Peer ! {key, Qref, self()},
  receive
      % we got a response with our successor's key
    {Qref, Skey} -> 
      {ok, {Skey, Peer}}
  after ?Timeout ->
    io:format("Time out: no response~n",[])
end.

% Behaviour of a node
% Waitning for different incoming messages
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

    stabilize ->
	    stabilize(Successor),
	    node(Id, Predecessor, Successor);
    % adding signal to the ring!
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor);
    {probe, Id, Nodes, T} -> % Id, [Id]
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor)

    
end.
schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).
% Asking we are asking our successor if we are still his predecessor
stabilize({_, Spid}) ->
    Spid ! {request, self()}.
% we can only slide in once a pred and successor is next to eachother 
% then we update the pred of the successor (send request) to update the predecessor to have us as a successor
stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,
  % checking if any node in between Pred and successor
  case Pred of
      % If circuit is empty, Node can slide in
    nil -> 
      Spid ! {notify, {Id, self()}}, 
	    Successor;
    % Pointing back to us = dont do anything
    {Id, _} -> 
    % Pointing to itself 
      Successor;
    {Skey, _} -> 
      Spid ! {notify, {Id, self()}}, 
	    Successor;
    % updating if some one is entering
    {Xkey, Xpid} -> 
      case key:between(Xkey, Id, Skey) of 
        true -> 
            % Adopt this node as our successor 
			% Run stabilization again
          stabilize(Pred,Id,{Xkey, Xpid}); 
        false -> 
            % Adopt this node as our successor 
			% Run stabilization again
          Spid ! {notify, {Id, self()}},
		      Successor
      end 
  end.


request(Peer, Predecessor) ->
  case Predecessor of
      % "I dont have a predecessor"
    nil -> 
        % Tell this to Peer
      Peer ! {status, nil};
    % this my predecessor
    % Tell this to peer (successor).
    % node recieves request messege from stabilize/1
    % after it calls this function
    {Pkey, Ppid} -> 
    % we now send the status message, to the node, and stabilizer/3 is called.
      Peer ! {status, {Pkey, Ppid}}
end.

% notify = telling pred that we are its successor
notify({Nkey, Npid}, Id, Predecessor) ->
  case Predecessor of
    % if no predecessor, we can use new node as predecessor
    nil -> 
      {Nkey, Npid};
    {Pkey,  _} ->
      case key:between(Nkey, Pkey, Id) of 
        true ->
		% if new key (Nkey) is in between, the new node can be our successor
          {Nkey, Npid};
        false -> 
		% if not, we keep our existing predecessor
        % How will it know if we have discarded its friendly proposal? 
        % Once stabilizaéd agein, we inform the new node about his new successor 
          Predecessor 
      end 
  end.

%Create signal
% Print Id
% Forward first Id to successor
create_probe(Id, Successor) ->
  {_Key, Pid} = Successor,
  io:format("ID : ~w,",[Id]),
  Pid ! {probe, Id, [Id], erlang:monotonic_time()}.

% Print time of probe
remove_probe(Time, Nodes) ->
  T = erlang:monotonic_time(),
  T2 = T - Time,
  io:format("Total Time : ~w , Nodes : ~w~n",[T2,Nodes]).

% Forward current Id, the reference ? (all Ids), nodes ?, current time to successor
% Concatenate nodes with current Id. 
forward_probe(Ref, T, Nodes, Id, Successor) ->
  {_Key, Pid} = Successor,
  io:format("ID : ~w~n",[Id]),
  Pid ! {probe, Ref, Nodes ++ [Id], T}.