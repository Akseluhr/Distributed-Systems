-module(node3).
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
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  node(Id, Predecessor, Successor, storage:create()).

connect(Id, nil) ->
  {ok, {Id, self()}};
connect(_Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} -> 
      {ok, {Skey, Peer}}
  after ?Timeout ->
    io:format("Time out: no response~n",[])
end.

node(Id, Predecessor, Successor, Store) ->
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store);
    {notify, New} ->
      {Pred, NewStore} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, NewStore);
    {request, Peer} ->
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor, Store);
    {status, Pred} ->
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ, Store);
    stabilize ->
	    stabilize(Successor),
	    node(Id, Predecessor, Successor, Store);
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Store);
    {probe, Id, Nodes, T} -> 
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Store);
    {add, Key, Value, Qref, Client} ->
        Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
        node(Id, Predecessor, Successor, Added);
    {lookup, Key, Qref, Client} ->
        lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
        node(Id, Predecessor, Successor, Store);
    {handover, Elements} ->
        Merged = storage:merge(Store, Elements),
        node(Id, Predecessor, Successor, Merged);
    state -> 
        io:format("Id: ~w, Predecessor: ~w, Successor: ~w, Store: ~w", [Id, Predecessor, Successor, Store]),
        node(Id, Predecessor, Successor, Store)
 
end.

stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of
    nil -> 
      Spid ! {notify, {Id, self()}}, 
	    Successor;
    {Id, _} ->
      Successor;
    {Skey, _} -> 
      Spid ! {notify, {Id, self()}}, 
	    Successor;
    {Xkey, Xpid} -> 
      case key:between(Xkey, Id, Skey) of 
        true -> 
          stabilize(Pred,Id,{Xkey, Xpid}); 
        false -> 
          Spid ! {notify, {Id, self()}},
		      Successor
      end 
  end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).


stabilize({_, Spid}) ->
    Spid ! {request, self()}.


request(Peer, Predecessor) ->
  case Predecessor of
    nil -> 
      Peer ! {status, nil};
    {Pkey, Ppid} -> 
      Peer ! {status, {Pkey, Ppid}}
end.



create_probe(Id, Successor) ->
  {_Key, Pid} = Successor,
  io:format("ID : ~w~n",[Id]),
  Pid ! {probe, Id, [Id], erlang:monotonic_time()}.

forward_probe(Ref, T, Nodes, Id, Successor) ->
  {_Key, Pid} = Successor,
  io:format("ID : ~w~n",[Id]),
  Pid ! {probe, Ref, Nodes ++ [Id], T}.

remove_probe(Time, Nodes) ->
  T = erlang:monotonic_time(),
  T2 = T - Time,
  io:format("Elapsed Time : ~w , Nodes : ~w~n",[T2,Nodes]).


add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
        case key:between(Key, Pkey, Id) of
            true ->
                Client ! {Qref, ok},
                    storage:add(Key, Value, Store);
                false ->
                    Spid ! {add, Key, Value, Qref, Client},
                    Store
            end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
    % if we are in between, we are responsible of the key
    % Then we send message to client about the result
    case key:between(Key, Pkey, Id) of 
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
           false ->
               {_, Spid} = Successor,
               %forwarding the request to successor
               Spid ! {lookup, Key, Qref, Client}
    end.
% notifies about responsibility in the ring once a new node is joined
notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        % if nil, we have no predecessor (?) and the new node is the first in the ring. We keep the store
        nil ->
            Keep = handover(Id, Store, Nkey, Npid),
            {{Nkey, Npid}, Keep}; % keep? 
        {Pkey,  _} -> 
            case key:between(Nkey, Pkey, Id) of
            true ->
                % If Nkey is in between predecessor key and successor, we hand over the "updated" part of the store
                Keep = handover(Id, Store, Nkey, Npid),  
                {{Nkey, Npid}, Keep};
            false ->
                % If Nkey is not in between predecessor key and successor, 
                % We keep our store or predecessor
                {Predecessor, Store}
            end
        end.

handover(Id, Store, Nkey, Npid) ->
    % split store based on new node Key
    {Rest, Keep} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    Keep.