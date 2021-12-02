-module(routy).
-export([start/2,stop/1]).

% starts and registers a router process
start(Reg, Name) ->
    register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
    Node ! stop,
    unregister(Node).

% initiate the router
init(Name) ->
  Interface = router:new(),
  Map = map:new(),
  Table = dijkstra:table(Interface, Map),
  History = hist:new(Name),
  router(Name, 0, History, Interface, Table, Map).

% adding interfaces
router(Name, N, History, Interface, Table, Map) ->
  receive
		{add, Node, Pid} ->
		    Ref = erlang:monitor(process,Pid),
		    Interface1 = router:add(Node, Ref, Pid, Interface),
		    router(Name, N, History, Interface1, Table, Map);
		{remove, Node} ->
		    {ok, Ref} = router:ref(Node, Interface),
		    erlang:demonitor(Ref),
		    Interface1 = router:remove(Node, Interface),
		    router(Name, N, History, Interface1, Table, Map);
		{'DOWN', Ref, process, _, _} ->
		    {ok, Down} = router:name(Ref, Interface),
		    io:format("~w: exit recived from ~w~n", [Name, Down]),
		    Interface1 = router:remove(Down, Interface),
		    router(Name, N, History, Interface1, Table, Map);
		% 5.2 link-state messages
		% Next we need to implement the link-state message. 
	   % Old values, < R, are discarded since it is considered an old message. 

		{links, Node, R, Links} ->
	    case hist:update(Node, R, History) of
		    {new, History1} ->
		      router:broadcast({links, Node, R, Links}, Interface),
		      Map1 = map:update(Node, Links, Map),
		      router(Name, N, History1, Interface, Table, Map1);
		    old ->
		      router(Name, N, History, Interface, Table, Map)
	    end;
		% 5.4 Routing a message
		% message has actually arrived to the final destination.
		{route, Name, From, Message} ->
    	  io:format("(~w) node: received message: (~w) from (~w)~n", [Name, Message,From]),
    	  router(Name, N, History, Interface, Table, Map);
		% Forwarding messages that is not for the current router
		% This part is for the cases when we are used as a link in the netwoek
		% If gateway is found in the routing table, forward the message to the gateway. 
		% If not, or no interface found, we drop the packet

		{route, To, From, Message} ->
	   		 io:format("(~w) node: routing message: (~w) from: (~w)~n", [Name, Message,From]),
	   		 case dijkstra:route(To, Table) of
			    {ok, Gateway} ->
			      case router:lookup(Gateway, Interface) of
				      {ok, Pid} ->
				        Pid ! {route, To, From, Message};
				      notfound ->
				        ok
			      end;
			    notfound ->
			      ok
		   	 end,
	    router(Name, N, History, Interface, Table, Map);
		% Allows local user to initiate routing of message w/o knowing the local router name 

		{send, To, Message} ->
		      self() ! {route, To, Name, Message},
		      router(Name, N, History, Interface, Table, Map);
		% updating routing table when map is updated - dijakstra
		% every time we receive a link-state message or every time the map changes 
		% now manually with r1 ! update
		update ->
		    Table1 = dijkstra:table(router:list(Interface), Map),
		    router(Name, N, History, Interface, Table1, Map);
		% boradcast a link-state message by router. senfing msg to
	   broadcast ->
	    Message = {links, Name, N, router:list(Interface)},
	    router:broadcast(Message, Interface),
	    router(Name, N+1, History, Interface, Table, Map);
	   % prints out the status for a router
		status ->
			io:format("Name: ~w~nN: ~w~nHistory: ~w~nInterfaces: ~w~nTable: ~w~nMap: ~w~n", [Name, N, History, Interface, Table, Map]),
			router(Name, N, History, Interface, Table, Map);
		{status, From} ->
	      From ! {status, {Name, N, History, Interface, Table, Map}},
	      router(Name, N, History, Interface, Table, Map);
		stop ->
			ok
	end.