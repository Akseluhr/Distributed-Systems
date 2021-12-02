%% @author akseluhr
%% @doc @todo Add description to hist.


-module(hist).

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, update/3]).

new(Name) ->
	[{Name, inf}].%[{Name, Args}].

update(Node, N, History) ->
	Entry = lists:keyfind(Node, 1, History), 
	CheckMessage = element(2, Entry),
		if is_tuple(Entry) ->
			if CheckMessage < N ->
			 	old;
			true->
				Updated = lists:keyreplace(Node, 1, History, {Node, N}),
				{new, Updated}
			end;
		true->
			new(Node)
	end.
		
		
	

