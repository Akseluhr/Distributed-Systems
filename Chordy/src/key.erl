%% @author akseluhr
%% @doc @todo Add description to key.


-module(key).

%% ====================================================================
%% API functions
%% ====================================================================
-export([generate/0, between/3]).

generate() ->
  rand:uniform(1000000000).
  
% OBS key:between(3, 5, 10). funkar ej. Antar att det är utanför scope
between(Key, From, To) ->
	% Full circle, anything is in between From and True = true. 
	% Unless Key is greater than From/To, then it is outside the scope.
	%if (Key == To) and (Key < From)  ->
	%	true;
	 %  true -> 
			% From = 2, Key = 3, To  = 8
			% Key greater than From, and less than or equal to To, it is in between
	%		if (Key > From) and (Key =< To) ->
	%			true;
	%			true ->
						% Checks if in between last and first peer.
	%					if (From > To) and (Key > From) ->
	%						true;
	%						true ->
	%							false
	%					end
	%			end
%	end.

	% if from is any node but last node
	if(From < To) and (Key > From) and (Key =< To) ->
		true;
	% if from is the last node
	(From > To) and ((Key > From) or (Key =< To)) ->
		true; 
	% first node
	(From == To) ->
		true; 
	true ->
		false	
	
	
	end.



		