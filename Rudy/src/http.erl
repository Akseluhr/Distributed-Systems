%% @author akseluhr
%% @doc @todo Add description to http.


-module(http).

%% ====================================================================
%% API functions
%% ====================================================================
-export([parse_request/1, ok/1, get/1]).

% cd("User/eclipse-workspace/Rudy/src/").
% http:parse_request("GET /index.html HTTP/1.1\r\nfoo 34\r\n\r\nHello").
% > {{get,"/index.html",v11},["foo 34"],"Hello"}
% Så vad händer här?
% Vi ser vår input - vi hämtar meddelandet "Hello" som finns i body
% Notera att versionen hämtas
% från sindan /index.html
% och metoden "get" skrivs ut, samt headers "foo 34".


%% ====================================================================
%% Internal functions
%% ====================================================================

% Parsing of a http get request
% This means that the parser interprets a byte stream according to the http specification
% This contains:
% A request line, optional sequence of headers, a carriage return line feed 
% Implementation of each parsing function so that it will parse its element and return a tuple 
% Consisting of the parsed result and the rest of the string. 
% _ = vilken term som helst
% Tänk nedan som "main program" - alla funktioner kommer sedan implementeras
% OBS! vad är R0? en länk? 

parse_request(R0) ->
	{Request, R1} = request_line(R0),
	{Headers, R2} = headers(R1),
	{Body, _} = message_body(R2),
	{Request, Headers, Body}.

% strings = list of integers.
% I följande kod implementerar vi en parser för en GET - request. (32 = space)'
% vi hittar URI, http-versionen och ... 
% URI = identifierare av en specifik resurs. Tex page, book or document
% URL = special typ av identifierare som också talar om hur vi ska tillgå denna, tex HTTP, FRP
%  TEX URL:  https://
%kom ihåg! 
% line feed = backslash N
% carriege return = moving the cursor to the beginning of the line 
% Detta är CRLF
% Vi returnar i sista raden
% Första elementet i returnerade tupeln (get, URI, Ver) är den parsade representationen av request line
% och R3 är resten av strängen (R0 ?)

% Funktionen kallas på av vår första funktion
% Get + mellanslag och resten av inputsträngen parsas
% URI- funktionen kallas på
% därefter version- funktionen. resultatet av detta läggs in i en lista med CRLF
% "get", URI (index.html), versionen, returneras i en kollektion.
request_line([$G, $E, $T, 32 |R0]) ->
	{URI, R1} = request_uri(R0),
	{Ver, R2} = http_version(R1),
	[13,10|R3] = R2,
	{{get, URI, Ver}, R3}.

% nu implementerar vi request_uri som finns ovan. returnerar en sträng
% blankrad sedan resten av R0-strängen. Detta eftersom vi la blankrad efter get
% först basfall
% därefter 
% det efter sista kommat i en funktion returneras - men även efter pilen!!!! 
% Semikolon: "orelse"
% KOMMA: "ANDALSO"
% I exemplet nedan, vad är C? Character? fråga imorgon 

request_uri([32|R0])->
	{[], R0}; % end of base case
request_uri([C|R0]) ->
	{Rest, R1} = request_uri(R0),
	{[C|Rest], R1}.

% nu ska vi läsa http-versioner - antingen 1.0 eller 1.1
% Version 1.1 ovan, version 1.0 under 

http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
	{v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
	{v10, R0}.

%headersfunktioner nedan 
% den första behandlar en sekvens av headers 
%(eftersom vissa http-requests/messenges innehåller flera headers)
% headers används flr att skicka ytterligare information mellan klient och server genom
% request och response header
% glöm ej. 13 = carriage reutrn, 10 = line feed 

headers([13,10|R0]) ->
	{[],R0};
headers(R0) ->
	{Header, R1} = header(R0),
	{Rest, R2} = headers(R1),
	{[Header|Rest], R2}.

header([13,10|R0]) ->
	{[], R0};
header([C|R0]) ->
	{Rest, R1} = header(R0),
	{[C|Rest], R1}.

% body nedan 
% längden av body finns i headers av requesten (snarare)
% finns även flera sätt - bökigt, ngt som ej behandlas
% input: R (string)
% return : R i en lista ????
message_body(R) ->
	{R, []}.

% ===========================================

%replies from server
% double r/n, för att täcka upp för tomma headers
% socket här != socket som används för kommunikation. 


ok(Body) ->
 	"HTTP/1.1 OK 200 OK\r\n" ++ "\r\n" ++ Body.

get(URI) ->
	"GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".


