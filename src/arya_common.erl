%%
%%  Copyright (C) 2012-2013 Roman Nazarenko.
%%
%%  This file is part of Arya.
%%
%%  Arya is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  Arya is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with Arya.  If not, see <http://www.gnu.org/licenses/>.

%%  Author: Nazarenko Roman <mailto: me@jtalk.me>
%%  License: <http://www.gnu.org/licenses/gpl.html>

%% @author Roman Nazarenko <me@jtalk.me>
%% @copyright 2012-2013 Roman Nazarenko
%% @doc This is the tier 1 and 2 Arya processors' special 
%%  subroutines. 

-module(arya_common).
-include("arya_types.hrl").

%% Dns error header.
-define(ERROR_HEADER, <<133, 2, 0, 0, 0, 0, 0, 0, 0, 0>> ).
%% Response part length
-define(RESPONSE_LEN, 100).
%% HTTP port.
-define(PORT, 80).
%% Host identifier.
-define(HOST, "host").

%% Dunno Y I NO delete this.
-define(SPACE, $ ).
-define(SLASH, $/).
-define(NEWLINE, "\r\n").

-define(FIRST_ELEM, -1).
-define(SEPARATOR, $,).

-export([download/1, send_back/2]).
-export([request/3]).

%%% @spec send_back(To, Packet) -> ok
%%%   To = {Address, Port}
%%%     Address = inet:ip_address() | inet:hostname()
%%%    Port = inet:port_number()
%%%    Packet = {id, ID} | binary()
%%%     ID = binary()
%%%
%%%  @doc Sends a message specified to the Address and Port using arya_server.
%%%
send_back(To, {id, ID }) -> %% If only ID is sent, then error occured.
  arya_server:send(#send {
      to = To, 
      data = << ID/binary, ?ERROR_HEADER/binary >> 
    }
  );
send_back(To, Packet) -> %% Packet is already formed there.
  arya_server:send(#send {to = To, data = Packet}).

%% Helpers: 

%%% @doc Perform all remote page downloading routines.
download(State) when is_record(State, state) ->
  Data = construct(State#state.url, <<>>),
  {ok, PureRequest} = decrypt(Data),
  {ok, Address} = address(PureRequest),
  {ok, Packet} = request(Address, ?PORT, PureRequest),
  {_Parts, _Len} = split(Packet, ?RESPONSE_LEN).
  
%%% @doc Constructs a packet from the list provided.
%%% @TODO: Rewrite with foldl.
construct([], Ready) ->
  Ready;
construct([ {_, Part} | Rest], Ready) ->
  New = << Ready/binary, Part/binary >>,
  construct(Rest, New).
  
%%% @doc Incoming message decryption (base64).
decrypt(Data ) ->
  Replaced1 = binary:replace(Data, << $- >>, << $+ >>), % Those replaces for base64-web encoding.
  Replaced2 = binary:replace(Replaced1, << $_ >>, << $/ >>),
  {ok, decode(Replaced2, 3)}.
  
%%% @doc Decodes the binary given, tries to add N parts sequentially until 
%%% data becomes decodable, falls if not.
%%% Since there're some trouble transferring '=' via DNS queries, this
%%% nail is the only way.
decode(Data, -1) ->
  error({badarg, Data});
decode(Data, Num) ->
  case catch base64:decode_to_string(Data) of
    Str when is_list(Str) ->
      Str;
    _ -> %% If there was an exception.
      decode(<< Data/binary, $= >>, Num-1)
  end.
  
%%% @doc Returns remote host's address extracted from Data.
%%% Looks kinda magic. In fact, just filters 'example.org' from the HTTP requrst.
address(Request) -> % needs future research whether it even works.
  Lower = string:to_lower(Request),
  WithBlanks = 
    case string:str(Lower, ?HOST) of
      0 -> % No "host:" property in the request 
        Pos = string:chr(Lower, ?SPACE),
        Len = string:chr(Lower, ?SLASH) - Pos,
        string:substr(Lower, Pos+1, Len);
      Pos ->
        Len = string:str(
          string:substr(Lower, Pos+string:len(?HOST)+1, string:len(Lower) - Pos), 
          ?NEWLINE
        ),
        string:substr(Lower, Pos+string:len(?HOST)+1, Len-1)
    end,
  {ok, string:strip(WithBlanks)}.
  
%%% @doc Makes HTTP request to the remote server specified.
request(Address, Port, Data ) ->
  {ok, Socket} = gen_tcp:connect(Address, Port, [ {active, false}, binary ]),
  ok = gen_tcp:send(Socket, Data),
  {ok, _Packet} = recv(Socket, <<>>).
  
%%% @doc Recursively receives messages until query's empty.
recv(Socket, Data) ->
  {ok, Timeout} = application:get_env(timeout),
  case gen_tcp:recv(Socket, 0, Timeout) of 
    {ok, Packet} ->
      recv(Socket, << Data/binary, Packet/binary >>);
    {error, _} ->
      gen_tcp:close(Socket),
      {ok, Data}
  end.
  
%%% @doc Splits a packet to parts not greater than lenght specified.
%%% @TODO: Try to use folding here.
split(Packet, MaxLen) when is_binary(Packet), is_integer(MaxLen) ->
  {Parts, Len} = split(Packet, MaxLen, []),
  %% format("PARTS: ", Parts),
  {lists:reverse(Parts), Len}.

%%% @doc Just split/2 recursive worker (with accumulator).
split(<<>>, _Len, Array) ->
  {Array, length(Array)};
split(Data, Len, Array) ->
  {Part, Rest} = 
    try
      {binary:part(Data, 0, Len),
      binary:part(Data, Len, byte_size(Data) - Len) }
    catch
      error:badarg -> 
        {Data, <<>> }
    end,
  split(Rest, Len, [ Part | Array]).  
