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

-module(arya_server).
-behaviour(gen_server).
-include("arya_types.hrl").

%% Callbacks:
-export([ terminate/2]).
-export([ init/1]).
-export([ handle_info/2, handle_call/3, handle_cast/2]).
-export([ code_change/3]).

%% Working with the server (starting and sending):
-export([ start_link/1, send/1]).

%%% ---------------------------------------------------------
%%% This is the main Arya server module describes callbacks 
%%% for gen_server handling UDP socket for both incoming and 
%%% outcoming connections.
%%% ---------------------------------------------------------

%%% @spec start_link(Args) -> Result
%%%    Args = term(), ignored
%%%    Result = {ok,Pid} | ignore | {error,Error}
%%%     Pid = pid()
%%%     Error = {already_started,Pid} | term()
%%%
%%% @doc Starts the Arya UDP server.
%%% For now server starts in active mode.
%%%
start_link(Args) ->
  report(1, "Starting DNS server"),
  gen_server:start_link(
    {local, ?MODULE},
    ?MODULE,
    Args, 
    []
  ). 
    
%%% @spec send(Message) -> ok
%%%    Message = {send, Dest, Packet}
%%%     Dest = {Address, Port}
%%%      Address = inet:ip_address() | inet:hostname()
%%%      Port = inet:port()
%%%     Packet = binary()
%%%
%%% @doc Sends the message provided to the socket connected with the current gen_server.
%%%
send(Message) ->
  gen_server:cast(?MODULE, Message). 
  
%% Callbacks:  

init(_Args) ->
  Port = getenv(dns_port, "Unable to read application setting dns_port"),
  DnsParams = getenv(dns_params, "Unable to read application setting dns_params"),
  {ok, _Socket} = gen_udp:open(Port, DnsParams).
  
terminate(Reason, Socket) ->
  report(1, "Terminating DNS server"), 
  report(2, "Reason", Reason),
  gen_udp:close(Socket). % closes the socket
  
%% Handles message from the port. Since server is in active mode, all the messages are 
%% comming to the process as special Erlang messages.
handle_info(Message, Socket) when is_record(Message, udp) ->
  report(1, "DNS packet received"),
  report(3, "Message", Message),
  {ok, Child } = arya_proc_sup:child(), %% Making new worker for that packet.
  arya_processor:process(Child,  #recv {
      from = {Message#udp.address, Message#udp.port},
      data = Message#udp.data
    }
  ),
  {noreply, Socket};
  
handle_info(Data, State ) ->
  report(0, "Wrong info in DNS server",Data),
  {noreply, State }.
  
handle_call(Data, _, State) ->
  report(0, "Wrong call in DNS server",Data),
  {reply, unknown, State }.

%% Sending message.
handle_cast(Message , Socket) when is_record(Message, send) ->
  report(1, "DNS package sending"),
  report(3, "Package", Message),
  {Address, Port} = Message#send.to, % getting dest addresss/port pair
  case gen_udp:send(Socket, Address, Port, Message#send.data) of
    ok ->
      {noreply, Socket};
    {error, Reason} ->
      {stop, Reason, Socket}
  end;
  
handle_cast(Data, State) ->
  report(0, "Wrong cast in DNS server",Data),
  {noreply, State }.
  
%% Dummy
code_change(_, State, _) ->
  report(1, "Code change in DNS server"),
  {ok, State }.
