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

-module(arya_processor).
-behaviour(gen_server).
-include("arya_types.hrl").

%% Handling:
-export([ start_link/0]).
-export([ process/2]).

%% Callbacks:
-export([ init/1, terminate/2]).
-export([ handle_call/3, handle_info/2, handle_cast/2]).
-export([ code_change/3]).

%% DNS header size.
-define(HEADER_SIZE, 12).

%%% -----------------------------------------------------
%%% This is the tier 1 Arya processor. It receives a raw
%%% message from the supervisor (and supervisor receives
%%% it from the arya_server) and handles it.
%%%
%%% It recognizes the message received, divides it into 
%%% ID, flags, questions and so on, then sends all the 
%%% parts to the tier 2 processors (arya_dl), or just drop
%%% it, if the message is malformed.
%%%
%%% This modue describes callbacks for gen_server starting  
%%% and terminating as well as a workers' methods.
%%%
%%% Really, using gen_server is not required for this module,
%%% it's simple enough to be run as a single function, but
%%% gen_server allows us to use supervision and automated 
%%% process handling, so let it be.
%%% -----------------------------------------------------

%%% @spec start_link() -> Result
%%%    Result = {ok,Pid} | ignore | {error,Error}
%%%     Pid = pid()
%%%     Error = {already_started,Pid} | term()
%%%  
%%% @doc Creates new Arya processor.
%%%
start_link() ->
  gen_server:start_link(arya_processor, [], []).

%%% @spec process(Message) -> Result
%%%    Message = record(recv)
%%%    Result = {stop, Reason, null}
%%%     Reason = normal | {error, Error}
%%%      Error = term()
%%%  
%%% @doc Main parsing routine starter.
%%%
process(Pid, Message) when is_record(Message, recv) ->
  report(1, "Processing message", Pid),
  report(3, "Message", Message),
  gen_server:cast(Pid, Message).
  
%% Routines:
  
%% Main parsing routine of the processor.
process(Message)  ->
  try
    {ok, Parsed} = arya_parse:parse(Message), % we got the url array too
    arya_decision:postprocess(Parsed), % making decision about this packet.  
    {stop, normal, null}
  catch
    error:Error ->
      ID = binary:part(Message#recv.data, 0, 2), % First two bytes are ID.
      report(0, "Error in Processor", Error),
      arya_common:send_back(Message#recv.from, {id, ID}),
      {stop, {error, Error}, null}
  end.
  
%% Callbacks:

init(_) ->
  random:seed(now()),
  report(1, "Processor starting"),
  {ok, null}.
  
handle_cast(Message, _) when is_record(Message, recv) ->
  process(Message),
  {stop, normal, null};
  
handle_cast(Data, State) ->
  report(0, "Wrong cast in Processor", Data),
  {noreply, State}.

handle_call(Data, _, State) ->
  report(0, "Wrong sync event in Processor",Data),
  {reply, ok, State}.

handle_info(Data, State) ->
  report(0, "Wrong info in Processor",Data),
  {noreply, State}.
  
terminate(Reason, _) ->
  report(1, "Terminating Processor"), 
  report(2, "Reason", Reason),
  ok.

code_change(_, StateData, _) ->
  report(1, "Code changing in Processor"),
  {ok, StateData}.
