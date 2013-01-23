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

-module(arya_token_storage).
-behaviour(gen_server).
-import(jdb, [report/3, report/2, appenv/3, getenv/1]).

%% Callbacks:
-export([ terminate/2]).
-export([ init/1]).
-export([ handle_info/2, handle_call/3, handle_cast/2]). 
-export([ code_change/3]).

%% Server queries:
-export([ start_link/1, get_pid/1, add_pid/2, delete_pid/1]).

%%% -------------------------------------------------------
%%% This is the Arya server storing PIDs for all the tokens
%%% handling by Arya in the current moment. It describes  
%%% callbacks for gen_server handling UDP socket for both   
%%% incoming and outcoming connections.
%%% -------------------------------------------------------

%% Starting:

%%% @spec start_link(Args) -> Result
%%%    Args = term(), ignored
%%%    Result = {ok,Pid} | ignore | {error,Error}
%%%     Pid = pid()
%%%     Error = {already_started,Pid} | term()
%%%
%%% @doc Starts the Arya token storage.
%%%
start_link(_) ->
  gen_server:start_link(
    {local, ?MODULE}, 
    ?MODULE, 
    [], 
    []
  ).
    
%% Routines:

%%% @spec get_pid(Token) -> {ok, Pid} | false
%%%    Token = false | term() 
%%%
%%% @doc Returns Pid if there is a pid on storage, and false otherwise.
%%%
get_pid(Token) ->
  Pid = gen_server:call(arya_token_storage, {get, Token}),
  report(2, "Getting token", {Token, Pid}),
  Pid.

%%% @spec add_pid(Token, Pid) -> ok
%%%    Token = term()
%%%
%%% @doc Adds Pid to the storage.
%%%
add_pid(Token, Pid) ->
  report(2, "Adding PID", {Token, Pid}),
  gen_server:call(arya_token_storage, {add, Token, Pid}).
  
%%% @spec delete_pid(Token) -> ok
%%%    Token = term()
%%%
%%% @doc Removes an entry associated with the Token from the storage.
%%%
delete_pid(Token ) ->
  Pid = gen_server:call(arya_token_storage, {del, Token}),
  report(2, "Deleting PID", {Token, Pid}),
  Pid.
  
%% Callbacks:

init(_) ->
  report(1, "Token storage started"),
  {ok, assoc:empty()}.
  
terminate(Reason, _State ) ->
  report(1, "Token storage terminated"),
  report(2, "Reason", Reason),
  ok.
  
%%% No info handled.
handle_info(Data, State ) ->
  report(0, "Wrong info in Token storage", Data),
  {noreply, State }.
  
%%% Storage handles all the requrests via synchronous calls.
%% Calls:
handle_call({get, Token}, _, Storage) ->
  Ret = assoc:get(Token, Storage),
  case Ret of
    false ->
      {reply, false, Storage};
    {value, Pid} ->
      {reply, {ok, Pid}, Storage}
  end;
  
handle_call({add, Token, Pid}, _, Storage) ->
  NewStorage = assoc:put(Token, Pid, Storage),
  {reply, ok, NewStorage};
  
handle_call({del, Token}, _, Storage) ->
  case assoc:delete(Token, Storage) of 
    {_, NewStorage} -> 
      {reply, ok, NewStorage};
    _ -> 
      {reply, ok, Storage}
  end;
  
handle_call(Data, _, State) ->
  report(0, "Wrong call in Token storage",Data),
  {noreply, State}.

%% Casts:
handle_cast(Data, State) ->
  report(0, "Wrong cast in Token storage",Data),
  {noreply, State }.
  
%% Just dummy code_change callback.
code_change(_, State, _) ->
  report(1, "Code change in Token storage"),
  {ok, State }.
  

  
