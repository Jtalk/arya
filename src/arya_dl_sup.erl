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

-module(arya_dl_sup).
-behaviour(supervisor).

%% Debug:
-import(jdb, [report/3, report/2, appenv/3, getenv/1]).

-export([start_link/1]).
-export([child/1]).

%% Callbacks:
-export([init/1]).

%%% -----------------------------------------------------
%%% This is the downloaders' supervisor of Arya server. 
%%% It looks after an advanced workers processing previ-
%%% ously data before it become send to the client.
%%%
%%% For further information about downloader module see 
%%% arya_dl documentation.
%%%
%%% This modue describes callbacks for supervisor starting  
%%% and terminating as well as a worker adding method.
%%% -----------------------------------------------------

%%% @spec start_link(Timing) -> Result
%%%    Timing = {MaxR, MaxT, Await} 
%%%     MaxR = integer() > 0
%%%     MaxT = integer() > 0
%%%     Await = integer() >= 0 | infinity
%%%    Result = startlink_ret()
%%%
%%% For details about Timing and Result see supervisor(3).
%%%
%%% @doc Initializes Arya processors and handles them.
%%%
start_link(Args) ->
  report(1, "Starting downloaders' supervisor"),
  supervisor:start_link(
    {local, arya_dl_sup}, 
    arya_dl_sup, 
    Args
  ).
  
init({MaxR, MaxT, Await}) ->
  report(1, "Downloader supervisor starting"),
  {ok, 
    {
      {simple_one_for_one , MaxR, MaxT }, 
      [
        {  
          arya_dl, {arya_dl, start_link, []},
          temporary, Await, worker, [arya_dl]
        }
      ] 
    }
  }.
      
%%% @spec child(Token) -> {ok, pid()} | false
%%%     Token = integer()
%%%    
%%%  @doc Starts new Arya downloader to process further messages.
%%% It registers itself in arya_token_storage.
%%%
child(Token) ->
  report(1, "Creating a downloader"),
  report(2, "Token", Token),
  case arya_token_storage:get_pid(Token) of
    false ->
      {ok, Pid} = supervisor:start_child(arya_dl_sup, [Token]),
      arya_token_storage:add_pid(Token, Pid),
      {ok, Pid};
    Result ->
      Result
  end.
