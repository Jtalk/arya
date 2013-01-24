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
%% @doc This is the processors' supervisor of Arya server. 
%% It looks after an essential workers processing all
%% the incoming data before it sends to the downloader
%% process (or just dropped, if data is malformed).
%%
%% For further information about processor module see its 
%% documentation.
%%
%% This modue describes callbacks for supervisor starting  
%% and terminating as well as a worker adding method.


-module(arya_proc_sup).
-behaviour(supervisor).

%% Debug
-import(jdb, [report/3, report/2, appenv/3, getenv/1]).

%% Making new processor and supervisor starting.
-export([ start_link/1]).
-export([ child/0]).

%% Callbacks:
-export([ init/1]).

%%% @spec child() -> {ok, child()} | {error, term()}
%%%    
%%% @doc Starts new Arya processor and returns its PID.
%%%
child() ->
  report( 1, "Child creating in Process supervisor"),
  supervisor:start_child( ?MODULE, []).

%%% @spec start_link( Timing) -> Result
%%%    Timing = { MaxR, MaxT, Await} 
%%%     MaxR = integer() 
%%%     MaxT = integer() 
%%%     Await = integer() | infinity
%%%    Result = startlink_ret()
%%%
%%% @doc Initializes Arya processors and handles them.
%%%
start_link( { MaxR, MaxT, Await} ) ->
  report( 1, "Starting processors' supervisor"),
  supervisor:start_link(
    { local, arya_proc_sup },
    arya_proc_sup,
    { MaxR, MaxT, Await} %% @see supervisor manual.
  ).

%% Callbacks:
%%% @private
%%% @doc Starts simple one-for-one supervisor, producing arya_processor modules.
%%%
init( { MaxR, MaxT, Await} ) ->
  report( 1, "Process supervisor starting"),
  { ok, 
    {
      { simple_one_for_one , MaxR, MaxT }, 
      [
        {   
          arya_processor, { arya_processor, start_link, []},
          temporary, Await, worker, [arya_processor] % See supervisor behaviour articles
        }
      ]
    }
  }.
  