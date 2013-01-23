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

-module(arya_main_sup).
-behaviour(supervisor).

%% Debug:
-import(jdb, [report/3, report/2, appenv/3, ret/1]).

%% Callback and start function 
-export( [ start_link/1, init/1]).

%%% -----------------------------------------------------
%%% This is the main supervisor of Arya server. It looks 
%%% after all the major parts of the server:
%%%   arya_server,
%%%    arya_token_storage,
%%%    arya_proc_sup, 
%%%    arya_dl_sup.
%%%
%%% For further information about this modules see their 
%%% documentation.
%%%
%%% This modue describes callbacks for supervisor starting  
%%% and terminating.
%%% -----------------------------------------------------

%%% @spec start_link( Timing) -> Result
%%%    Timing = { MaxR, MaxT } 
%%%    Result = startlink_ret()
%%%
%%%  For details about Timing and Result see supervisor(3).
%%%
%%% @doc Initializes Arya major modules with the supervision timings provided
%%%
start_link( { MaxR, MaxT} ) ->
  supervisor:start_link( 
    { local, ?MODULE }, 
    ?MODULE, 
    { MaxR, MaxT}
  ).

init({ MaxR, MaxT} ) ->
  report(1, "Arya main supervisor initializing"),
  {ok, 
    { 
      {one_for_one, MaxR, MaxT },
      [%<Internal name> <Module name> <Start func> <Arguments> <Restart type> <Exit timeout> <Process type> <Depends>
        { arya_server, {arya_server, start_link, [null]}, transient, 1000, worker, [arya_server]},
        { arya_token_storage, { arya_token_storage, start_link, [null]}, transient, 100, worker, [arya_token_storage]},
        { arya_proc_sup, {arya_proc_sup, start_link, [{ MaxR, MaxT, 100}]}, transient, infinity, supervisor, [arya_proc_sup]},
        { arya_dl_sup, { arya_dl_sup, start_link, [{ MaxR, MaxT, 100}]}, transient, infinity, supervisor, [arya_dl_sup]}        
      ]
    }
  }.