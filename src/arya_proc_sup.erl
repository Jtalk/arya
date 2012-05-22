%%
%%	Copyright (C) 2012 Nazarenko Roman.
%%
%%	This file is part of Arya.
%%
%%	Arya is free software: you can redistribute it and/or modify
%%	it under the terms of the GNU General Public License as published by
%%	the Free Software Foundation, either version 3 of the License, or
%%	(at your option) any later version.
%%
%%	Arya is distributed in the hope that it will be useful,
%%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%	GNU General Public License for more details.
%%
%%	You should have received a copy of the GNU General Public License
%%	along with Arya.  If not, see <http://www.gnu.org/licenses/>.

%%	Author: Nazarenko Roman <mailto: me@jtalk.me>
%%	License: <http://www.gnu.org/licenses/gpl.html>

-module(arya_proc_sup).
-behaviour(supervisor).

%% Debug
-import(jdb, [report/3, report/2, appenv/3, getenv/1]).

%% Starting
-export([ start_link/1]).

%% Starting callback
-export([ init/1]).

%% Creation of the new child
-export([ child/0]).


%%% -----------------------------------------------------
%%% This is the processors' supervisor of Arya server. 
%%% It looks after an essential workers processing all
%%% the incoming data before it sends to the downloader
%%% process (or just dropped, if data is malformed).
%%%
%%% For further information about processor module see its 
%%% documentation.
%%%
%%% This modue describes callbacks for supervisor starting  
%%% and terminating as well as a worker adding method.
%%% -----------------------------------------------------

%% Starting

%%% @spec start_link( Timing) -> Result
%%%		Timing = { MaxR, MaxT, Await} 
%%%		 MaxR = integer() > 0
%%%		 MaxT = integer() > 0
%%%		 Await = integer() >= 0 | infinity
%%%		Result = startlink_ret()
%%%
%%%	For details about Timing and Result see supervisor(3).
%%%
%%% @doc Initializes Arya processors and handles them.
%%%
start_link( { MaxR, MaxT, Await} ) ->
	report( 1, "Starting processors' supervisor"),
	supervisor:start_link(
		{ local, arya_proc_sup },
		arya_proc_sup,
		{ MaxR, MaxT, Await}
	).

%%% @see Module:init/1 in supervisor(3).
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
			
%% Routines			
%%% @spec process( Message) -> ok | {error, Reason}
%%%		Message = record( recv)
%%%		
%%%	@doc Starts new Arya processor to process a Message specified.
%%%
child() ->
	report( 1, "Child creating in Process supervisor"),
	supervisor:start_child( ?MODULE, []).
	