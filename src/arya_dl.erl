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

-module(arya_dl).
-behaviour(gen_server).

%% Standard overproject types.
-include("arya_types.hrl").

%% Starting.
-export([ start_link/1]).
%% Start/stop callbacks.
-export([ init/1, terminate/2]).
%% Processing.
-export([ process/2, download/1]).
%% Event handling callbacks.
-export([ handle_call/3, handle_info/2, handle_cast/2]).
%% Patching callback.
-export([ code_change/3]).

%%% -----------------------------------------------------
%%% This is the tier 2 Arya processor (downloader). It 
%%% receives pre-processed messages from the supervisor
%%% (implicitly from the Arya processor) and handles it.
%%%
%%% It recognizes the message received and adds it's con-
%%% tent to the internal fsm state. When all the messages
%%% are in, it downloads the page requested and gets ready
%%% to send it back.
%%%
%%% This modue describes callbacks for gen_fsm starting  
%%% and terminating as well as a workers' methods.
%%%
%%% Really, using gen_fsm is not required for this module,
%%% it's simple enough to be run as a single function, but
%%% gen_fsm allows us to use supervision and automated 
%%% process handling, so let it be.
%%% -----------------------------------------------------

%% Starting
%%% @spec start_link( Token) -> Result
%%%		Token = integer() > 0
%%%		Result = {ok,Pid} | ignore | {error,Error}
%%%		 Pid = pid()
%%%		 Error = {already_started,Pid} | term()
%%%	
%%% @doc Creates new Arya downloader.
%%%
start_link(Token) ->
	gen_server:start_link( arya_dl, Token, []).	

%% Callbacks

%%% @see Module:init/1 in gen_fsm(3).
init( Token ) ->
	random:seed( now()),
	report( 1, "Starting Downloader"),
	report( 2, "Downloader token", Token),
	{ ok, #state{ token = Token } }.
	
process( Pid, Data) ->
	gen_server:call( Pid, Data).
	
download( Pid) ->
	gen_server:cast( Pid, download).
	
%%% @see Module:handle_event/3 in gen_fsm(3).
handle_cast( download, State) ->
	report( 1, "Downloader received cast for page download"),
	report( 3, "Downloader state", State),

	if State#state.data_len > 0 ->
		report( 1, "Already downloaded"),
		{ noreply, State};
	true ->
		{ Data, Len} = arya_common:download( State),
		{ noreply, 	State#state { 
						data = Data, 
						data_len = Len
					}
		}
	end;
handle_cast( Data, State) ->
	report( 0, "Wrong cast in Downloader"),
	report( 3, "Downloader data", Data),
	report( 3, "Downloader state", State),
	{ noreply, State}.

%%% @see Module:handle_sync_event/4 in gen_fsm(3).
handle_call( Data, _, State) when is_record( Data, entry) ->
	arya_process:process( Data, State);
handle_call(Data, _, State) ->
	report( 0, "Wrong call in Downloader"),
	report( 3, "Downloader data", Data),
	report( 3, "Downloader state", State),
	{ reply, ok, State}.

%%% @see Module:handle_info/3 in gen_fsm(3).
handle_info( Data, State) ->
	report( 0, "Wrong info in Downloader"),
	report( 3, "Downloader data", Data),
	report( 3, "Downloader state", State),
	{ noreply, State}.
	
%%% @see Module:terminate/3 in gen_fsm(3).
terminate( Reason, State) ->
	report( 1, "Downloader terminating", Reason),
	arya_token_storage:delete_pid( State#state.token),
	ok.
	
%%% @see Module:code_change/4 in gen_fsm(3).
code_change( _, StateData, _) ->
	report( 1, "Code changing in Downloader"),
	{ ok, StateData}.