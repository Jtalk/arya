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

-module(arya_token_storage).
-behaviour(gen_server).

-import(error_logger, [format/2]).

-export([ start_link/1, terminate/2]).
-export([ init/1]).
-export([ handle_info/2, handle_call/3, handle_cast/2]). 
-export([ code_change/3]).
-export([ get_pid/1, add_pid/2, delete_pid/1]).


%%% -------------------------------------------------------
%%% This is the Arya server storing PIDs for all the tokens
%%% handling by Arya in the current moment. It describes  
%%% callbacks for gen_server handling UDP socket for both   
%%% incoming and outcoming connections.
%%% -------------------------------------------------------

%% Starting
%%% @spec start_link( Args) -> Result
%%%		Args = term(), ignored
%%%		Result = {ok,Pid} | ignore | {error,Error}
%%%		 Pid = pid()
%%%		 Error = {already_started,Pid} | term()
%%%
%%% @doc Starts the Arya token storage.
%%%
start_link(_) ->
	gen_server:start_link( 
		{ local, ?MODULE}, 
		?MODULE, 
		[], 
		[]
	).
	
%%% @spec init( Args) -> { ok, nil}
%%%		Args = term(), ignored
%%%
%%% @doc Arya token storage starting callback.
%%%
%%% See gen_server(3) for further information.
%%%
init(_) ->
	{ ok, assoc:empty()}.
	
%% Routines
%%% @spec get_pid( Token) -> { ok, Pid} | false
%%%		Token = term()
%%%
%%% @doc Returns Pid if there is a pid on storage, and false otherwise.
%%%
get_pid( Token ) ->
	gen_server:call( arya_token_storage, { get, Token}).

%%% @spec add_pid( Token, Pid) -> ok
%%%		Token = term()
%%%
%%% @doc Adds Pid to the storage.
%%%
add_pid( Token, Pid) ->
	gen_server:call( arya_token_storage, { add, Token, Pid}).
	
%%% @spec delete_pid( Token) -> ok
%%%		Token = term()
%%%
%%% @doc Removes an entry associated with the Token from the storage.
%%%
delete_pid( Token ) ->
	gen_server:call( arya_token_storage, { del, Token}).
	
%% Callbacks
%%% @doc Termination callback.
terminate( _Reason, _State ) ->
	ok.
	
%%% @see Module:handle_info in gen_server(3).
handle_info( Data, State ) ->
	format("Wrong info in arya_token_storage:",Data),
	{ noreply, State }.
	
%%% @see Module:handle_call in gen_server(3).
handle_call({ get, Token}, _, State) ->
	Ret = assoc:get(Token, State),
	case Ret of
		false ->
			{ reply, false, State};
		{ value, Pid} ->
			{ reply, {ok, Pid}, State}
	end;
handle_call({ add, Token, Pid}, _, State) ->
	NewState = assoc:put(Token, Pid, State),
	{ reply, ok, NewState};
handle_call({ del, Token}, _, State) ->
	case assoc:delete( Token, State) of 
		{ _, NewState} -> 
			{ reply, ok, NewState};
		_ -> 
			{ reply, ok, State}
	end;
handle_call(Data, _, State) ->
	format("Wrong call in arya_token_storage:",Data),
	{ noreply, State}.

%%% @see Module:handle_cast in gen_server(3).
handle_cast( Data, State) ->
	format("Wrong cast in arya_token_storage:",Data),
	{ noreply, State }.
	
%%% @see Module:code_change in gen_server(3).
code_change(_, State, _) ->
	{ ok, State }.
	

	
