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

-module(arya_processor).
-behaviour(gen_server).

%% Debig.
-import(error_logger, [format/2]).
%% Standard overproject types.
-include("arya_types.hrl").

%% Starting.
-export([ start_link/0]).
%% Start/stop callbacks.
-export([ init/1, terminate/2]).
%% Processimg callback.
-export([ process/2]).
%% Events handling callbacks.
-export([ handle_call/3, handle_info/2, handle_cast/2]).
%% Patching callback.
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

%% Starting
%%% @spec start_link() -> Result
%%%		Result = {ok,Pid} | ignore | {error,Error}
%%%		 Pid = pid()
%%%		 Error = {already_started,Pid} | term()
%%%	
%%% @doc Creates new Arya processor.
%%%
start_link() ->
	gen_server:start_link( arya_processor, [], []).

%%% @see Module:init/1 in gen_fsm(3).
init( _) ->
	random:seed( now()),
	{ ok, null}.
	
process( Pid, Message) when is_record( Message, recv) ->
	gen_server:cast( Pid, Message).
	
%% Callback		
%%% @spec process( Message) -> Result
%%%		Message = record(recv)
%%%		Result = { stop, Reason, null}
%%%		 Reason = normal | { error, Error}
%%%		  Error = term()
%%%	
%%% @doc Main parsing routine of the processor.
%%%
process( Message)  ->
	%%try
		{ ok, Parsed} = arya_common:parse( Message), % we got the url array too
		arya_common:postprocess( Parsed), % making decision about this packet.	
		{ stop, normal, null}
	%%catch
	%%	error:Error ->
	%%		ID = binary:part( Message#recv.data, 0, 2),
	%%		format( "Error in arya_processor: ", Error),
	%%		arya_common:send_back( Message#recv.from, { id, ID}),
	%%		{ stop, {error, Error}, null}
	%% end.
	.
		
%%% @see Module:handle_cast/2 in gen_server(3).
handle_cast( Message, _) when is_record( Message, recv) ->
	process( Message), % start handling cycle
	{ stop, normal, null};
handle_cast( Data, State) ->
	format("Wrong cast in arya_processor: ", Data),
	{ noreply, State}.

%%% @see Module:handle_sync_event/4 in gen_fsm(3).
handle_call( Data, _, State) ->
	format("Wrong sync event in arya_processor: ",Data),
	{ reply, ok, State}.

%%% @see Module:handle_info/3 in gen_fsm(3).
handle_info( Data, State) ->
	format("Wrong info in arya_processor: ",Data),
	{ noreply, State}.
	
%%% @see Module:terminate/3 in gen_fsm(3).
terminate( _, _) ->
	ok.
	
%%% @see Module:code_change/4 in gen_fsm(3).
code_change( _, StateData, _) ->
	{ ok, StateData}.
