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

-module(arya).
-behaviour(application).

%% Debug
-import(error_logger, [format/2]).

%% Callbacks
-export([ start/2, stop/1]).

%%% -----------------------------------------------------
%%% This is the main Arya module describes callbacks for
%%% application starting and terminating.
%%% -----------------------------------------------------

%%% @spec start( Type, Timing) -> Result
%%%		Type = term(), ignored
%%%		Timing = { MaxR, MaxT } 
%%%		Result = startlink_ret()
%%%
%%%	For details about Timing and Result see supervisor(3).
%%%
%%% @doc Starts the Arya application with the supervision timings provided
%%%
start( _, Timing ) ->
	error_logger:logfile( { open, "./log.txt"}),
	random:seed(),
	case arya_main_sup:start_link(Timing) of
		ignore ->
			{ error, ignore };
		Ret -> 
			Ret
	end.
	
%%% @spec stop( Reason ) -> Result
%%%		Reason = term(), ignored
%%%		Result = ok | {error, Reason}
%%%		Reason = term()
%%%
%%% @doc Stops the Arya application.
%%%
stop( _ ) ->
	error_logger:logfile(close),
	ok.
	
	