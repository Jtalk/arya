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

-module(jdb).

-export( [ appenv/3, getenv/2, report/2, report/3, configure/0]).

%%% ------------------------------------------------------------------------
%%% This is the debug module with debug leveling support. It uses standard
%%% Erlang error_logger module, supports both tty and file output.
%%% ------------------------------------------------------------------------

appenv( Name, Function, ErrorMessage) ->
	case application:get_env( Name) of
		{ ok, Value} ->
			Function(Value);
		Error ->
			report( 0, ErrorMessage, Error),
			Error
	end.
	
getenv( Name, ErrorMessage) ->
	appenv( Name, fun( Value) -> Value end, ErrorMessage).

report( Level, Report) ->
	{ ok,Default} = application:get_env( loglevel),
	if 
		Level == 0 ->
			error_logger:format( Report, self());
		Level =< Default ->
			error_logger:info_msg( Report, self());
		true ->
			false
	end.
	
report( Level, Report, Data) ->
	{ ok,Default} = application:get_env( loglevel),
	if 
		Level == 0 ->
			error_logger:format( Report, { self(), Data});
		Level =< Default ->
			error_logger:info_msg( Report, { self(), Data}),
			Data;
		true ->
			Data
	end.
		
configure() ->
	appenv( logfile, fun error_logger:logfile/1, "Unable to create config file"),
	appenv( is_tty, fun error_logger:tty/1, "Unable to read tty configuration").
	
	
	
	
	
	
	
	
	
	
	
	
	
	