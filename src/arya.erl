%%
%% Copyright (C) 2012-2013 Roman Nazarenko.
%%
%% This file is part of Arya.
%%
%% Arya is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Arya is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with Arya.  If not, see <http://www.gnu.org/licenses/>.

%% Author: Nazarenko Roman <mailto: me@jtalk.me>
%% License: <http://www.gnu.org/licenses/gpl.html>

%% @author Roman Nazarenko <me@jtalk.me>
%% @copyright 2012-2013 Roman Nazarenko
%% @doc This is the main Arya module describes callbacks for
%% application starting and terminating.

-module(arya).
-behaviour(application).

%% Debug:
-import(jdb, [report/3, appenv/3, ret/1]).

%% Callbacks:
-export([ start/2, stop/1]).

%%% @spec start(Type, Timing) -> Result
%%%     Type = term()
%%%     Timing = { MaxR :: integer(), MaxT :: integer() } 
%%%     Result = startlink_ret()
%%%
%%% @doc Starts the Arya application with the supervision timings provided
%%%
start( _, Timing ) ->
  jdb:configure(),
  case arya_main_sup:start_link(Timing) of
  ignore ->
    report( 0, "Unable to load Arya application", ignore),
    { error, ignore };
  Error = { error, _} ->
    report( 0, "Unable to load Arya application", Error);
  Ret -> 
    Ret
  end.
 
%%% @spec stop(Reason) -> Result
%%%     Reason = term()
%%%     Result = ok | {error, Reason}
%%%     Reason = term()
%%%
%%% @doc Stops the Arya application.
%%%
stop( _ ) ->
  error_logger:logfile(close),
  ok.
 
 