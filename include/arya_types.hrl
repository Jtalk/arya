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

-import(jdb, [report/3, report/2, appenv/3, getenv/2]).

%% DNS Protocol entry types.
-define(INIT, 256).
-define(SEND, 257).
-define(STATUS, 258).
-define(RECV, 259).

%%% Message sent between processes, it contains source address 
%%% and port and the raw data of the incoming message.
-record( recv, 
    { 
      from :: {inet:ip_address(), inet:port_number()},
      data :: binary()
    }
  ).
    
%%% Message sent between processes, it contains destination address 
%%% and port and the raw data of the message to send to the client.
-record( send, 
    { 
      to,   % = { address, port}
      data  % = binary()
    }
  ).
    
%%% The UDP message received via an active gen_udp socket.
-record( udp, 
    { 
      socket,  % = socket()
      address,% = inet:ip_address() | inet:hostname()
      port,   %  = inet:port_number()
      data  % = binary()
    }
  ).
    
%%% An entry send between Tier 1 processes, it contains source address,
%%% incoming request ID, type of the message received, original 
%%% question in binary form and an array of binary urls.
-record( entry, 
    {
      from     :: {inet:ip_address(), inet:port_number()},
      id       :: binary(), 
      flags    :: binary(), 
      type     :: integer(), 
      question :: binary(),
      url      :: [binary()]
    }
  ).
    
    
%%%  The internal state of a downloader.It contains source of the 
%%% initial message, token value of this session and all the down-
%%% loaded content in binary form.
-record( state, 
    {
      address      :: {inet:ip_address(), inet:port_number()}, 
      token        :: binary(),
      url_len = 0  :: integer(),
      url = []     :: [{integer(), binary()}], % - size and binary representation
      data_len = 0 :: integer(),
      data         :: [{integer(), binary()}]
    }
  ).
