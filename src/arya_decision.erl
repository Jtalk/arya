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

-module(arya_decision).


%% Standard overproject types.
-include("arya_types.hrl").
-export([postprocess/1]).
	

%% Dns standard header.
-define( HEADER, <<133, 0, 0, 1, 0, 1, 0, 0, 0, 0>> ).
%% Token has been generated:
-define( TOKEN, << "token=">>).
%% Message has been processed: 
-define( DONE, << "done">> ).
%% Extra messages needed:
-define( NEED, << "need=" >>).
%% Error:
-define( ERROR, << "error=" >>).
%% Number of ready parts:
-define( READY, << "ready=" >>).
%% Data to send back:
-define( DATA, << "data=" >>).
	
%% Postprocessing routines. ------------------------------------------------
%%% @spec postprocess( Data) -> ok
%%%		Data = record( entry)
%%%	
%%% @doc Gets token and starts making a decision.
%%%
postprocess( Data) when is_record(Data, entry) ->
	case Data#entry.type of 
		?INIT -> 
			[ Uid, Passkey | Rest] = lists:reverse( Data#entry.url),
			Token = make_token( Uid, Passkey),
			report( 2, "UID, Passkey, Token", { Uid, Passkey, Token}),
			
			decision( Token, Data#entry{ url = Rest});
		_ -> 
			[ Token | Rest] = lists:reverse( Data#entry.url),
			report( 1, "Token got"),
			report( 2, "Token", Token),
			
			decision( Token, Data#entry{ url = Rest})
	end.

%%% @doc Puts a valid packet received to the downloader already run.
decision( Token, Data) ->
	report( 1, "Starting making a decision"),
	report( 2, "Decision token", Token),
	report( 3, "Decision data", Data),
	
	{ ok, Pid} =	if Data#entry.type == ?INIT ->
						arya_dl_sup:child( Token);
					true->
						arya_dl_sup:child(Token)
					end,
	report( 1, "PID is got"),
	report( 2, "PID", Pid),
	
	case arya_dl:process( Pid, Data) of
		registered ->
			report( 1, "Registered"),
			arya_common:send_back( Data#entry.from, make_dns( Data, << ?TOKEN/binary, Token/binary>>));
		ok -> 
			report( 1, "Done"),
			arya_common:send_back( Data#entry.from, make_dns( Data, ?DONE));
		{ need, Need} ->
			report( 1, "Need"),
			report( 2, "Need value", Need),
			arya_common:send_back( 
				Data#entry.from, 
				make_dns( Data, << ?NEED/binary, (need_to_bin( Need))/binary >>)
			);
		{ ready, Len} ->
			report( 1, "Ready"),
			report( 2, "Ready lenght", Len),
			arya_common:send_back( 
				Data#entry.from, 
				make_dns( Data, << ?READY/binary, (binary:list_to_bin(io_lib:format("~p", [Len])))/binary >> )
			);
		{ recv, Item} ->
			report( 1, "Receiving"),
			report( 2, "Receiving item", Item),
			arya_common:send_back( 
				Data#entry.from, 
				make_dns( Data, << ?DATA/binary, Item/binary >>)
			);
		last -> 
			report( 1, "Last"),
			arya_common:send_back( 
				Data#entry.from, 
				make_dns( Data, ?DONE)
			),
			arya_dl:download( Pid);
		{ fail, Reason} ->
			% it's an attack and we should send no confirmation, just log it. Address will be logged 
			% as a part of Data record.
			report( 0, "Fail in arya_processor:decision", { Reason, Token});
		{ exception, Error } -> 
			report( 0, "Exception in arya_processor:decision", { Error, Token}),
			arya_common:send_back(
				Data#entry.from,
				make_dns( Data, << ?ERROR/binary, "Server error" >> )
			)
	end.
				
%%% @doc Creates a new binary token from the uid and pass presented. 
%%%
%%% @note Really just returns a random value now.
%%%
make_token( _Uid, _Pass) -> 
	random:seed( now()),
	MaxTok = getenv(max_token, "Unable to get maximum token value"),
	InToken = random:uniform( MaxTok),
	report( 1, "Token made"),
	report( 2, "Token value", InToken),
	binary:list_to_bin(io_lib:format( "~p", [InToken])).
	
%%% @doc Creates DNS packet from data specified.
make_dns( Data, String) -> 
	report( 1, "Making DNS packet"),
	report( 2, "Return data", String),
	report( 3, "Data", Data),
	
	Header = << (Data#entry.id)/binary, ?HEADER/binary >>,	
	Size = byte_size( String),
	Answer = << 16#c00c:16, (Data#entry.type):16, 1:16, 0:32, (Size + 1):16, Size, String/binary>>,
	report( 2, "Header, Size, Answer", { Header, Size, Answer}),
	
	Packet = << Header/binary, (Data#entry.question)/binary, Answer/binary >>,
	report( 2, "DNS Packet", Packet),
	Packet.
	
%%% @doc Converts need array to binary representation.
need_to_bin( Need) ->
	Bin = need_to_bin( Need, <<>> ), 
	report( 2, "Binary need", Bin),
	Bin.
need_to_bin( [], <<>>) ->
	<<>>;
need_to_bin( [], Bin) ->
	binary:part( Bin, 0, byte_size( Bin) - 1);
need_to_bin( [ Need | Rest], Bin) ->
	Number = io_lib:format( "~p,", [Need]),
	need_to_bin( Rest, << Bin/binary, (binary:list_to_bin(Number))/binary >> ).