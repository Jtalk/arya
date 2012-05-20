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

-module(arya_common).

%% Debug.
-import(error_logger, [format/2]).
%% Standard overproject types.
-include("arya_types.hrl").

%% Dns standard header.
-define( HEADER, <<133, 0, 0, 1, 0, 1, 0, 0, 0, 0>> ).
%% Dns header size.
-define( HEADER_SIZE, 12).
%% Dns error header.
-define( ERROR_HEADER, <<133, 2, 0, 0, 0, 0, 0, 0, 0, 0>> ).
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
%% Response part length
-define( RESPONSE_LEN, 100).
%% HTTP port.
-define( PORT, 80).
%% Host identifier.
-define( HOST, "host").

%% Dunno Y I NO delete this.
-define( SPACE, $ ).
-define( SLASH, $/).
-define( NEWLINE, "\r\n").

-define( FIRST_ELEM, -1).
-define( SEPARATOR, $,).

-export([ parse/1, postprocess/1, process/2, download/1, send_back/2]).
-export([ request/3]).

	
%%% -----------------------------------------------------------------------
%%% This is the tier 1 and 2 Arya processors' special 
%%%	subroutines. To make processor module looks clearer 
%%% and quite easier to be looked after, I moved all the 
%%% "heavy" routines here.
%%% -----------------------------------------------------------------------

%% Initial parsing. -------------------------------------------------------

%%% @spec parse( Data) -> Result
%%%		Data = record( recv)
%%%		Result = { ok, Entry} 
%%%		 Entry = record( entry)
%%%	
%%% @doc Parses the packet, extracts its ID, Flags, binary question and 
%%% 	parses the question to get the type and the url array.
parse( Data) when is_record( Data, recv) ->
	Header = binary:part( Data#recv.data, 0, ?HEADER_SIZE),
	ID = binary:part( Header, 0, 2),
	Flags = binary:part( Header, 2, 2),
	1 = binary:decode_unsigned( 
		binary:part( Header, 4, 2)), % whether questions count is 1
	Question = binary:part( Data#recv.data, byte_size(Data#recv.data), -(byte_size(Data#recv.data) - 12)),
	{ ok, Url, Type} = parse_question( Question),
	{ ok, 
		#entry {
			from = Data#recv.from,
			id = ID, 
			flags = Flags,
			type = Type,
			question = Question,
			url = Url
		}
	}.

%%% @doc Converts a binary question to a list of binary parts.
parse_question( Question ) ->
	BinUrl = binary:part( Question, 0, byte_size(Question)-4 ),
	Type = binary:decode_unsigned(
		binary:part( Question, byte_size(Question)-4, 2 )),
	Url = parse_url( BinUrl, []),
	{ ok, Url, Type }.
	
%%% @doc Recursive url parsing routine.
parse_url( Url, List ) ->
	Len = binary:at( Url, 0),
	if 
		Len == 0 ->
			List;
		true ->
			Part = binary:part( Url, 1, Len),
			Rest = binary:part( Url, Len+1, byte_size(Url) - Len - 1 ),
			parse_url( Rest, [ Part | List])
	end.
	
%% Postprocessing routines. ------------------------------------------------
%%% @spec postprocess( Data) -> ok
%%%		Data = record( entry)
%%%	
%%% @doc Makes a decision on packet's future.
%%%
postprocess( Data) when is_record(Data, entry) ->
	case Data#entry.type of 
		?INIT -> 
			[ Uid, Passkey | Rest] = lists:reverse( Data#entry.url),
			Token = make_token( Uid, Passkey),
			decision( Token, Data#entry{ url = Rest});
		_ -> 
			[ Token | Rest] = lists:reverse( Data#entry.url),
			decision( Token, Data#entry{ url = Rest})
	end.
		
%%% @doc Puts a valid packet received to the downloader already run.
decision( Token, Data) ->
	{ ok, Pid} =	if Data#entry.type == ?INIT ->
						arya_dl_sup:child( Token);
					true->
						case arya_token_storage:get_pid( Token ) of
							Ok = { ok, _} -> Ok;
							false -> arya_dl_sup:child(Token)
						end
					end,
	case arya_dl:process( Pid, Data) of
		registered ->
			send_back( Data#entry.from, make_dns( Data, << ?TOKEN/binary, Token/binary>>));
		ok -> 
			send_back( Data#entry.from, make_dns( Data, ?DONE));
		{ need, Need} ->
			send_back( 
				Data#entry.from, 
				make_dns( Data, << ?NEED/binary, (need_to_bin( Need))/binary >>)
			);
		{ ready, Len} ->
			send_back( 
				Data#entry.from, 
				make_dns( Data, << ?READY/binary, (binary:list_to_bin(io_lib:format("~p", [Len])))/binary >> )
			);
		{ recv, Item} ->
			send_back( 
				Data#entry.from, 
				make_dns( Data, << ?DATA/binary, Item/binary >>)
			);
		last -> 
			format("Last", {}),
			send_back( 
				Data#entry.from, 
				make_dns( Data, ?DONE)
			),
			arya_dl:download( Pid);
		{ fail, Reason} ->
			% it's an attack and we should send no confirmation, just log it. Address will be logged 
			% as a part of Data record.
			format( "Fail in arya_processor:decision: ", { Reason, Token});
		{ exception, Error } -> 
			format( "Error in arya_processor:decision: ", Error),
			send_back(
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
	{ ok, MaxTok} = application:get_env(max_token),
	InToken = random:uniform( MaxTok),
	format( "TOKEN: ", InToken),
	binary:list_to_bin(io_lib:format( "~p", [InToken])).
	
%%% @doc Creates DNS packet from data specified.
make_dns( Data, String) -> 
	Header = << (Data#entry.id)/binary, ?HEADER/binary >>,
	Size = byte_size( String),
	Answer = << 16#c00c:16, (Data#entry.type):16, 1:16, 0:32, (Size + 1):16, Size, String/binary>>,
	<< Header/binary, (Data#entry.question)/binary, Answer/binary >>.	
	
%%% @doc Converts need array to binary representation.
need_to_bin( Need) ->
	need_to_bin( Need, <<>> ).
need_to_bin( [], <<>>) ->
	<<>>;
need_to_bin( [], Bin) ->
	binary:part( Bin, 0, byte_size( Bin) - 1);
need_to_bin( [ Need | Rest], Bin) ->
	Number = io_lib:format( "~p,", [Need]),
	need_to_bin( Rest, << Bin/binary, (binary:list_to_bin(Number))/binary >> ).
	
%% Sending to client routines ------------------------------------------------
%%% @spec send_back( To, Packet) -> ok
%%% 	To = { Address, Port}
%%%		 Address = inet:ip_address() | inet:hostname()
%%% 	 Port = inet:port_number()
%%%		Packet = { id, ID} | binary()
%%%		 ID = binary()
%%%
%%%	@doc Sends a message specified to the Address and Port using arya_server.
%%%
send_back( To, { id, ID }) ->
	arya_server:send( 	#send { 
							to = To, 
							data = << ID/binary, ?ERROR_HEADER/binary >> 
						}
	);
send_back( To, Packet) ->
	arya_server:send( #send { to = To, data = Packet}).
			

%% Proceed with downloader messages -----------------------------------------
%%% @spec process( Data, State) -> Reply
%%%		Data = record(entry)
%%%		State = record(state)
%%%		Reply = { reply, Message, NewState}
%%%		 Message = 	last 
%%%					| ok 
%%%					| { need, Need} 
%%%					| { ready, Num}
%%%					| { recv, Part}
%%%					| { fail, Reason}
%%%		  Need = [ integer()>0 ]
%%%		  Num = integer() > 0
%%%		  Part = binary()
%%%		  Reason = term()
%%%
%%% @doc Processes the message provided, downloads data when needed and returns it back to caller.
%%%
process( Data, State) when Data#entry.type == ?INIT ->
	[ BinQuantity | _] = Data#entry.url,
	{ Quantity, _} = string:to_integer( binary:bin_to_list( BinQuantity)), 
	format( "REGISTRATION: ", { Quantity, Data#entry.from}),
	{ reply, registered, State#state { 
					url_len = Quantity, 
					address = Data#entry.from
				}
	};
	% no need in exception handling since 	
	% future processing is not allowed without len
process( Data, State) when State#state.url_len == 0 ->
	format( "URL_LEN: ", State#state.url_len),
	{ reply, { fail, { index_quantity_undefined, Data, State}}, State};
process( Data, State) when Data#entry.type == ?SEND ->
	try part( Data, length( State#state.url), State#state.url_len) of
		{ ok, Part } ->
			{ reply, ok, State#state { url = [ Part | State#state.url ]}};
		{ last, Part } ->
			Url = lists:usort( fun compare/2, [ Part | State#state.url]), % sort with ununique values deletion
			if length( Url) < State#state.url_len ->
				Need = need( Url),
				{ reply, { need, Need}, State#state { url = Url}};
			true ->
				{ reply, last, State#state{ url = Url}}
			end
	catch 
		error:Error ->
			{ reply, { exception, { Error, Data, State}}, State}
	end;
process( Data, State) when Data#entry.type == ?STATUS ->
	% format( "STATUS", State),
	[ Ans | _] = Data#entry.url,
	if % need no try since we're working with no external data
		Ans == ?DONE ->
			{ stop, normal, ok, State};
		length(State#state.url) < State#state.url_len ->
			Need = need( State#state.url),
			{ reply, { need, Need}, State};
		State#state.data_len == 0 ->
			{ reply, last, State };
		true ->
			{ reply, { ready, State#state.data_len}, State}
	end;
process( Data, State) when Data#entry.type == ?RECV ->
	if % need no try since we're working with no external data
		length(State#state.url) < State#state.url_len ->
			Need = need( State#state.url),
			{ reply, { need, Need}, State};
		State#state.data_len == 0 ->
			{ reply, last, State};
		true ->
			try 
				[ BinNumber | _] = Data#entry.url,
				{ Number, _} = string:to_integer(binary:bin_to_list( BinNumber)),
				Item = lists:nth( Number+1, State#state.data),
				{ reply, { recv, Item}, State}
			catch 
				error:Error -> 
					{ reply, { exception, { Error}}, State}
			end
	end;
process( Data, State) ->
	{ reply, { fail, { unauthorized, Data, State}}, State}.
	
%%% @doc Generates a list of lost packets.
need( Url) ->
	need( lists:usort( fun compare/2, Url), [], -1).
need( [], List, _Prev) ->
	List;
need( [ { Index, _} | Rest], List, Prev) ->
	NewList = 
		if Index == (Prev + 1) -> 
			List;
		true ->
			[ Index | List]
		end,
	need( Rest, NewList, Index).
	
%%% @doc Gets a data from a packet and looks whether this packet is last
part( Raw, ActualLen, NeededLen) ->
 	[ BinIndex, Data | _] = Raw#entry.url,
 	Ret = if ActualLen+1 >= NeededLen ->
			last;
		true ->
			ok
	end,
	{ Index, _} = string:to_integer( binary:bin_to_list( BinIndex)),
	{ Ret, { Index, Data}}.

%%% @doc Comparsion routine for usort.
compare( { Index1, _}, { Index2, _}) when Index1 > Index2 ->
	false;
compare( { _Index1, _}, { _Index2, _}) ->
	true.
	
%% Downloader ----------------------------------------------------------------------
%%% @doc Perform all remote page downloading routines.
download( State) when is_record( State, state) ->
	Data = construct( State#state.url, <<>>),
	{ ok, Pure} = decrypt( Data),
	{ ok, Address} = address( Pure),
	{ ok, Packet} = request( Address, ?PORT, Pure),
	%% format( "PACKET: ", Packet),
	{ _Parts, _Len} = split( Packet, ?RESPONSE_LEN).
	
%%% @doc Constructs a packet from the list provided.
construct( [], Ready) ->
	Ready;
construct( [ {_, Part} | Rest], Ready) ->
	New = << Ready/binary, Part/binary >>,
	construct( Rest, New).
	
%%% @doc Incoming message decryption.
decrypt( Data ) ->
	Replaced1 = binary:replace( Data, << $- >>, << $+ >>),
	Replaced2 = binary:replace( Replaced1, << $_ >>, << $/ >>),
	{ ok, decode( Replaced2, 3)}.
	
%%% @doc Decodes the binary given, tries to add N parts sequentially until 
%%% 	data becomes decodable, falls if not.
decode( Data, -1) ->
	error( {badarg, Data});
decode( Data, Num) ->
	case catch base64:decode_to_string( Data) of
		Str when is_list( Str) ->
			Str;
		_ -> 
			decode( << Data/binary, $= >>, Num-1)
	end.
	
%%% @doc Returns remote host's address extracted from a Data.
address( Request) -> % needs future research whether it even works.
	Lower = string:to_lower(Request),
	WithBlanks = 
		case string:str( Lower, ?HOST) of
			0 -> % No "host:" property in the request 
				Pos = string:chr( Lower, ?SPACE),
				Len = string:chr( Lower, ?SLASH) - Pos,
				string:substr( Lower, Pos+1, Len);
			Pos ->
				Len = string:str( 
					string:substr( Lower, Pos+string:len(?HOST)+1, string:len( Lower) - Pos), 
					?NEWLINE
				),
				string:substr( Lower, Pos+string:len(?HOST)+1, Len-1)
		end,
	{ ok, string:strip( WithBlanks)}.
	
%%% @doc Makes a request to the remote server specified.
request( Address, Port, Data ) ->
	{ ok, Socket} = gen_tcp:connect( Address, Port, [ {active, false}, binary ]),
	ok = gen_tcp:send( Socket, Data),
	{ ok, _Packet} = recv( Socket, <<>>).
recv( Socket, Data) ->
	{ ok, Timeout} = application:get_env( timeout),
	case gen_tcp:recv( Socket, 0, Timeout) of 
		{ ok, Packet} ->
			recv( Socket, << Data/binary, Packet/binary >>);
		{ error, _} ->
			gen_tcp:close( Socket),
			{ ok, Data}
	end.
	
%%% @doc Splits a packet to parts not greater than lenght specified.
split( Packet, MaxLen) when is_binary( Packet), is_integer( MaxLen) ->
	{ Parts, Len} = split( Packet, MaxLen, []),
	%% format( "PARTS: ", Parts),
	{ lists:reverse(Parts), Len}.
split( <<>>, _Len, Array) ->
	{ Array, length( Array)};
split( Data, Len, Array) ->
	{ Part, Rest} = 
		try
			{ binary:part( Data, 0, Len),
			binary:part( Data, Len, byte_size( Data) - Len) }
		catch
			error:badarg -> 
				{ Data, <<>> }
		end,
	split( Rest, Len, [ Part | Array]).	
	
	