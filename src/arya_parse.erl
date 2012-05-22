-module(arya_parse).


%% Standard overproject types.
-include("arya_types.hrl").
-export([ parse/1]).


%% Dns header size.
-define( HEADER_SIZE, 12).

%%%-----------------------------------------------------------------
%%% This is Arya raw messages parsing routines module.
%%%-----------------------------------------------------------------

%%% @spec parse( Data) -> Result
%%%		Data = record( recv)
%%%		Result = { ok, Entry} 
%%%		 Entry = record( entry)
%%%	
%%% @doc Parses the packet, extracts its ID, Flags, binary question and 
%%% 	parses the question to get the type and the url array.
parse( Data) when is_record( Data, recv) ->
	report( 1, "Parsing started"),
	report( 3, "Parsing data", Data),
	
	Header = binary:part( Data#recv.data, 0, ?HEADER_SIZE),
	report( 2, "Parsing header", Header),
	ID = binary:part( Header, 0, 2),
	Flags = binary:part( Header, 2, 2),
	
	
	1 = case binary:decode_unsigned( binary:part( Header, 4, 2)) of % whether questions count is 1
		1 -> 
			1;
		Other ->
			report( 0, "Parsing: wrong question count", Other),
			Other
	end,
	
	Question = binary:part( Data#recv.data, byte_size(Data#recv.data), -(byte_size(Data#recv.data) - 12)),
	report( 2, "Parsing question", Question),
	
	{ ok, Url, Type} = parse_question( Question),
	Entry =	{ ok, 
				#entry {
					from = Data#recv.from,
					id = ID, 
					flags = Flags,
					type = Type,
					question = Question,
					url = Url
				}
			},
	report( 3, "Parsing entry", Entry).

%%% @doc Converts a binary question to a list of binary parts.
parse_question( Question ) ->
	BinUrl = binary:part( Question, 0, byte_size(Question)-4 ),
	Type = binary:decode_unsigned(
		binary:part( Question, byte_size(Question)-4, 2 )),
	Url = parse_url( BinUrl, []),
	report( 2, "Question parsed", { ok, Url, Type }).
	
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
	
