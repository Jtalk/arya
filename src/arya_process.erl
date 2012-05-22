-module( arya_process).

%% Standard overproject types.
-include("arya_types.hrl").
-export([process/2]).


-define( DONE, << "done">> ).
%% Extra messages needed:

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
	report( 1, "Starting registering in Downloader"),
	
	[ BinQuantity | _] = Data#entry.url,
	{ Quantity, _} = string:to_integer( binary:bin_to_list( BinQuantity)), 
	report( 1, "Registration in Downloader"),
	report( 2, "Registration in Downloader", Quantity),
	
	{ reply, registered, State#state { 
					url_len = Quantity, 
					address = Data#entry.from
				}
	};
	% no need in exception handling since 	
	% future processing is not allowed without len
process( Data, State) when State#state.url_len == 0 ->
	report( 0, "Url lenght undefined in Downloader", State),
	{ reply, { fail, { index_quantity_undefined, Data, State}}, State};
process( Data, State) when Data#entry.type == ?SEND ->
	report( 1, "SEND data received in Downloader"),
	report( 3, "Data", { self(), Data, State}),
	
	try part( Data, length( State#state.url), State#state.url_len) of
		{ ok, Part } ->
			report( 1, "Part is not last"),
			report( 2, "Part", Part),
			
			{ reply, ok, State#state { url = [ Part | State#state.url ]}};
		{ last, Part } ->
			report( 1, "Part is last"),
			report( 2, "Part", Part),
			
			Url = lists:usort( fun compare/2, [ Part | State#state.url]), % sort with ununique values deletion
			report( 3, "Sorted items", Url),
			
			if length( Url) < State#state.url_len ->
				report( 1, "Not all"),
				Need = need( Url),
				{ reply, { need, Need}, State#state { url = Url}};
			true ->
				report( 1, "All has been received"),
				{ reply, last, State#state{ url = Url}}
			end
	catch 
		error:Error ->
			{ reply, { exception, { Error, Data, State}}, State}
	end;
process( Data, State) when Data#entry.type == ?STATUS ->
	report( 1, "STATUS data received in Downloader"),
	report( 3, "Data", { Data, State}),
	
	[ Ans | _] = Data#entry.url,
	if % need no try since we're working with no external data
		Ans == ?DONE ->
			report( 1, "DONE status received, Downloader is going to shutdown now"),
			report( 2, "From", Data#entry.from),
			report( 3, "Data", Data),
			
			{ stop, normal, ok, State};
		length(State#state.url) < State#state.url_len ->
			report( 1, "Not all parts are received, resend needed"),
			report( 2, "Lengthes", { length(State#state.url), State#state.url_len}),
			
			Need = need( State#state.url),
			{ reply, { need, Need}, State};
		State#state.data_len == 0 ->
			report( 1, "Data has not downloaded yet in Downloader"),
			
			{ reply, last, State };
		true ->
			report( 1, "All is ready to be sent"),

			{ reply, { ready, State#state.data_len}, State}
	end;
process( Data, State) when Data#entry.type == ?RECV ->
	report( 1, "RECV data received in Downloader"),
	report( 3, "Data", { Data, State}),
	
	if % need no try since we're working with no external data
		length(State#state.url) < State#state.url_len ->
			report( 1, "Not all parts are received, resend needed"),
			report( 2, "Lengthes", { length(State#state.url), State#state.url_len}),
			
			Need = need( State#state.url),
			{ reply, { need, Need}, State};
		State#state.data_len == 0 ->
			report( 1, "Data has not downloaded yet in Downloader"),
			
			{ reply, last, State};
		true ->
			report( 1, "All is ready to be sent"),
			try 
				[ BinNumber | _] = Data#entry.url,
				{ Number, _} = string:to_integer(binary:bin_to_list( BinNumber)),
				report( 2, "Number of parts", Number),
				
				Item = lists:nth( Number+1, State#state.data),
				report( 3, "Nth element", Item),
				
				{ reply, { recv, Item}, State}
			catch 
				error:Error -> 
					{ reply, { exception, { Error}}, State}
			end
	end;
process( Data, State) ->
	report( 0, "Wrong data is received in Downloader"),
	{ reply, { fail, { unauthorized, Data, State}}, State}.
	
%%% @doc Generates a list of lost packets.
need( Url) ->
	report( 2, "Sorted need", need( lists:usort( fun compare/2, Url), [], -1)).
need( [], List, _Prev) ->
	report( 2, "Need is", List),
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