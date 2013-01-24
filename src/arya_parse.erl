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

%% @author Roman Nazarenko <me@jtalk.me>
%% @copyright 2012-2013 Roman Nazarenko
%% @doc This is Arya raw DNS messages parsing routines module.

-module(arya_parse).
-include("arya_types.hrl").
-export([ parse/1]).

%% Dns header size.
-define(HEADER_SIZE, 12).

%%% @spec parse(Data) -> Result
%%%    Data = record(recv)
%%%    Result = {ok, Entry} 
%%%     Entry = record(entry)
%%%  
%%% @doc Parses the packet, extracts its ID, Flags, binary question and 
%%% parses the question to get the type and the url array.
parse(Data) when is_record(Data, recv) ->
  report(1, "Parsing started"),
  report(3, "Parsing data", Data),
  Header = binary:part(Data#recv.data, 0, ?HEADER_SIZE),
  report(2, "Parsing header", Header),
  ID = binary:part(Header, 0, 2),
  Flags = binary:part(Header, 2, 2),
  1 = case binary:decode_unsigned(binary:part(Header, 4, 2)) of % whether questions count is 1
    1 -> 
      1;
    Other ->
      report(0, "Parsing: wrong question count", Other),
      Other %% Exception would be thrown here.
  end,
  Question = binary:part(Data#recv.data, byte_size(Data#recv.data), -(byte_size(Data#recv.data) - 12)),
  report(2, "Parsing question", Question),
  {ok, Url, Type} = parse_question(Question),
  Entry =  {ok, 
    #entry {
      from = Data#recv.from,
      id = ID, 
      flags = Flags,
      type = Type,
      question = Question,
      url = Url
    }
  },
  report(3, "Parsing entry", Entry).

%%% @doc Converts a binary question to a list of binary parts.
parse_question(Question ) ->
  BinUrl = binary:part(Question, 0, byte_size(Question)-4 ),
  Type = binary:decode_unsigned(
    binary:part(Question, byte_size(Question)-4, 2 )),
  Url = parse_url(BinUrl, []),
  report(2, "Question parsed", {ok, Url, Type }).
  
%%% @doc Recursive url parsing routine.
parse_url(Url, List) ->
  Len = binary:at(Url, 0),
  if 
    Len == 0 ->
      List;
    true ->
      Part = binary:part(Url, 1, Len),
      Rest = binary:part(Url, Len+1, byte_size(Url) - Len - 1 ),
      parse_url(Rest, [ Part | List])
  end.
  
