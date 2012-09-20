%%------------------------------------------------------------------------------
%% Copyright 2012 FlowForwarding.org
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%-----------------------------------------------------------------------------

%% @author Erlang Solutions Ltd. <openflow@erlang-solutions.com>
%% @author Krzysztof Rutka <krzysztof.rutka@erlang-solutions.com>
%% @copyright 2012 FlowForwarding.org
%% @doc Encode/decode module for end-of-message framing mechanism.
-module(enetconf_fm_eom).

%% API
-export([encode/1,
         decode/1,
         decode_one/1,
         decode_n/2,
         new_parser/0,
         parse/2]).

-define(END, <<"]]>]]>">>).

-record(eom_parser, {
          stack = <<>> :: binary()
         }).

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

%% @doc Encode by encapsulating into end-of-message frame.
-spec encode(binary()) -> {ok, Encoded :: iolist()}.
encode(Message) ->
    {ok, [Message, ?END]}.

%% @doc Decode received data to separate messages.
-spec decode(binary()) -> {ok, Messages :: [binary()], Rest :: binary()}.
decode(Encoded) ->
    Splitted = re:split(Encoded, ?END),
    [Leftovers | Messages] = lists:reverse(Splitted),
    {ok, lists:reverse(Messages), Leftovers}.

%% @doc Decode first message from the received data.
-spec decode_one(binary()) -> {ok, Message :: [binary()], Rest :: binary()}.
decode_one(Encoded) ->
    Splitted = re:split(Encoded, ?END, [{parts, 2}]),
    [Leftovers | Messages] = lists:reverse(Splitted),
    {ok, lists:reverse(Messages), Leftovers}.

%% @doc Decode first N messages from the received data.
-spec decode_n(binary(), integer()) -> {ok, Messages :: [binary()],
                                        Rest :: binary()}.
decode_n(Encoded, N) ->
    Splitted = re:split(Encoded, ?END, [{parts, N + 1}]),
    [Leftovers | Messages] = lists:reverse(Splitted),
    {ok, lists:reverse(Messages), Leftovers}.

%% @doc Return new parser.
-spec new_parser() -> {ok, #eom_parser{}}.
new_parser() ->
    {ok, #eom_parser{}}.

%% @doc Parse received data.
%% Returns new parser.
-spec parse(binary(), #eom_parser{}) -> {ok, Messages :: [binary()],
                                         NewParser :: #eom_parser{}}.
parse(Encoded, #eom_parser{stack = Stack} = Parser) ->
    {ok, Messages, Leftovers} = decode([Stack, Encoded]),
    {ok, Messages, Parser#eom_parser{stack = Leftovers}}.
