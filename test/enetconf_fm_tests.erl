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
%% @doc eUnit suite for NETCONF's framing mechanisms.
%% @private
-module(enetconf_fm_tests).

-include_lib("eunit/include/eunit.hrl").

%% Tests -----------------------------------------------------------------------

fm_test_() ->
    [[{"Encode/decode a single message using" ++ fm_name(Module),
       {with, Module, [fun encode_decode/1]}},
      {"Encode/decode an empty message using" ++ fm_name(Module),
       {with, Module, [fun encode_decode_empty/1]}},
      {"Encode few messages and parse them using" ++ fm_name(Module),
       {with, Module, [fun encode_parse/1]}},
      {"Parse messages split into random chunks using" ++ fm_name(Module),
       {with, Module, [fun parsing_chunks/1]}}]
     || Module <- [enetconf_fm_eom, enetconf_fm_chunked]].

encode_decode(Module) ->
    Message = <<"test">>,
    ?assertEqual(Message, encode_decode(Module, Message)).

encode_decode_empty(Module) ->
    Message = <<>>,
    ?assertEqual(Message, encode_decode(Module, Message)).

encode_parse(Module) ->
    Messages = [<<"ala">>, <<"ma">>, <<>>, <<"kota">>],
    Encoded = [Module:encode(M) || M <- Messages],
    Binary = list_to_binary([E || {ok, E} <- Encoded]),
    {ok, Parser} = Module:new_parser(),
    {ok, Decoded, _} = Module:parse(Binary, Parser),
    ?assertEqual(Messages, Decoded).

parsing_chunks(enetconf_fm_eom = Module) ->
    Binaries = [<<"al">>, <<"a]">>, <<"]>">>, <<"]]">>, <<">m">>, <<"a]]">>,
                <<">]]">>, <<">ko">>, <<"ta]]>">>, <<"]">>, <<"]">>, <<">">>],
    parse_those_chunks(Module, Binaries);
parsing_chunks(enetconf_fm_chunked = Module) ->
    Binaries = [<<"\n#">>, <<"1\na">>, <<"\n#2\nla">>, <<"\n##\n">>,
                <<"\n#2\nma">>, <<"\n##\n\n">>, <<"#3\nkot">>,
                <<"\n#1\n">>, <<"a">>, <<"\n#">>, <<"#">>, <<"\n">>],
    parse_those_chunks(Module, Binaries).

%% Helper functions ------------------------------------------------------------

encode_decode(Module, Message) ->
    {ok, Encoded} = Module:encode(Message),
    {ok, [Decoded], _} = Module:decode_one(list_to_binary(Encoded)),
    Decoded.

parse_those_chunks(Module, Binaries) ->
    Messages = [<<"ala">>, <<"ma">>, <<"kota">>],
    {ok, InitParser} = Module:new_parser(),
    Fun = fun(Binary, {Parser, DecodedMsgs}) ->
                  {ok, NewMsgs, NewParser} = Module:parse(Binary, Parser),
                  {NewParser, DecodedMsgs ++ NewMsgs}
          end,
    {_, Decoded} = lists:foldl(Fun, {InitParser, []}, Binaries),
    ?assertEqual(Messages, Decoded).

fm_name(enetconf_fm_eom) ->
    " End-of-Message Framing Mechanism";
fm_name(enetconf_fm_chunked) ->
    " Chunked Framing Mechanism".
