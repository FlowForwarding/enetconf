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
%% @doc Encode/decode module for chunked framing mechanism.
-module(enetconf_fm_chunked).

%% API
-export([encode/1,
         decode/1,
         decode_one/1,
         new_parser/0,
         parse/2]).

-define(LF, <<16#0A>>).
-define(HASH, <<16#23>>).
-define(CHUNK_SIZE, 1024).
-define(MIN_CHUNK_SIZE, 9).
-define(MAX_CHUNK_DATA_SIZE, 4294967295).

-record(chunk_parser, {
          stack = <<>> :: binary
         }).

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

%% @doc Encode by encapsulating into a chunked frame.
-spec encode(binary()) -> {ok, Encoded :: iolist()}.
encode(Message) ->
    do_encode(Message, []).

%% @doc Decode received data to separate messages.
-spec decode(binary()) -> {ok, Messages :: [binary()], Rest :: binary()}.
decode(Binary) ->
    do_decode(Binary, []).

%% @doc Decode first message from the received data.
-spec decode_one(binary()) -> {ok, Message :: [binary()], Rest :: binary()}.
decode_one(Binary) ->
    case decode_chunk(Binary, <<>>) of
        {ok, Chunk, Rest} ->
            {ok, [Chunk], Rest};
        {error, incomplete_chunk} ->
            {ok, [], Binary}
    end.

%% @doc Return new parser.
-spec new_parser() -> {ok, #chunk_parser{}}.
new_parser() ->
    {ok, #chunk_parser{}}.

%% @doc Parse received data.
%% Returns new parser.
-spec parse(binary(), #chunk_parser{}) -> {ok, Messages :: [binary()],
                                           NewParser :: #chunk_parser{}}.
parse(Binary, Parser) ->
    do_parse(Binary, Parser, []).

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

%% @private
do_encode(ChunkData, Encoded) when byte_size(ChunkData) < ?CHUNK_SIZE ->
    ChunkSize = integer_to_binary(byte_size(ChunkData)),
    Final = [?LF, ?HASH, ?HASH, ?LF,
             ChunkData, ?LF, ChunkSize, ?HASH, ?LF | Encoded],
    {ok, lists:reverse(Final)};
do_encode(<<ChunkData:?CHUNK_SIZE/binary, Rest/binary>>, Encoded) ->
    ChunkSize = integer_to_binary(byte_size(ChunkData)),
    do_encode(Rest, [ChunkData, ?LF, ChunkSize, ?HASH, ?LF | Encoded]).

%% @private
do_decode(Binary, DecodedChunks) ->
    case decode_chunk(Binary, <<>>) of
        {ok, Chunk, Rest} ->
            do_decode(Rest, [Chunk | DecodedChunks]);
        {error, incomplete_chunk} ->
            {ok, lists:reverse(DecodedChunks), Binary}
    end.

%% @private
decode_chunk(<<$\n, $#, $#, $\n, Rest/binary>>, Decoded) ->
    {ok, Decoded, Rest};
decode_chunk(<<$\n, $#, Chunk/binary>>, Decoded) ->
    case get_chunk_size(Chunk) of
        {ok, ChunkSize, ChunkRest} ->
            ChunkSizeInt = binary_to_integer(ChunkSize),
            case byte_size(ChunkRest) >= ChunkSizeInt of
                true ->
                    <<ChunkData:ChunkSizeInt/binary, Rest/binary>> = ChunkRest,
                    decode_chunk(Rest, <<Decoded/binary, ChunkData/binary>>);
                false ->
                    {error, incomplete_chunk}
            end;
        error ->
            {error, incomplete_chunk}
    end;
decode_chunk(_Binary, _) ->
    {error, incomplete_chunk}.

%% @private
do_parse(Binary, #chunk_parser{stack = Stack} = Parser, Chunks) ->
    ToDecode = <<Stack/binary, Binary/binary>>,
    case decode_chunk(ToDecode, <<>>) of
        {ok, Chunk, Rest} ->
            do_parse(Rest, Parser#chunk_parser{stack = <<>>}, [Chunk | Chunks]);
        {error, incomplete_chunk} ->
            {ok, lists:reverse(Chunks), Parser#chunk_parser{stack = ToDecode}}
    end.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

%% @private
integer_to_binary(N) ->
    list_to_binary(integer_to_list(N)).

binary_to_integer(Bin) ->
    list_to_integer(binary_to_list(Bin)).

%% @private
get_chunk_size(Binary) ->
    case re:split(Binary, <<$\n>>, [{parts, 2}]) of
        [<<>>, _] ->
            error;
        [_] ->
            error;
        [ChunkSize | Rest] ->
            {ok, ChunkSize, list_to_binary(Rest)}
    end.
