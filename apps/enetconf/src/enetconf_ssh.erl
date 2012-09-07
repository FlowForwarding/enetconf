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
%% @copyright 2012 FlowForwarding.org
%% @doc SSH Netconf backend module.
-module(enetconf_ssh).

-behaviour(ssh_channel).

-include_lib("enetconf/include/enetconf.hrl").

%% API
-export([]).

%% ssh_channel callbacks
-export([init/1,
         handle_msg/2,
         handle_ssh_msg/2,
         terminate/2]).

-type expected_chunk() :: first_chunk_lf
                        | first_chunk_header_len
                        | body
                        | next_chunk_lf
                        | header_len_or_end_of_chunks
                        | last_chunk.

-record(body, {
          total_size    = 0  :: integer(),
          received_size = 0  :: integer(),
          payload       = "" :: string()}).

-record(state, {
          channel_id      :: ssh_channel:channel_id(),
          connection_ref  :: sh_channel:connection_ref(),
          expected_chunk  :: expected_chunk(),
          body = #body{}  :: #body{}
         }).

-define(DATA_TYPE_CODE, 0).

init(_) ->
    {ok, #state{expected_chunk = first_chunk_lf}}.

handle_msg({ssh_channel_up, ChannelId, ConnRef}, State) ->
    ssh_connection:send(ConnRef, ChannelId, get_server_capabilities()),
    {ok, State#state{channel_id = ChannelId,
                     connection_ref= ConnRef}};
handle_msg({'EXIT', Reason}, #state{channel_id = ChannelId} = State) ->
    ?INFO("SSH channel ~p exited with reason : ~p",
          [ChannelId, Reason]),
    {stop, ChannelId, State}.

handle_ssh_msg({ssh_cm, ConnRef, {data, ChannelId,
                                  ?DATA_TYPE_CODE,  Data}},
               #state{connection_ref= ConnRef, channel_id = ChannelId,
                      body = Body} = State) ->
    case decode_chunked_framing(State#state.expected_chunk,
                                binary_to_list(Data),
                                Body) of
        {continue, NextChunk} ->
            {ok, State#state{expected_chunk = NextChunk}};
        {continue, body, NewBody} ->
            {ok, State#state{expected_chunk = body, body = NewBody}};
        {continue, next_chunk_lf, NewBody} ->
            ?INFO("@@@@@@ Full body: ~p", [NewBody#body.payload]),
            %% ssh_connection:send(ConnRef, ChannelId, list_to_binary(NewBody#body.payload)),
            {ok, State#state{expected_chunk = next_chunk_lf, body = #body{}}};
        {stop, last_chunk} ->
            ssh_connection:send(ConnRef, ChannelId, <<"\nBYE.\n">>),
            {stop, ChannelId, State}
    end;
handle_ssh_msg({ssh_cm, ConnRef, {eof, ChannelId}},
               #state{connection_ref= ConnRef, channel_id = ChannelId} = State) ->
    {ok, State};
handle_ssh_msg({ssh_cm, ConnRef, {signal, ChannelId, _Signal}},
               #state{connection_ref= ConnRef, channel_id = ChannelId} = State) ->
    {ok, State};
handle_ssh_msg({ssh_cm, ConnRef, {exit_signal, ChannelId,
                                  _ExitSignal, _ErrorMsg, _LanguageString}},
               #state{connection_ref= ConnRef, channel_id = ChannelId} = State) ->
    {ok, State};
handle_ssh_msg({ssh_cm, ConnRef, {exit_status, ChannelId, _ExitStatus}},
               #state{connection_ref= ConnRef, channel_id = ChannelId} = State) ->
    {ok, State}.

terminate(Subsystem, State) ->
    ?INFO("SSH connection with subsystem: ~p terminated with state: ~p",
          [Subsystem, State]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_server_capabilities() ->
    <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<hello xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">
  <capabilities>
    <capability>
      urn:ietf:params:netconf:base:1.1
    </capability>
    <capability>
      urn:ietf:params:ns:netconf:capability:startup:1.0
    </capability>
  </capabilities>
  <session-id>4</session-id>
</hello>
]]>]]>
">>.

decode_chunked_framing(first_chunk_lf, "\n", _Body) ->
    {continue, first_chunk_header_len};
decode_chunked_framing(first_chunk_header_len, [$# | ChunkSizeAndNewline], _Body) ->
    [$\n | ReversedChunkSize] = lists:reverse(ChunkSizeAndNewline),
    ChunkSize = lists:reverse(ReversedChunkSize),
    case list_to_integer(ChunkSize) of
        BodySize ->
            {continue, body, #body{total_size = BodySize}}
    end;
decode_chunked_framing(body, NewPayload, #body{total_size = TotalSize,
                                               received_size = ReceivedSize,
                                               payload = Payload} = Body) ->
    TotalReceived = ReceivedSize + length(NewPayload),
    if
        TotalReceived < TotalSize ->
            {continue, body, Body#body{received_size = TotalReceived,
                                       payload = Payload ++ NewPayload}};
        TotalReceived == TotalSize ->
            {continue, next_chunk_lf, Body#body{received_size = TotalReceived,
                                                payload = Payload ++ NewPayload}};
        TotalReceived > TotalSize ->
            {error, body_too_big}
    end;
decode_chunked_framing(next_chunk_lf, "\n", _Body) ->
    {continue, header_len_or_end_of_chunks};
decode_chunked_framing(header_len_or_end_of_chunks, "##\n", _Body) ->
    {stop, last_chunk};
decode_chunked_framing(header_len_or_end_of_chunks,
                       [$# | ChunkSizeAndNewline], _Body) ->
    [$\n | ReversedChunkSize] = lists:reverse(ChunkSizeAndNewline),
    ChunkSize = lists:reverse(ReversedChunkSize),
    case list_to_integer(ChunkSize) of
        BodySize ->
            {continue, body, #body{total_size = BodySize}}
    end.
