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
%% @author Konrad Kaplita <konrad.kaplita@erlang-solutions.com>
%% @author Krzysztof Rutka <krzysztof.rutka@erlang-solutions.com>
%% @copyright 2012 FlowForwarding.org
%% @doc SSH Netconf backend module.
-module(enetconf_ssh).

-behaviour(ssh_channel).

-include("enetconf.hrl").

%% API
-export([]).

%% ssh_channel callbacks
-export([init/1,
         handle_msg/2,
         handle_ssh_msg/2,
         terminate/2]).

-record(state, {
          channel_id     :: ssh_channel:channel_id(),
          connection_ref :: ssh_channel:connection_ref(),
          parsing_module :: enetconf_frame_end
                          | enetconf_frame_chunk,
          parser         :: record(),
          callbacks = [] :: [{atom(), atom()}]
         }).

-define(DATA_TYPE_CODE, 0).

%%-----------------------------------------------------------------------------
%% ssh_channel callbacks
%%-----------------------------------------------------------------------------

%% @private
init(_) ->
    {ok, Callbacks} = application:get_env(enetconf, callbacks),
    {ok, #state{callbacks = Callbacks}}.

%% @private
handle_msg({ssh_channel_up, ChannelId, ConnRef}, State) ->
    Capabilities = get_server_capabilities(get_session_id()),
    {ok, EncodedCaps} = enetconf_fm_eom:encode(Capabilities),
    ssh_connection:send(ConnRef, ChannelId, EncodedCaps),
    {ok, State#state{connection_ref= ConnRef,
                     channel_id = ChannelId}};
handle_msg({'EXIT', Reason}, #state{channel_id = ChannelId} = State) ->
    ?INFO("SSH channel ~p exited with reason: ~p", [ChannelId, Reason]),
    {stop, ChannelId, State}.

%% @private
handle_ssh_msg({ssh_cm, ConnRef, {data, ChannelId, ?DATA_TYPE_CODE,  Data}},
               #state{connection_ref = ConnRef,
                      channel_id = ChannelId,
                      parsing_module = undefined,
                      parser = undefined} = State) ->
    %% Decode first received XML
    case enetconf_fm_eom:decode_one(Data) of
        {ok, [FirstMessage], Rest} ->
            %% Parse the XML to check if it's a friendly hello
            case enetconf_parser:parse(binary_to_list(FirstMessage)) of
                {ok, #hello{} = Hello} ->
                    %% Decide on version, choose framing mechanism module
                    case get_parser_module(Hello) of
                        {error, bad_version} ->
                            %% TODO: Send an error
                            {stop, ChannelId, State};
                        Module ->
                            {ok, Parser} = Module:new_parser(),
                            NewParser = handle_messages(Module, Parser, Rest),
                            {ok, State#state{parsing_module = Module,
                                             parser = NewParser}}
                    end;
                {error, _} ->
                    %% TODO: Send an error
                    ?WARNING("Invalid hello: ~p~n", [FirstMessage]),
                    {stop, ChannelId, State}
            end;
        {ok, [], BadHello} ->
            %% TODO: Send an error?
            ?WARNING("Invalid hello: ~p~n", [BadHello]),
            {stop, ChannelId, State}
    end;
handle_ssh_msg({ssh_cm, ConnRef, {data, ChannelId, ?DATA_TYPE_CODE,  Data}},
               #state{connection_ref= ConnRef, channel_id = ChannelId,
                      parsing_module = Module, parser = Parser} = State) ->
    NewParser = handle_messages(Module, Parser, Data),
    {ok, State#state{parser = NewParser}};
handle_ssh_msg({ssh_cm, ConnRef, {eof, ChannelId}},
               #state{connection_ref = ConnRef,
                      channel_id = ChannelId} = State) ->
    {ok, State};
handle_ssh_msg({ssh_cm, ConnRef, {signal, ChannelId, _Signal}},
               #state{connection_ref = ConnRef,
                      channel_id = ChannelId} = State) ->
    {ok, State};
handle_ssh_msg({ssh_cm, ConnRef, {exit_signal, ChannelId, _ExitSignal,
                                  _ErrorMsg, _LanguageString}},
               #state{connection_ref = ConnRef,
                      channel_id = ChannelId} = State) ->
    {ok, State};
handle_ssh_msg({ssh_cm, ConnRef, {exit_status, ChannelId, _ExitStatus}},
               #state{connection_ref = ConnRef,
                      channel_id = ChannelId} = State) ->
    {ok, State}.

%% @private
terminate(Subsystem, State) ->
    ?INFO("SSH connection with subsystem: ~p terminated with state: ~p",
          [Subsystem, State]).

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

%% @private
handle_messages(Module, Parser, Data) ->
    %% Decode messages
    {ok, Messages, NewParser} = Module:parse(Data, Parser),

    %% TODO: Parse and execute received rpc operations
    [begin
         parse_xml(Msg),
         ?INFO("Received: ~p~n", [Msg])
     end || Msg <- Messages],
    NewParser.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

%% @private
get_session_id() ->
    %% TODO: Create global session-id counter
    1.

%% @private
get_server_capabilities(SessionId) ->
    {ok, Capabilities} = application:get_env(enetconf, capabilities),
    enetconf_xml:capabilities(Capabilities, SessionId).

%% @private
get_parser_module(#hello{capabilities = Capabilities}) ->
    case lists:member(?BASE_CAPABILITY, Capabilities) of
        true ->
            enetconf_fm_chunked;
        false ->
            case lists:member(?BASE_CAPABILITY_OLD, Capabilities) of
                true ->
                    enetconf_fm_eom;
                false ->
                    {error, bad_version}
            end
    end.

%% @private
parse_xml(_) ->
    ok.
