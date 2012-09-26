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
          connection_ref :: ssh_channel:connection_ref(),
          channel_id :: ssh_channel:channel_id(),
          session_id :: integer(),
          parsing_module :: enetconf_frame_end | enetconf_frame_chunk,
          parser :: record(),
          callback_module :: atom()
         }).

-define(DATA_TYPE_CODE, 0).

%%-----------------------------------------------------------------------------
%% ssh_channel callbacks
%%-----------------------------------------------------------------------------

%% @private
init(_) ->
    {ok, Callback} = application:get_env(enetconf, callback_module),
    {ok, #state{callback_module = Callback}}.

%% @private
handle_msg({ssh_channel_up, ChannelId, ConnRef}, State) ->
    SessionId = get_session_id(),
    Capabilities = get_server_capabilities(SessionId),
    {ok, EncodedCaps} = enetconf_fm_eom:encode(Capabilities),
    ssh_connection:send(ConnRef, ChannelId, EncodedCaps),
    {ok, State#state{connection_ref= ConnRef,
                     channel_id = ChannelId,
                     session_id = SessionId}};
handle_msg({'EXIT', Reason}, #state{channel_id = ChannelId} = State) ->
    ?INFO("SSH channel ~p exited with reason: ~p", [ChannelId, Reason]),
    {stop, ChannelId, State}.

%% @private
handle_ssh_msg({ssh_cm, ConnRef, {data, ChannelId, ?DATA_TYPE_CODE, Data}},
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
                            TempState = State#state{parsing_module = Module,
                                                    parser = Parser},
                            NewState = handle_messages(Rest, TempState),
                            {ok, NewState}
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
handle_ssh_msg({ssh_cm, ConnRef, {data, ChannelId, ?DATA_TYPE_CODE, Data}},
               #state{connection_ref= ConnRef,
                      channel_id = ChannelId} = State) ->
    NewState = handle_messages(Data, State),
    {ok, NewState};
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
handle_messages(Data, #state{connection_ref = ConnRef, channel_id = ChannelId,
                             session_id = SessionId,
                             parsing_module = Module, parser = Parser,
                             callback_module = Callback} = State) ->
    %% Decode messages
    {ok, Messages, NewParser} = Module:parse(Data, Parser),

    %% Parse and execute received rpc operations
    [begin
         ?INFO("Received: ~p~n", [Msg]),
         case parse_xml(Msg, SessionId, Callback) of
             {ok, MessageId} ->
                 Ok = enetconf_xml:ok(MessageId),
                 send(ConnRef, ChannelId, Module, Ok);
             {ok, MessageId, Config} ->
                 ConfigReply = enetconf_xml:config_reply(MessageId, Config),
                 send(ConnRef, ChannelId, Module, ConfigReply);
             {error, MessageId, Reason} ->
                 ErrorReply = get_error(MessageId, Reason),
                 send(ConnRef, ChannelId, Module, ErrorReply);
             {close, MessageId} ->
                 Ok = enetconf_xml:ok(MessageId),
                 send(ConnRef, ChannelId, Module, Ok),
                 close(ConnRef, ChannelId)
         end
     end || Msg <- Messages],

    State#state{parser = NewParser}.

%% @private
parse_xml(XML, SessionId, Callback) ->
    case enetconf_parser:parse(binary_to_list(XML)) of
        {ok, Operation} ->
            execute(Operation, SessionId, Callback);
        {error, MessageId, Reason} ->
            {error, MessageId, Reason}
    end.

%% @private
execute(#hello{}, _SessionId, _Callback) ->
    {error, undefined, {invalid_value, rpc}};
execute(#rpc{message_id = MessageId,
             operation = Operation}, SessionId, Callback) ->
    case execute_rpc(Operation, SessionId, Callback) of
        ok ->
            {ok, MessageId};
        {ok, Config} ->
            {ok, MessageId, Config};
        close ->
            {close, MessageId};
        {error, Reason} ->
            {error, MessageId, Reason}
    end.

execute_rpc(#close_session{}, _SessionId, _Callback) ->
    close;
execute_rpc(#get_config{source = Source,
                        filter = Filter}, SessionId, Callback) ->
    Callback:handle_get_config(SessionId, Source, Filter);
execute_rpc(#edit_config{target = Target,
                         config = Config}, SessionId, Callback) ->
    Callback:handle_edit_config(SessionId, Target, Config);
execute_rpc(#copy_config{source = Source,
                         target = Target}, SessionId, Callback) ->
    Callback:handle_copy_config(SessionId, Source, Target);
execute_rpc(#delete_config{target = Target}, SessionId, Callback) ->
    Callback:handle_delete_config(SessionId, Target);
execute_rpc(#lock{target = Target}, SessionId, Callback) ->
    Callback:handle_lock(SessionId, Target);
execute_rpc(#unlock{target = Target}, SessionId, Callback) ->
    Callback:handle_unlock(SessionId, Target);
execute_rpc(#get{filter = Filter}, SessionId, Callback) ->
    Callback:handle_get(SessionId, Filter).

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

%% @private
get_session_id() ->
    ets:update_counter(enetconf, session_id, {2, 1, 65536, 0}).

%% @private
get_server_capabilities(SessionId) ->
    {ok, Capabilities} = application:get_env(enetconf, capabilities),
    enetconf_xml:hello(Capabilities, SessionId).

%% @private
get_parser_module(#hello{capabilities = Capabilities}) ->
    case lists:member({base, {1, 1}}, Capabilities) of
        true ->
            enetconf_fm_chunked;
        false ->
            case lists:member({base, {1, 0}}, Capabilities) of
                true ->
                    enetconf_fm_eom;
                false ->
                    {error, bad_version}
            end
    end.

%% @private
get_error(MessageId, {in_use, Type}) ->
    enetconf_xml:in_use(MessageId, Type);
get_error(MessageId, {invalid_value, Type}) ->
    enetconf_xml:invalid_value(MessageId, Type);
get_error(MessageId, _) ->
    %% TODO: Return other errors
    enetconf_xml:invalid_value(MessageId, application).

%% @private
send(Conn, Channel, Module, Message) ->
    {ok, EncodedMessage} = Module:encode(Message),
    ssh_connection:send(Conn, Channel, EncodedMessage).

%% @private
close(Conn, Channel) ->
    ssh_connection:close(Conn, Channel).
