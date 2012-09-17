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
%% @doc Simple NETCONF 1.1 client.
-module(enetconf_client).

-behaviour(gen_server).

%% API
-export([connect/2,
         get_config/2,
         edit_config/3,
         copy_config/3,
         delete_config/2,
         close_session/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("enetconf.hrl").

-record(state, {
          connection :: {pid(), integer()},
          message_id = 0 :: integer(),
          parsing_module = enetconf_fm_chunked :: enetconf_fm_chunked
                                                | enetconf_fm_eom
         }).

-define(DEFAULT_PORT, 830).
-define(TIMEOUT, 3000).

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

%% @doc Start the client by connecting to an SSH server.
-spec connect(string(), [{atom(), term()}]) -> {ok, Pid :: pid()} | ignore |
                                               {error, Reason :: term()}.
connect(Host, Opts) ->
    gen_server:start(?MODULE, [Host, Opts], []).

%% @doc Get configuration.
-spec get_config(pid(), get_config_source()) -> {ok, Reply :: term()} |
                                                {error, timeout}.
get_config(Pid, Source) ->
    gen_server:call(Pid, {get_config, Source}).

%% @doc Edit configuration.
-spec edit_config(pid(), target(), config()) -> {ok, Reply :: term()} |
                                                {error, timeout}.
edit_config(Pid, Target, Config) ->
    gen_server:call(Pid, {edit_config, Target, Config}).

%% @doc Copy configuration.
-spec copy_config(pid(), source(), target()) -> {ok, Reply :: term()} |
                                                {error, timeout}.
copy_config(Pid, Source, Target) ->
    gen_server:call(Pid, {copy_config, Source, Target}).

%% @doc Delete configuration.
-spec delete_config(pid(), target()) -> {ok, Reply :: term()} |
                                        {error, timeout}.
delete_config(Pid, Target) ->
    gen_server:call(Pid, {delete_config, Target}).

%% @doc Close the session.
-spec close_session(pid()) -> {ok, Reply :: term()} | {error, timeout}.
close_session(Pid) ->
    gen_server:call(Pid, close_session).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

%% @private
init([Host, Opts]) ->
    application:start(crypto),
    application:start(ssh),
    Port = case lists:keyfind(port, 1, Opts) of
               false ->
                   ?DEFAULT_PORT;
               {port, P} ->
                   P
           end,
    {ok, Pid} = ssh:connect(Host, Port, lists:keydelete(port, 1, Opts)),
    {ok, Channel} = ssh_connection:session_channel(Pid, ?TIMEOUT),
    success = ssh_connection:subsystem(Pid, Channel, "netconf", ?TIMEOUT),
    Connection = {Pid, Channel},
    {ok, _} = do_send(Connection, enetconf_xml:hello([]), enetconf_fm_eom),
    {ok, #state{connection = Connection}}.

%% @private
handle_call(close_session, _,
            #state{connection = C, message_id = MessageId} = State) ->
    Reply = do_send(C, enetconf_xml:close_session(MessageId)),
    {stop, normal, Reply, State};
handle_call({get_config, Source}, _,
            #state{connection = C, message_id = MessageId} = State) ->
    GetConfig = enetconf_xml:get_config(MessageId, Source, undefined),
    Reply = do_send(C, GetConfig),
    {reply, Reply, State#state{message_id = MessageId + 1}};
handle_call({edit_config, Target, Config}, _,
            #state{connection = C, message_id = MessageId} = State) ->
    EditConfig = enetconf_xml:edit_config(MessageId, Target, Config),
    Reply = do_send(C, EditConfig),
    {reply, Reply, State#state{message_id = MessageId + 1}};
handle_call({copy_config, Source, Target}, _,
            #state{connection = C, message_id = MessageId} = State) ->
    CopyConfig = enetconf_xml:copy_config(MessageId, Source, Target),
    Reply = do_send(C, CopyConfig),
    {reply, Reply, State#state{message_id = MessageId + 1}};
handle_call({delete_config, Target}, _,
            #state{connection = C, message_id = MessageId} = State) ->
    DeleteConfig = enetconf_xml:delete_config(MessageId, Target),
    Reply = do_send(C, DeleteConfig),
    {reply, Reply, State#state{message_id = MessageId + 1}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, #state{connection = {Pid, Channel}}) ->
    ssh_connection:close(Pid, Channel).

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

%% @private
do_send(Connection, Message) ->
    do_send(Connection, Message, enetconf_fm_chunked).

%% @private
do_send({Pid, Channel}, Message, Module) ->
    {ok, EncodedMessage} = Module:encode(Message),
    ssh_connection:send(Pid, Channel, EncodedMessage),

    %% Wait for the reply
    receive
        {ssh_cm, Pid, {data, Channel, 0, Data}} ->
            {ok, [Reply], _} = Module:decode(Data),
            {ok, Reply}
    after
        ?TIMEOUT ->
            {error, timeout}
    end.
