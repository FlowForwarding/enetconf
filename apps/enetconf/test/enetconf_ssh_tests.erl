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
%% @doc eUnit suite for testing SSH connectivity.
%% @private
-module(enetconf_ssh_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PORT, 8830).
-define(TIMEOUT, 1000).
-define(SERVER_CAPABILITIES, []).

%% Tests -----------------------------------------------------------------------

ssh_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Connect to SSH server, exchange hellos, close session", fun hello/0}]}.

hello() ->
    C = connect(),
    exchange_hellos(C),
    close_session(C).

%% Fixtures --------------------------------------------------------------------

setup() ->
    SchemaPath = filename:join(code:priv_dir(enetconf), "netconf-1.0.xsd"),
    {ok, State} = xmerl_xsd:process_schema(SchemaPath),
    ets:new(enetconf, [named_table, set, public, {read_concurrency, true}]),
    ets:insert(enetconf, {schema, State}),

    error_logger:tty(false),
    application:set_env(enetconf, capabilities, ?SERVER_CAPABILITIES),
    application:set_env(enetconf, callback_module, undefined),
    application:start(ssh),
    Config = [{system_dir, filename:join([code:priv_dir(enetconf), "sshd"])},
              {user_dir, filename:join([code:priv_dir(enetconf), "sshd"])},
              {shell, {enetconf_ssh, spawn_link, []}},
              {subsystems, [{"netconf", {enetconf_ssh, []}}]},
              {user_passwords, [{"test", "test"}]}],
    {ok, Ref} = ssh:daemon(any, ?PORT, Config),
    Ref.

teardown(Ref) ->
    exit(Ref, kill),
    application:stop(ssh).

%% Helper functions ------------------------------------------------------------

exchange_hellos(C) ->
    %% Send hello
    send(C, enetconf_xml:hello([]), eom),
    
    %% Wait for hello
    Message = wait_for_message(C, eom),
    ?assertEqual(enetconf_xml:hello(?SERVER_CAPABILITIES, 1), Message).

close_session(C) ->
    %% Send close-session
    send(C, enetconf_xml:close_session("123"), chunked),

    %% Wait for ok
    Message = wait_for_message(C, chunked),
    ?assertEqual(enetconf_xml:ok("123"), Message).

connect() ->
    %% Connect to SSH server
    Config = [{user, "test"}, {password, "test"},
              {silently_accept_hosts, true}],
    {ok, Ref} = ssh:connect("localhost", ?PORT, Config),
    {ok, Channel} = ssh_connection:session_channel(Ref, ?TIMEOUT),
    success = ssh_connection:subsystem(Ref, Channel, "netconf", ?TIMEOUT),
    {Ref, Channel}.

send({Ref, Channel}, Message, FM) ->
    Module = parser(FM),
    {ok, EncodedMessage} = Module:encode(Message),
    ssh_connection:send(Ref, Channel, EncodedMessage).

wait_for_message({Ref, Channel}, FM) ->
    receive
        {ssh_cm, Ref, {data, Channel, 0, Data}} ->
            Module = parser(FM),
            {ok, [Message], _} = Module:decode(Data),
            Message
    after
        ?TIMEOUT ->
            timeout
    end.

parser(eom) ->
    enetconf_fm_eom;
parser(chunked) ->
    enetconf_fm_chunked.
