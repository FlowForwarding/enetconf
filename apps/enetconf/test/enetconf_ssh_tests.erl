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

-define(HELLO,
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        "<hello xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">"
        "  <capabilities>"
        "    <capability>"
        "      urn:ietf:params:netconf:base:1.1"
        "    </capability>"
        "  </capabilities>"
        "</hello>]]>]]>").

-define(SERVER_HELLO,
        <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
          "<hello xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">"
          "<capabilities>"
          "<capability>urn:ietf:params:netconf:base:1.1</capability>"
          "</capabilities>"
          "<session-id>1</session-id>"
          "</hello>]]>]]>">>).

%% Tests -----------------------------------------------------------------------

ssh_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Connect to SSH server and exchange hellos", fun hello/0}]}.

hello() ->
    %% Connect to SSH server
    Config = [{user, "test"}, {password, "test"},
              {silently_accept_hosts, true}],
    {ok, Ref} = ssh:connect("localhost", ?PORT, Config),
    {ok, Channel} = ssh_connection:session_channel(Ref, ?TIMEOUT),
    success = ssh_connection:subsystem(Ref, Channel, "netconf", ?TIMEOUT),

    %% Send hello
    ClientCapabilities = enetconf_xml:capabilities([]),
    {ok, EncCapabilities} = enetconf_fm_eom:encode(ClientCapabilities),
    ssh_connection:send(Ref, Channel, EncCapabilities),

    %% Wait for hello
    receive
        {ssh_cm, Ref, {data, Channel, 0, Message}} ->
            ok
    after
        ?TIMEOUT ->
            Message = timeout
    end,

    ?assertEqual(?SERVER_HELLO, Message),
    {Ref, Channel}.

setup() ->
    error_logger:tty(false),
    application:set_env(enetconf, capabilities, []),
    application:set_env(enetconf, callbacks, []),
    application:start(ssh),
    Config = [{system_dir, filename:join([code:priv_dir(enetconf), "sshd"])},
              {user_dir, filename:join([code:priv_dir(enetconf), "sshd"])},
              {shell, {enetconf_ssh, spawn_link, []}},
              {subsystems, [{"netconf", {enetconf_ssh, []}}]},
              {user_passwords, [{"test", "test"}]}],
    {ok, Ref} = ssh:daemon(any, ?PORT, Config),
    Ref.

teardown(Ref) ->
    ssh:stop_daemon(Ref),
    application:stop(ssh).
