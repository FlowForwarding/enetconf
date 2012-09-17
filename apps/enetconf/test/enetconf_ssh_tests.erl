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
     [{"Basic connectivity", fun hello/0}]}.

hello() ->
    {ok, C} = enetconf_client:connect("localhost", [{port, ?PORT},
                                                    {user, "test"},
                                                    {password, "test"}]),
    {ok, Ok} = enetconf_client:close_session(C),
    ?assertEqual(enetconf_xml:ok("0"), Ok),
    ?assertNot(is_process_alive(C)).

%% Fixtures --------------------------------------------------------------------

setup() ->
    error_logger:tty(false),
    application:load(enetconf),
    application:set_env(enetconf, schema_file, "netconf-1.0.xsd"),
    application:set_env(enetconf, capabilities, ?SERVER_CAPABILITIES),
    application:set_env(enetconf, callback_module, undefined),
    application:set_env(enetconf, sshd_ip, any),
    application:set_env(enetconf, sshd_port, ?PORT),
    application:set_env(enetconf, sshd_shell, {enetconf_ssh, spawn_link, []}),
    application:set_env(enetconf, sshd_user_passwords, [{"test", "test"}]),
    application:start(ssh),
    application:start(xmerl),
    application:start(enetconf).

teardown(_) ->
    application:stop(enetconf),
    application:stop(xmerl),
    application:stop(ssh).
