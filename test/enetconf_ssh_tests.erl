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
-include_lib("xmerl/include/xmerl.hrl").

-define(PORT, 8830).
-define(TIMEOUT, 5000).
-define(SERVER_CAPABILITIES, []).
-define(CONNECT_OPTS, [{port, ?PORT}, {user, "test"}, {password, "test"},
                       {silently_accept_hosts, true}]).
-define(CONFIG, #xmlElement{name = test}).

%% Tests -----------------------------------------------------------------------

ssh_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Basic connectivity", fun hello/0},
      {"Get configuration operation", fun get_config/0},
      {"Edit configuration operation", fun edit_config/0},
      {"Copy configuration operation", fun copy_config/0},
      {"Delete configuration operation", fun delete_config/0},
      {"Lock configuration operation", fun lock/0},
      {"Unlock configuration operation", fun unlock/0},
      {"Get configuration + state operation", fun get/0}]}.

hello() ->
    {ok, C} = enetconf_client:connect("localhost", ?CONNECT_OPTS),
    {ok, Ok} = enetconf_client:close_session(C),
    ?assertEqual(enetconf_xml:ok("0"), Ok),
    ?assertNot(is_process_alive(C)).

get_config() ->
    {ok, C} = enetconf_client:connect("localhost", ?CONNECT_OPTS),

    ExpectedReply = enetconf_xml:config_reply("0", ?CONFIG),
    {ok, Reply} = enetconf_client:get_config(C, running),
    ?assertEqual(ExpectedReply, Reply),

    {ok, Ok} = enetconf_client:close_session(C),
    ?assertEqual(enetconf_xml:ok("1"), Ok),
    ?assertNot(is_process_alive(C)).

edit_config() ->
    {ok, C} = enetconf_client:connect("localhost", ?CONNECT_OPTS),

    {ok, Reply} = enetconf_client:edit_config(C, running, {xml, ?CONFIG}),
    ?assertEqual(enetconf_xml:ok("0"), Reply),

    {ok, Ok} = enetconf_client:close_session(C),
    ?assertEqual(enetconf_xml:ok("1"), Ok),
    ?assertNot(is_process_alive(C)).

copy_config() ->
    {ok, C} = enetconf_client:connect("localhost", ?CONNECT_OPTS),

    {ok, Reply} = enetconf_client:copy_config(C, startup, running),
    ?assertEqual(enetconf_xml:ok("0"), Reply),

    {ok, Ok} = enetconf_client:close_session(C),
    ?assertEqual(enetconf_xml:ok("1"), Ok),
    ?assertNot(is_process_alive(C)).

delete_config() ->
    {ok, C} = enetconf_client:connect("localhost", ?CONNECT_OPTS),

    {ok, Reply} = enetconf_client:delete_config(C, candidate),
    ?assertEqual(enetconf_xml:ok("0"), Reply),

    {ok, Ok} = enetconf_client:close_session(C),
    ?assertEqual(enetconf_xml:ok("1"), Ok),
    ?assertNot(is_process_alive(C)).

lock() ->
    {ok, C} = enetconf_client:connect("localhost", ?CONNECT_OPTS),

    {ok, Reply} = enetconf_client:lock(C, candidate),
    ?assertEqual(enetconf_xml:ok("0"), Reply),

    {ok, Ok} = enetconf_client:close_session(C),
    ?assertEqual(enetconf_xml:ok("1"), Ok),
    ?assertNot(is_process_alive(C)).

unlock() ->
    {ok, C} = enetconf_client:connect("localhost", ?CONNECT_OPTS),

    {ok, Reply} = enetconf_client:unlock(C, candidate),
    ?assertEqual(enetconf_xml:ok("0"), Reply),

    {ok, Ok} = enetconf_client:close_session(C),
    ?assertEqual(enetconf_xml:ok("1"), Ok),
    ?assertNot(is_process_alive(C)).

get() ->
    {ok, C} = enetconf_client:connect("localhost", ?CONNECT_OPTS),

    ExpectedReply = enetconf_xml:config_reply("0", ?CONFIG),
    {ok, Reply} = enetconf_client:get(C, undefined),
    ?assertEqual(ExpectedReply, Reply),

    {ok, Ok} = enetconf_client:close_session(C),
    ?assertEqual(enetconf_xml:ok("1"), Ok),
    ?assertNot(is_process_alive(C)).

%% Fixtures --------------------------------------------------------------------

setup() ->
    error_logger:tty(false),

    %% HACK: Rebar is not preserving the directory structure and copies
    %%       everything to .eunit, so this symlink makes code:priv_dir/1
    %%       works again.
    file:make_symlink("..", "enetconf"),
    code:add_path("./enetconf/ebin"),

    application:load(enetconf),
    application:set_env(enetconf, schema_file, "netconf-1.0.xsd"),
    application:set_env(enetconf, capabilities, ?SERVER_CAPABILITIES),
    application:set_env(enetconf, callback_module, enetconf_default),
    application:set_env(enetconf, sshd_ip, any),
    application:set_env(enetconf, sshd_port, ?PORT),
    application:set_env(enetconf, sshd_shell, {undefined, undefined, []}),
    application:set_env(enetconf, sshd_user_passwords, [{"test", "test"}]),
    application:start(ssh),
    application:start(xmerl),
    application:start(enetconf).

teardown(_) ->
    application:stop(enetconf),
    application:stop(xmerl),
    application:stop(ssh).
