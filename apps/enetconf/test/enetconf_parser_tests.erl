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
%% @doc eUnit suite for testing XML parsing.
%% @private
-module(enetconf_parser_tests).

-include_lib("eunit/include/eunit.hrl").
-include("enetconf.hrl").

-define(SCHEMA, "netconf-1.0.xsd").

%% Test XMLs -------------------------------------------------------------------

-define(EDIT_CONFIG_RPC,
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        "<rpc message-id=\"1\""
        "     xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">"
        "  <edit-config xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">"
        "    <target>"
        "      <candidate/>"
        "    </target>"
        "    <default-operation>merge</default-operation>"
        "    <test-option>set</test-option>"
        "    <config>"
        "      <some-configuration/>"
        "    </config>"
        "  </edit-config>"
        "</rpc>").

-define(GET_CONFIG_RPC,
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        "<rpc message-id=\"2\""
        "     xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">"
        "  <get-config xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">"
        "    <source>"
        "      <running/>"
        "    </source>"
        "    <filter type=\"xpath\" select=\"/some-configuration\"/>"
        "  </get-config>"
        "</rpc>").

-define(COPY_CONFIG_RPC,
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        "<rpc message-id=\"3\""
        "     xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">"
        "  <copy-config>"
        "    <target>"
        "      <running/>"
        "    </target>"
        "    <source>"
        "      <url>https://mydomain.com/new-config.xml</url>"
        "    </source>"
        "  </copy-config>"
        "</rpc>").

-define(DELETE_CONFIG_RPC,
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        "<rpc message-id=\"4\""
        "     xmlns=\"urn:ietf:params:xml:ns:netconf:base:1.0\">"
        "  <delete-config>"
        "    <target>"
        "      <startup/>"
        "    </target>"
        "  </delete-config>"
        "</rpc>").

%% Tests -----------------------------------------------------------------------

parsing_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Test the 'edit-config' operation", fun edit_config/0},
      {"Test the 'get-config' operation", fun get_config/0},
      {"Test the 'copy-config' operation", fun copy_config/0},
      {"Test the 'delete-config' operation", fun delete_config/0}]}.

edit_config() ->
    EditConfig = #edit_config{target = candidate,
                              default_operation = merge,
                              test_option = set},
    RPC = #rpc{message_id = "1",
               operation = EditConfig},
    ?assertEqual({ok, RPC}, enetconf_parser:parse(?EDIT_CONFIG_RPC)).

get_config() ->
    GetConfig = #get_config{source = running,
                            filter = {xpath, "/some-configuration"}},
    RPC = #rpc{message_id = "2",
               operation = GetConfig},
    ?assertEqual({ok, RPC}, enetconf_parser:parse(?GET_CONFIG_RPC)).

copy_config() ->
    Url = "https://mydomain.com/new-config.xml",
    CopyConfig = #copy_config{source = {url, Url},
                              target = running},
    RPC = #rpc{message_id = "3",
               operation = CopyConfig},
    ?assertEqual({ok, RPC}, enetconf_parser:parse(?COPY_CONFIG_RPC)).

delete_config() ->
    DeleteConfig = #delete_config{target = startup},
    RPC = #rpc{message_id = "4",
               operation = DeleteConfig},
    ?assertEqual({ok, RPC}, enetconf_parser:parse(?DELETE_CONFIG_RPC)).

%% Fixtures --------------------------------------------------------------------

setup() ->
    SchemaPath = filename:join(code:priv_dir(enetconf), ?SCHEMA),
    {ok, State} = xmerl_xsd:process_schema(SchemaPath),
    ets:new(enetconf, [named_table, set, public, {read_concurrency, true}]),
    ets:insert(enetconf, {schema, State}).

teardown(_) ->
    ets:delete(enetconf).
