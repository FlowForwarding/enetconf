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
%% @author Konrad Kaplita <konrad.kaplita@erlang-solutions.com>
%% @copyright 2012 FlowForwarding.org
%% @doc NETCONF Network Configuration Protocol for Erlang.
-module(enetconf).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

%%------------------------------------------------------------------------------
%% Application callbacks
%%------------------------------------------------------------------------------

%% @private
start(_, _) ->
    load_schema(),
    init_global_session_id(),
    {ok, Pid} = start_ssh_daemon(),
    {ok, Pid, Pid}.

%% @private
stop(Pid) ->
    ssh:stop_daemon(Pid),
    ets:delete(enetconf).

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

%% @private
load_schema() ->
    %% Process the schema
    {ok, SchemaFile} = application:get_env(enetconf, schema_file),
    SchemaPath = filename:join(code:priv_dir(enetconf), SchemaFile),
    {ok, State} = xmerl_xsd:process_schema(SchemaPath),

    %% Save it to an ets table
    ets:new(enetconf, [named_table, set, public, {read_concurrency, true}]),
    ets:insert(enetconf, {schema, State}).

%% @private
init_global_session_id() ->
    ets:insert(enetconf, {session_id, -1}).

%% @private
start_ssh_daemon() ->
    %% Load the configuration
    {ok, IP} = application:get_env(sshd_ip),
    {ok, Port} = application:get_env(sshd_port),
    {ok, Passwords} = application:get_env(sshd_user_passwords),

    %% Start the daemon
    ssh:daemon(IP, Port,
               [{system_dir, filename:join([code:priv_dir(enetconf), "sshd"])},
                {user_dir, filename:join([code:priv_dir(enetconf), "sshd"])},
                %% Because NETCONF communication is handled as a separate
                %% subsystem, regular ssh shell is unavailable for the client.
                %% SSH connection _must_ always be initialized by the client
                %% with netconf subsystem like the following example:
                %% 'ssh guest@127.0.0.1 -p 830 -s netconf'
                %% Any attempt to connect to the regular ssh shell will be
                %% silently dropped and connection will be terminated by the
                %% server.
                {shell, {unavailable, unavailable, []}},
                {subsystems, [{"netconf", {enetconf_ssh, []}}]},
                {user_passwords, Passwords}]).
