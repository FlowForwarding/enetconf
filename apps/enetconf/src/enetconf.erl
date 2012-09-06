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
    {ok, IP} = application:get_env(sshd_ip),
    {ok, Port} = application:get_env(sshd_port),
    {ok, Passwords} = application:get_env(sshd_user_passwords),
    {ok, ShellMFA} = application:get_env(sshd_shell),
    ssh:daemon(IP, Port,
               [{system_dir, filename:join([code:priv_dir(?MODULE), "sshd"])},
                {user_dir, filename:join([code:priv_dir(?MODULE), "sshd"])},
                {shell, ShellMFA}, 
                {subsystems, []},
                {user_passwords, Passwords}
               ]).

%% @private
stop(_) ->
    ok.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

%% @private
load_schema() ->
    %% Process the schema
    {ok, SchemaFile} = application:get_env(?MODULE, schema_file),
    SchemaPath = filename:join(code:priv_dir(?MODULE), SchemaFile),
    {ok, State} = xmerl_xsd:process_schema(SchemaPath),

    %% Save it to an ets table
    ets:new(?MODULE, [named_table, set, public, {read_concurrency, true}]),
    ets:insert(?MODULE, {schema, State}).
