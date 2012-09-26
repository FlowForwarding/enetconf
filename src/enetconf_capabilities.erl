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
%% @doc Module for handling NETCONF's capabilities.
-module(enetconf_capabilities).

%% API
-export([versions/1,
         check/2]).

-include("enetconf.hrl").

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

%% @doc Return acceptable capability versions based on the 'base' version.
-spec versions({base, {integer(), integer()}}) -> [capability()].
versions({base, {1, 0}}) ->
    [{'writable-running', {1, 0}},
     {candidate, {1, 0}},
     {'rollback-on-error', {1, 0}},
     {startup, {1, 0}},
     {url, {1, 0}},
     {xpath, {1, 0}}];
versions({base, {1, 1}}) ->
    Updates = [],
    Updated = update_versions(Updates, versions({1, 0})),
    New = [{'confirmed-commit', {1, 1}},
           {validate, {1,1}}],
    Updated ++ New.

%% @doc Check if a given capability is allowed and supported.
-spec check(capability(), [capability()]) -> boolean().
check(Capability, Capabilities) ->
    lists:memeber(Capability, Capabilities).

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

%% @private
update_versions(Updates, Previous) ->
    UpdateF = fun({Name, _} = Update, Updated) ->
                      lists:keyreplace(Name, 1, Updated, Update)
              end,
    lists:foldl(UpdateF, Previous, Updates).
