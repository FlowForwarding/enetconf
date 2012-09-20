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
%% @doc Default callback module.
-module(enetconf_default).

-behaviour(gen_netconf).

%% gen_netconf callbacks
-export([handle_get_config/2,
	 handle_edit_config/2,
	 handle_copy_config/2,
	 handle_delete_config/1,
         handle_lock/1,
         handle_unlock/1,
         handle_get/1]).

-include_lib("xmerl/include/xmerl.hrl").

-define(TEST_CONFIG, #xmlElement{name = test}).

%%------------------------------------------------------------------------------
%% gen_netconf callbacks
%%------------------------------------------------------------------------------

%% @private
handle_get_config(Source, Filter) ->
    From = case Source of
               {url, Url} ->
                   io_lib:format("Get config from ~p", [Url]);
               ConfigName ->
                   io_lib:format("Get ~p config", [ConfigName])
           end,
    With = case Filter of
               {subtree, Subtree} ->
                   io_lib:format(" with subtree filter: ~p~n",
                                 [to_xml(Subtree)]);
               {xpath, Select} ->
                   io_lib:format(" with xpath filter: ~p~n", [Select]);
               undefined ->
                   "~n"
           end,
    error_logger:info_msg(From ++ With, []),
    {ok, ?TEST_CONFIG}.

%% @private
handle_edit_config(Target, Config) ->
    To = case Target of
             {url, Url} ->
                 io_lib:format("Edit config at ~p", [Url]);
             ConfigName ->
                 io_lib:format("Edit ~p config", [ConfigName])
         end,
    What = case Config of
               {url, Url2} ->
                   io_lib:format(" using the one from ~p~n", [Url2]);
               {xml, XML} ->
                   io_lib:format(" with: ~p~n", [to_xml(XML)])
           end,
    error_logger:info_msg(To ++ What, []),
    ok.

%% @private
handle_copy_config(Source, Target) ->
    From = case Source of
               {url, Url} ->
                   io_lib:format("Copy config from ~p", [Url]);
               {xml, XML} ->
                   io_lib:format("Copy config: ~p", [to_xml(XML)]);
               ConfigName ->
                   io_lib:format("Copy ~p config", [ConfigName])
           end,
    To = case Target of
             {url, Url2} ->
                 io_lib:format(" to ~p~n", [Url2]);
             ConfigName2 ->
                 io_lib:format(" to ~p config~n", [ConfigName2])
         end,
    error_logger:info_msg(From ++ To, []),
    ok.

%% @private
handle_delete_config({url, Url}) ->
    error_logger:info_msg("Delete config from ~p~n", [Url]),
    ok;
handle_delete_config(ConfigName) ->
    error_logger:info_msg("Delete ~p config~n", [ConfigName]),
    ok.

%% @private
handle_lock({url, Url}) ->
    error_logger:info_msg("Lock config from ~p~n", [Url]),
    ok;
handle_lock(ConfigName) ->
    error_logger:info_msg("Lock ~p config~n", [ConfigName]),
    ok.

%% @private
handle_unlock({url, Url}) ->
    error_logger:info_msg("Unlock config from ~p~n", [Url]),
    ok;
handle_unlock(ConfigName) ->
    error_logger:info_msg("Unlock ~p config~n", [ConfigName]),
    ok.

%% @private
handle_get({subtree, Subtree}) ->
    error_logger:info_msg("Get state with subtree filter: ~p~n",
                          [to_xml(Subtree)]),
    {ok, ?TEST_CONFIG};
handle_get({xpath, Select}) ->
    error_logger:info_msg("Get state with xpath filter: ~p~n", [Select]),
    {ok, ?TEST_CONFIG};
handle_get(undefined) ->
    error_logger:info_msg("Get state~n", []),
    {ok, ?TEST_CONFIG}.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

%% @private
to_xml(XMLElement) ->
    lists:flatten(xmerl:export([XMLElement], xmerl_xml, [{prolog, ""}])).
