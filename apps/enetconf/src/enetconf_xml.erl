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
%% @doc Module for generating XML replies and errors.
-module(enetconf_xml).

%% API
-export([ok/1,
	 capabilities/1,
	 capabilities/2]).

-include_lib("xmerl/include/xmerl.hrl").
-include("enetconf.hrl").

-define(PROLOG, {prolog, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"}).
-define(NS, {xmlns, "urn:ietf:params:xml:ns:netconf:base:1.0"}).

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

%% @doc Return ok.
-spec ok(string()) -> binary().
ok(MessageId) ->
    export({'rpc-reply', [{'message-id', MessageId}, ?NS],
	    [{ok, [], []}]}).

%% @doc Returns hello message with list of available capabilities.
-spec capabilities([string()]) -> binary().
capabilities(Capabilities) ->
    export({hello, [?NS],
	    [{capabilities, [],
	      [{capability, [], [?BASE_CAPABILITY]}
	       | [{capability, [], [Cap]} || Cap <- Capabilities]]}]}).

%% @doc Returns hello message with list of available capabilities + session-id.
-spec capabilities([string()], integer()) -> binary().
capabilities(Capabilities, SessionId) ->
    export({hello, [?NS],
	    [{capabilities, [],
	      [{capability, [], [?BASE_CAPABILITY]}
	       | [{capability, [], [Cap]} || Cap <- Capabilities]]},
	     {'session-id', [], [integer_to_list(SessionId)]}]}).

%% @private
export(SimpleFormXml) ->
    list_to_binary(xmerl:export_simple([SimpleFormXml], xmerl_xml, [?PROLOG])).
