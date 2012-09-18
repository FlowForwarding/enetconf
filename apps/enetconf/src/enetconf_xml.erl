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
-export([hello/1,
         hello/2]).
-export([get_config/3,
         edit_config/3,
         copy_config/3,
         delete_config/2,
         lock/2,
         unlock/2,
         get/2,
         close_session/1]).
-export([ok/1,
         config_reply/2]).
-export([to_simple_form/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include("enetconf.hrl").

-define(PROLOG, {prolog, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"}).
-define(NS, {xmlns, "urn:ietf:params:xml:ns:netconf:base:1.0"}).

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

%% @doc Returns hello message with list of available capabilities.
-spec hello([string()]) -> binary().
hello(Capabilities) ->
    export({hello, [?NS],
            [{capabilities, [],
              [{capability, [], [?BASE_CAPABILITY]}
               | [{capability, [], [Cap]} || Cap <- Capabilities]]}]}).

%% @doc Returns hello message with list of available capabilities + session-id.
-spec hello([string()], integer()) -> binary().
hello(Capabilities, SessionId) ->
    export({hello, [?NS],
            [{capabilities, [],
              [{capability, [], [?BASE_CAPABILITY]}
               | [{capability, [], [Cap]} || Cap <- Capabilities]]},
             {'session-id', [], [integer_to_list(SessionId)]}]}).

get_config(MessageId, Source, Filter) ->
    export({rpc, [{'message-id', MessageId}, ?NS],
            [{'get-config', [],
              [{source, [], [get_config_source(Source)]}]
              ++ [filter(Filter) || Filter /= undefined]}]}).

edit_config(MessageId, Target, Config) ->
    export({rpc, [{'message-id', MessageId}, ?NS],
            [{'edit-config', [],
              [{target, [], [target(Target)]},
               {config, [], [config(Config)]}]}]}).

copy_config(MessageId, Source, Target) ->
    export({rpc, [{'message-id', MessageId}, ?NS],
            [{'copy-config', [],
              [{target, [], [target(Target)]},
               {source, [], [source(Source)]}]}]}).

delete_config(MessageId, Target) ->
    export({rpc, [{'message-id', MessageId}, ?NS],
            [{'delete-config', [],
              [{target, [], [target(Target)]}]}]}).

lock(MessageId, Target) ->
    export({rpc, [{'message-id', MessageId}, ?NS],
            [{lock, [],
              [{target, [], [target(Target)]}]}]}).

unlock(MessageId, Target) ->
    export({rpc, [{'message-id', MessageId}, ?NS],
            [{unlock, [],
              [{target, [], [target(Target)]}]}]}).

get(MessageId, Filter) ->
    export({rpc, [{'message-id', MessageId}, ?NS],
            [{get, [],
              [filter(Filter) || Filter /= undefined]}]}).

close_session(MessageId) ->
    export({rpc, [{'message-id', MessageId}, ?NS],
            [{'close-session', [], []}]}).

%% @doc Return ok.
-spec ok(string()) -> binary().
ok(MessageId) ->
    export({'rpc-reply', [{'message-id', MessageId}, ?NS],
            [{ok, [], []}]}).

config_reply(MessageId, Config) ->
    export({'rpc-reply', [{'message-id', MessageId}, ?NS],
            [{data, [], [to_simple_form(Config)]}]}).

%% @doc Convert XML records returned by xmerl to a simple form tuples.
%% It will output only the xmlElement and xmlText records and skip all the
%% unnecessary whitespace xmlTexts.
-spec to_simple_form(#xmlElement{} | list()) -> tuple().
to_simple_form(#xmlElement{name = Name,
                           attributes = Attrs,
                           content = Content}) ->
    {Name, attributes(Attrs), content(Content)};
to_simple_form(#xmlText{value = Value}) ->
    string:strip(Value);
to_simple_form(Elements) when is_list(Elements) ->
    content(Elements).

%%------------------------------------------------------------------------------
%% Simple form creation functions
%%------------------------------------------------------------------------------

%% @private
source({url, Url}) ->
    {url, [], [Url]};
source({xml, XML}) ->
    XML;
source(Name) ->
    {Name, [], []}.

%% @private
get_config_source({url, Url}) ->
    {url, [], [Url]};
get_config_source(Name) ->
    {Name, [], []}.

%% @private
target({url, Url}) ->
    {url, [], [Url]};
target(Name) ->
    {Name, [], []}.

%% @private
config({xml, XML}) ->
    to_simple_form(XML);
config({url, Url}) ->
    {url, [], [Url]}.

%% @private
filter({subtree, Subtree}) ->
    {filter, [{type, subtree}], [to_simple_form(Subtree)]};
filter({xpath, Select}) ->
    {filter, [{type, xpath}, {select, Select}], []}.

%%------------------------------------------------------------------------------
%% Simple form conversion functions
%%------------------------------------------------------------------------------

%% @private
content(Elements) ->
    content(Elements, []).

%% @private
content([], SimpleForms) ->
    lists:reverse(SimpleForms);
content([Element | Rest], SimpleForms) when is_record(Element, xmlElement) or
                                            is_record(Element, xmlText) ->
    case to_simple_form(Element) of
        "" ->
            content(Rest, SimpleForms);
        SimpleForm ->
            content(Rest, [SimpleForm | SimpleForms])
    end;
content([_ | Rest], SimpleForms) ->
    content(Rest, SimpleForms).

%% @private
attributes(Attrs) ->
    [{Name, Value} || #xmlAttribute{name = Name, value = Value} <- Attrs].

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

%% @private
export(SimpleFormXml) ->
    list_to_binary(xmerl:export_simple([SimpleFormXml], xmerl_xml, [?PROLOG])).
