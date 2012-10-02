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

%% API: Hellos
-export([hello/1,
         hello/2]).

%% API: Operations
-export([get_config/3,
         edit_config/3,
         copy_config/3,
         delete_config/2,
         lock/2,
         unlock/2,
         get/2,
         close_session/1,
         kill_session/2]).

%% API: Replies
-export([ok/1,
         config_reply/2]).

%% API: Errors
-export([in_use/2,
         invalid_value/2,
         too_big/2,
         missing_attribute/4,
         bad_attribute/4,
         unknown_attribute/4,
         missing_element/3,
         bad_element/3,
         unknown_element/3,
         unknown_namespace/4,
         access_denied/2,
         lock_denied/2,
         resource_denied/2,
         rollback_failed/2,
         data_exists/1,
         data_missing/1,
         operation_not_supported/2,
         operation_failed/2,
         partial_operation/4,
         malformed_message/1]).

%% API: Helpers
-export([to_simple_form/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include("enetconf.hrl").

-define(PROLOG, {prolog, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"}).
-define(NS, {xmlns, "urn:ietf:params:xml:ns:netconf:base:1.0"}).

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

%% Hellos ----------------------------------------------------------------------

%% @doc Returns hello message with list of available capabilities.
-spec hello([string()]) -> binary().
hello(Capabilities) ->
    export({hello, [?NS], [capabilities(Capabilities)]}).

%% @doc Returns hello message with list of available capabilities + session-id.
-spec hello([string()], integer()) -> binary().
hello(Capabilities, SessionId) ->
    export({hello, [?NS], [capabilities(Capabilities),
                           {'session-id', [integer_to_list(SessionId)]}]}).

%% Operations ------------------------------------------------------------------

get_config(MessageId, Source, Filter) ->
    rpc(MessageId, [{'get-config', [get_config_source(Source)]
                     ++ [filter(Filter) || Filter /= undefined]}]).

edit_config(MessageId, Target, Config) ->
    rpc(MessageId, [{'edit-config', [target(Target), config(Config)]}]).

copy_config(MessageId, Source, Target) ->
    rpc(MessageId, [{'copy-config', [target(Target), source(Source)]}]).

delete_config(MessageId, Target) ->
    rpc(MessageId, [{'delete-config', [target(Target)]}]).

lock(MessageId, Target) ->
    rpc(MessageId, [{lock, [target(Target)]}]).

unlock(MessageId, Target) ->
    rpc(MessageId, [{unlock, [target(Target)]}]).

get(MessageId, Filter) ->
    rpc(MessageId, [{get, [filter(Filter) || Filter /= undefined]}]).

close_session(MessageId) ->
    rpc(MessageId, ['close-session']).

kill_session(MessageId, SessionId) ->
    rpc(MessageId, [{'kill-session',
                     [{'session-id', [integer_to_list(SessionId)]}]}]).

%% Replies ---------------------------------------------------------------------

%% @doc Return ok.
-spec ok(string()) -> binary().
ok(MessageId) ->
    rpc_reply(MessageId, [ok]).

%% @doc Return the configuration.
config_reply(MessageId, Config) ->
    rpc_reply(MessageId, [{data, [to_simple_form(Config)]}]).

%% Errors ----------------------------------------------------------------------

-spec in_use(string(), protocol | application) -> binary().
in_use(MessageId, Type) ->
    rpc_reply(MessageId, rpc_error('in-use', Type, error, none)).

-spec invalid_value(string(), protocol | application) -> binary().
invalid_value(MessageId, Type) ->
    rpc_reply(MessageId, rpc_error('invalid-value', Type, error, none)).

-spec too_big(string(), transport | rpc | protocol | application) -> binary().
too_big(MessageId, Type) ->
    rpc_reply(MessageId, rpc_error('too-big', Type, error, none)).

-spec missing_attribute(string(), rpc | protocol | application,
                        atom(), atom()) -> binary().
missing_attribute(MessageId, Type, Attribute, Element) ->
    rpc_reply(MessageId, rpc_error('missing-attribute', Type, error,
                                   [{'bad-attribute', Attribute},
                                    {'bad-element', Element}])).

-spec bad_attribute(string(),
                    rpc | protocol | application, atom(), atom()) -> binary().
bad_attribute(MessageId, Type, Attribute, Element) ->
    rpc_reply(MessageId, rpc_error('bad-attribute', Type, error,
                                   [{'bad-attribute', Attribute},
                                    {'bad-element', Element}])).

-spec unknown_attribute(string(), rpc | protocol | application,
                        atom(), atom()) -> binary().
unknown_attribute(MessageId, Type, Attribute, Element) ->
    rpc_reply(MessageId, rpc_error('unknown-attribute', Type, error,
                                   [{'bad-attribute', Attribute},
                                    {'bad-element', Element}])).

-spec missing_element(string(), protocol | application, atom()) -> binary().
missing_element(MessageId, Type, Element) ->
    rpc_reply(MessageId, rpc_error('missing-element', Type, error,
                                  [{'bad-element', Element}])).

-spec bad_element(string(), protocol | application, atom()) -> binary().
bad_element(MessageId, Type, Element) ->
    rpc_reply(MessageId, rpc_error('bad-element', Type, error,
                                   [{'bad-element', Element}])).

-spec unknown_element(string(), protocol | application, atom()) -> binary().
unknown_element(MessageId, Type, Element) ->
    rpc_reply(MessageId, rpc_error('unknown-element', Type, error,
                                   [{'bad-element', Element}])).

-spec unknown_namespace(string(),
                        protocol | application, atom(), term()) -> binary().
unknown_namespace(MessageId, Type, Element, Namespace) ->
    rpc_reply(MessageId, rpc_error('unknown_namespace', Type, error,
                                   [{'bad-element', Element},
                                    {'bad-namespace', Namespace}])).

-spec access_denied(string(), protocol | application) -> binary().
access_denied(MessageId, Type) ->
    rpc_reply(MessageId, rpc_error('access-denied', Type, error, none)).

-spec lock_denied(string(), integer()) -> binary().
lock_denied(MessageId, SessionId) ->
    rpc_reply(MessageId, rpc_error('lock-denied', protocol, error,
                                   [{'session-id', SessionId}])).

-spec resource_denied(string(),
                      transport | rpc | protocol | application) -> binary().
resource_denied(MessageId, Type) ->
    rpc_reply(MessageId, rpc_error('resource-denied', Type, error, none)).

-spec rollback_failed(string(), protocol | application) -> binary().
rollback_failed(MessageId, Type) ->
    rpc_reply(MessageId, rpc_error('rollback-failed', Type, error, none)).

-spec data_exists(string()) -> binary().
data_exists(MessageId) ->
    rpc_reply(MessageId, rpc_error('data-exists', application, error, none)).

-spec data_missing(string()) -> binary().
data_missing(MessageId) ->
    rpc_reply(MessageId, rpc_error('data-missing', application, error, none)).

-spec operation_not_supported(string(), protocol | application) -> binary().
operation_not_supported(MessageId, Type) ->
    rpc_reply(MessageId, rpc_error('operation-not-supported',
                                   Type, error, none)).

-spec operation_failed(string(), rpc | protocol | application) -> binary().
operation_failed(MessageId, Type) ->
    rpc_reply(MessageId, rpc_error('operation-failed', Type, error, none)).

-spec partial_operation(string(), term(), term(), term()) -> binary().
partial_operation(MessageId, OkElement, ErrElement, NoopElement) ->
    rpc_reply(MessageId, rpc_error('partial-operation', application, error,
                                   [{'ok-element', OkElement},
                                    {'err-element', ErrElement},
                                    {'noop-element', NoopElement}])).

-spec malformed_message(string()) -> binary().
malformed_message(MessageId) ->
    rpc_reply(MessageId, rpc_error('malformed-message', rpc, error, none)).

%% Helpers ---------------------------------------------------------------------

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
capabilities(Capabilities) ->
    {capabilities, [{capability, [capability(Capability)]}
                    || Capability <- Capabilities]}.

%% @private
capability({base, {Ver1, Ver2}}) ->
    "urn:ietf:params:netconf:base:"
        ++ integer_to_list(Ver1) ++ "." ++ integer_to_list(Ver2);
capability({Name, {Ver1, Ver2}}) ->
    "urn:ietf:params:netconf:capability:" ++ atom_to_list(Name) ++ ":"
        ++ integer_to_list(Ver1) ++ "." ++ integer_to_list(Ver2).

%% @private
source({url, Url}) ->
    {source, [{url, [Url]}]};
source({xml, XML}) ->
    {source, [to_simple_form(XML)]};
source(Name) ->
    {source, [Name]}.

%% @private
get_config_source({url, Url}) ->
    {source, [{url, [Url]}]};
get_config_source(Name) ->
    {source, [Name]}.

%% @private
target({url, Url}) ->
    {target, [{url, [Url]}]};
target(Name) ->
    {target, [Name]}.

%% @private
config({xml, XML}) ->
    {config, [to_simple_form(XML)]};
config({url, Url}) ->
    {config, [{url, [Url]}]}.

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
rpc_error(Tag, Type, Severity, Info) ->
    [{'rpc-error',
      [{'error-tag', [atom_to_list(Tag)]},
       {'error-type', [atom_to_list(Type)]},
       {'error-severity', [atom_to_list(Severity)]}]
      ++ [{'error-info', [{Name, [to_list(Value)]}
                          || {Name, Value} <- Info, Info /= none]}]}].

%% @private
rpc(MessageId, Content) ->
    export({rpc, [{'message-id', MessageId}, ?NS], Content}).

%% @private
rpc_reply(MessageId, Content) ->
    export({'rpc-reply', [{'message-id', MessageId}, ?NS], Content}).

%% @private
export(SimpleFormXml) ->
    list_to_binary(xmerl:export_simple([SimpleFormXml], xmerl_xml, [?PROLOG])).

%% @private
to_list(Int) when is_integer(Int) ->
    integer_to_list(Int);
to_list(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_list(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
to_list(List) when is_list(List) ->
    List.
