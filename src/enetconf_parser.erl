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
%% @doc Module for parsing NETCONF's XML.
-module(enetconf_parser).

%% API
-export([parse/1]).

%% Internal exports
-export([capabilities/1,
         capability/1,
         source/1,
         target/1,
         running/1,
         startup/1,
         candidate/1,
         url/1,
         config/1,
         filter/1,
         'default-operation'/1,
         'test-option'/1,
         'error-option'/1,
         'session-id'/1,
         'get-config'/1,
         'edit-config'/1,
         'copy-config'/1,
         'delete-config'/1,
         lock/1,
         unlock/1,
         get/1,
         'close-session'/1,
         'kill-session'/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include("enetconf.hrl").

-define(OPERATIONS, ['get-config',
                     'edit-config',
                     'copy-config',
                     'delete-config',
                     lock,
                     unlock,
                     get,
                     'close-session',
                     'kill-session']).

-define(CONFIGS, [running,
                  startup,
                  candidate,
                  url,
                  config]).

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

%% @doc Parse incoming XML.
-spec parse(string()) -> ok | {error, Reason :: term()}.
parse(XML) ->
    %% TODO: Add feature list
    try
        %% Scan the XML, convert it to xmerl records
        {ScannedXML, _Rest} = xmerl_scan:string(XML, [{quiet, true}]),

        %% Convert xmerl records to simple form tuples
        SimpleXML = to_simple_form(ScannedXML),

        %% Parse the XML, validating along the way
        do_parse(SimpleXML)
    catch
        exit:{fatal, {_, _, _, _}} ->
            {error, malformed_message};
        throw:ParseError ->
            {error, ParseError}
    end.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

%% @private
do_parse({rpc, Attrs, _} = RPC) ->
    MessageId = get_attr('message-id', RPC, required),
    [Operation] = content([{choice, ?OPERATIONS}], RPC),
    OtherAttributes = other_attributes(Attrs),
    {ok, #rpc{message_id = MessageId,
              operation = Operation,
              attributes = OtherAttributes}};
do_parse({hello, _, _} = Hello) ->
    [Capabilities] = content([{required, capabilities}], Hello),
    {ok, #hello{capabilities = Capabilities}};
do_parse(_) ->
    throw(malformed_message).

%% @private
'get-config'(GetConfig) ->
    [] = attributes([], GetConfig),
    [Source, Filter] = content([{required, source},
                                {optional, filter}], GetConfig),
    #get_config{source = Source,
                filter = Filter}.

%% @private
'edit-config'(EditConfig) ->
    [] = attributes([], EditConfig),
    Content = content([{required, target},
                       {{optional, merge}, 'default-operation'},
                       {optional, 'test-option'},
                       {{optional, 'stop-on-error'}, 'error-option'},
                       {choice, [url, config]}], EditConfig),
    [Target, DefaultOperation, TestOption, ErrorOption, Config] = Content,
    #edit_config{target = Target,
                 default_operation = DefaultOperation,
                 test_option = TestOption,
                 error_option = ErrorOption,
                 config = Config}.

%% @private
'copy-config'(CopyConfig) ->
    [] = attributes([], CopyConfig),
    [Source, Target] = content([{required, source},
                                {required, target}], CopyConfig),
    #copy_config{source = Source,
                 target = Target}.

%% @private
'delete-config'(DeleteConfig) ->
    [] = attributes([], DeleteConfig),
    [Target] = content([{required, target}], DeleteConfig),
    #delete_config{target = Target}.

%% @private
lock(Lock) ->
    [] = attributes([], Lock),
    [Target] = content([{required, target}], Lock),
    #lock{target = Target}.

%% @private
unlock(Unlock) ->
    [] = attributes([], Unlock),
    [Target] = content([{required, target}], Unlock),
    #unlock{target = Target}.

%% @private
get(Get) ->
    [] = attributes([], Get),
    [Filter] = content([{optional, filter}], Get),
    #get{filter = Filter}.

%% @private
source(Source) ->
    [] = attributes([], Source),
    [Src] = content([{choice, ?CONFIGS}], Source),
    Src.

%% @private
target(Target) ->
    [] = attributes([], Target),
    [Trg] = content([{choice, ?CONFIGS}], Target),
    Trg.

%% @private
running(Running) ->
    [] = attributes([], Running),
    [] = content([], Running),
    running.

%% @private
startup(Startup) ->
    [] = attributes([], Startup),
    [] = content([], Startup),
    startup.

%% @private
candidate(Candidate) ->
    [] = attributes([], Candidate),
    [] = content([], Candidate),
    candidate.

%% @private
url(Url) ->
    [] = attributes([], Url),
    {url, get_text(Url)}.

%% @private
'default-operation'(DefaultOperation) ->
    [] = attributes([], DefaultOperation),
    get_text(DefaultOperation, atom).

%% @private
'test-option'(TestOption) ->
    [] = attributes([], TestOption),
    get_text(TestOption, atom).

%% @private
'error-option'(ErrorOption) ->
    [] = attributes([], ErrorOption),
    get_text(ErrorOption, atom).

%% @private
config({config, _, [XML]} = Config) ->
    [] = attributes([], Config),
    {xml, to_xmerl(XML)};
config({config, _, _}) ->
    throw({bad_element, application, config}).

%% @private
filter({filter, Attrs, Content} = Filter) ->
    case get_attr(type, Filter, atom, required) of
        subtree ->
            NewFilter = {filter, lists:keydelete(type, 1, Attrs), []},
            [] = attributes([], NewFilter),
            case Content of
                [Subtree] ->
                    {subtree, to_xmerl(Subtree)};
                _Else ->
                    throw({bad_element, application, filter})
            end;
        xpath ->
            [] = content([], Filter),
            NewFilter = {filter, lists:keydelete(type, 1, Attrs), []},
            [Select] = attributes([{required, select}], NewFilter),
            {xpath, Select}
    end.

%% @private
'session-id'(SessionId) ->
    [] = attributes([], SessionId),
    get_text(SessionId, integer).

%% @private
'close-session'(CloseSession) ->
    [] = attributes([], CloseSession),
    [] = content([], CloseSession),
    #close_session{}.

%% @private
'kill-session'(KillSession) ->
    [] = attributes([], KillSession),
    [SessionId] = content([{required, 'session-id'}], KillSession),
    #kill_session{session_id = SessionId}.

%% @private
capabilities(Capabilities) ->
    [] = attributes([], Capabilities),
    Caps = content([{list, capability}], Capabilities),
    case lists:keymember(base, 1, Caps) of
        true ->
            Caps;
        false ->
            throw({bad_element, application, capabilities})
    end.

%% @private
capability(Capability) ->
    [] = attributes([], Capability),
    get_text(Capability, capability).

%% @private
other_attributes(Attrs) ->
    other_attributes(Attrs, []).

%% @private
other_attributes([], Filtered) ->
    lists:reverse(Filtered);
other_attributes([{'message-id', _} | Rest], Filtered) ->
    other_attributes(Rest, Filtered);
other_attributes([{Name, _} | Rest] = Attrs, Filtered) ->
    case atom_to_list(Name) of
        [$x, $m, $l, $n, $s | _] ->
            other_attributes(Rest, Filtered);
        _ ->
            other_attributes(Attrs, Filtered)
    end.

%%------------------------------------------------------------------------------
%% Validation functions
%%------------------------------------------------------------------------------

%% @private
content(LookingFor, {Parent, _, Content}) ->
    content(LookingFor, Parent, Content, []).

%% @private
content([], _, [], Found) ->
    lists:reverse(Found);
content([], Parent, [{Element, _, _} | _], _) ->
    throw({unknown_element, parent_layer(Parent), Element});
content([{choice, Choices} | Rest], Parent, Content, Found) ->
    {Parsed, NewContent} = choice(Choices, Parent, Content),
    content(Rest, Parent, NewContent, [Parsed | Found]);
content([{Type, Name} | Rest] = List, Parent, Content, Found) ->
    case lists:keyfind(Name, 1, Content) of
        {Name, _, _} = Element ->
            Parsed = ?MODULE:Name(Element),
            NewContent = lists:keydelete(Name, 1, Content),
            case Type of
                list ->
                    content(List, Parent, NewContent, [Parsed | Found]);
                _Else ->
                    content(Rest, Parent, NewContent, [Parsed | Found])
            end;
        false ->
            case Type of
                required ->
                    throw({missing_element, layer(Name), Name});
                optional ->
                    content(Rest, Parent, Content, [undefined | Found]);
                {optional, Default} ->
                    content(Rest, Parent, Content, [Default | Found]);
                list ->
                    content(Rest, Parent, Content, Found)
            end
    end.

%% @private
choice(Choices, Parent, Content) ->
    choice(Choices, Parent, Content, undefined).

%% @private
choice([], Parent, _, undefined) ->
    throw({bad_element, layer(Parent), Parent});
choice([], _, Content, Found) ->
    {Found, Content};
choice(_, _, [], Found) ->
    {Found, []};
choice([Name | Rest], Parent, Content, Found) ->
    case lists:keyfind(Name, 1, Content) of
        {Name, _, _} = Element ->
            case Found of
                undefined ->
                    Parsed = ?MODULE:Name(Element),
                    NewContent = lists:keydelete(Name, 1, Content),
                    choice(Rest, Parent, NewContent, Parsed);
                _ ->
                    throw({bad_element, layer(Parent), Parent})
            end;
        false ->
            choice(Rest, Parent, Content, Found)
    end.

%% @private
attributes(LookingFor, {Element, Attributes, _}) ->
    NoNamespace = lists:keydelete(xmlns, 1, Attributes),
    attributes(LookingFor, Element, NoNamespace, []).

%% @private
attributes([], _, [], Found) ->
    lists:reverse(Found);
attributes([], Element, [{Attr, _} | _], _) ->
    throw({unknown_attribute, layer(Element), Element, Attr});
attributes([{required, Name} | Rest], Element, Attributes, Found) ->
    case lists:keyfind(Name, 1, Attributes) of
        {Name, Value} ->
            NewAttributes = lists:keydelete(Name, 1, Attributes),
            attributes(Rest, Element, NewAttributes, [Value | Found]);
        false ->
            throw({missing_attributes, layer(Element), Element, Name})
    end.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

%% @private
get_attr(Name, {Element, Attrs, _}, Required) ->
    case lists:keyfind(Name, 1, Attrs) of
        {Name, Value} ->
            Value;
        false ->
            case Required of
                required ->
                    throw({missing_attribute, layer(Element), Element, Name});
                optional ->
                    undefined
            end
    end.

%% @private
get_attr(Name, Element, ConvertTo, Required) ->
    Attr = get_attr(Name, Element, Required),
    convert(Attr, ConvertTo).

%% @private
get_text({_, _, [Content]}) when is_list(Content) ->
    Content;
get_text({Parent, _, _}) ->
    throw({bad_element, parent_layer(Parent), Parent}).

%% @private
get_text(Element, ConvertTo) ->
    Text = get_text(Element),
    convert(Text, ConvertTo).

%% @private
convert(undefined, _) ->
    undefined;
convert(Text, To) ->
    case To of
        integer ->
            list_to_integer(Text);
        atom ->
            list_to_atom(Text);
        capability ->
            convert_to_capability(Text)
    end.

%% @private
convert_to_capability(Text) ->
    case re:run(Text, ?CAPABILITY_RE) of
        {match, _} ->
            enetconf_capabilities:convert(Text);
        nomatch ->
            throw({bad_element, application, capability})
    end.

%% @private
layer(rpc) -> rpc;
layer('get-config') -> protocol;
layer('edit-config') -> protocol;
layer('copy-config') -> protocol;
layer('delete-config') -> protocol;
layer(lock) -> protocol;
layer(unlock) -> protocol;
layer(get) -> protocol;
layer('close-session') -> protocol;
layer('kill-session') -> protocol;
layer(_) -> application.

%% @private
parent_layer(rpc) -> protocol;
parent_layer(_) -> application.

%%------------------------------------------------------------------------------
%% To simple form functions
%%------------------------------------------------------------------------------

%% @doc Convert XML records returned by xmerl to a simple form tuples.
%% It will output only the xmlElement and xmlText records and skip all the
%% unnecessary whitespace xmlTexts. It will also throw an exception on xmlText
%% between xmlElements.
%% @private
to_simple_form(#xmlElement{name = Name,
                           attributes = Attrs,
                           content = Content}) ->
    {name_to_simple_form(Name), attributes_to_simple_form(Attrs),
     content_to_simple_form(Content)};
to_simple_form(#xmlText{value = Value}) ->
    RemovedNewlines = re:replace(Value, "[\n\r\t ]", "",
                                 [global, {return, list}]),
    case string:strip(RemovedNewlines) of
        "" ->
            [];
        String ->
            [String]
    end;
to_simple_form(Elements) when is_list(Elements) ->
    content_to_simple_form(Elements).

%% @doc Remove namespace prefix from XML elements.
%% @private
name_to_simple_form(Name) ->
    case re:split(atom_to_list(Name), ":", [{return, list}]) of
        [_Namespace, Element] ->
            list_to_atom(Element);
        [_Element] ->
            Name
    end.

%% @private
content_to_simple_form([#xmlText{} = Text]) ->
    to_simple_form(Text);
content_to_simple_form(Elements) ->
    content_to_simple_form(Elements, []).

%% @private
content_to_simple_form([], SimpleForms) ->
    lists:reverse(SimpleForms);
content_to_simple_form([#xmlElement{} = Element | Rest], SimpleForms) ->
    content_to_simple_form(Rest, [to_simple_form(Element) | SimpleForms]);
content_to_simple_form([#xmlText{} = Text | Rest], SimpleForms) ->
    case to_simple_form(Text) of
        [] ->
            content_to_simple_form(Rest, SimpleForms);
        [String] when is_list(String) ->
            case SimpleForms of
                [] ->
                    content_to_simple_form(Rest, [String | SimpleForms]);
                [Last | More] when is_list(Last) ->
                    content_to_simple_form(Rest, [Last ++ String | More]);
                _Else ->
                    throw(malformed_message)
            end;
        _Else ->
            throw(malformed_message)
    end;
content_to_simple_form([_ | Rest], SimpleForms) ->
    content_to_simple_form(Rest, SimpleForms).

%% @private
attributes_to_simple_form(Attrs) ->
    [{Name, Value} || #xmlAttribute{name = Name, value = Value} <- Attrs].

%%------------------------------------------------------------------------------
%% Back to xmerl records functions
%%------------------------------------------------------------------------------

%% @private
to_xmerl({Name, Attrs, Content}) ->
    #xmlElement{name = Name,
                namespace = #xmlNamespace{
                               default = 'urn:onf:of111:config:yang',
                               nodes = []},
                attributes = attributes_to_xmerl(Attrs, []),
                content = content_to_xmerl(Content, [])};
to_xmerl(Text) when is_list(Text) ->
    #xmlText{value = Text}.

%% @private
attributes_to_xmerl([], Attributes) ->
    lists:reverse(Attributes);
attributes_to_xmerl([{Name, Value} | Rest], Attributes) ->
    XmerlAttr = #xmlAttribute{name = Name,
                              value = Value},
    attributes_to_xmerl(Rest, [XmerlAttr | Attributes]).

%% @private
content_to_xmerl([], Content) ->
    lists:reverse(Content);
content_to_xmerl([Element | Rest], Content) ->
    content_to_xmerl(Rest, [to_xmerl(Element) | Content]).
