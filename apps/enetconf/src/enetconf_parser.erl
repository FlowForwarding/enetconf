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

-include_lib("xmerl/include/xmerl.hrl").
-include("enetconf.hrl").

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

%% @doc Parse incoming XML using NETCONF's XML schema.
-spec parse(string()) -> ok | {error, Reason :: term()}.
parse(XML) ->
    %% Get the schema
    [{schema, Schema}] = ets:lookup(enetconf, schema),

    try
        %% Scan the XML
        {ScannedXML, _Rest} = xmerl_scan:string(XML, [{quiet, true}]),

        %% Validated XML against the schema
        case xmerl_xsd:validate(ScannedXML, Schema) of
            {error, Reason} ->
                SimpleReason = simplify_error(Reason),
                {error, SimpleReason};
            {ValidatedXML, _} ->
                do_parse(process(ValidatedXML))
        end
    catch
        exit:{fatal, {_, _, _, _}} ->
            {error, malformed_message};
        throw:{parse_error, ParseError} ->
            {error, ParseError}
    end.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

%% @private
do_parse(#xmlElement{name = rpc, attributes = Attrs, content = [Content]}) ->
    MessageId = get_attr('message-id', Attrs),
    Operation = operation(Content),
    {ok, #rpc{message_id = MessageId,
              operation = Operation}};
do_parse(#xmlElement{name = hello, content = Content}) ->
    Capabilities = get_text_array(capabilities, Content),
    SessionId = get_text('session-id', Content, integer),
    {ok, #hello{capabilities = Capabilities,
                session_id = SessionId}}.

%% @private
operation(#xmlElement{name = 'edit-config', content = Content}) ->
    Target = target(get_child(target, Content)),
    Operation = get_text('default-operation', Content, atom),
    Test = get_text('test-option', Content, atom),
    Error = get_text('error-option', Content, atom),
    Config = config(get_child(config, Content)),
    #edit_config{target = Target,
                 default_operation = Operation,
                 test_option = Test,
                 error_option = Error,
                 config = Config};
operation(#xmlElement{name = 'get-config', content = Content}) ->
    Source = get_config_source(get_child(source, Content)),
    Filter = filter(get_child(filter, Content)),
    #get_config{source = Source,
                filter = Filter};
operation(#xmlElement{name = 'copy-config', content = Content}) ->
    Source = source(get_child(source, Content)),
    Target = target(get_child(target, Content)),
    #copy_config{source = Source,
                 target = Target};
operation(#xmlElement{name = 'delete-config', content = [Content]}) ->
    #delete_config{target = target(Content)};
operation(#xmlElement{name = lock, content = [Content]}) ->
    #lock{target = target(Content)};
operation(#xmlElement{name = unlock, content = [Content]}) ->
    #unlock{target = target(Content)};
operation(#xmlElement{name = get, content = Content}) ->
    #get{filter = filter(get_child(filter, Content))};
operation(#xmlElement{name = 'close-session'}) ->
    #close_session{};
operation(#xmlElement{name = 'kill-session', content = Content}) ->
    #kill_session{session_id = get_text('session-id', Content, integer)}.

%% @private
get_config_source(#xmlElement{name = source,
                              content = [#xmlElement{name = url,
                                                     content = [Content]}]}) ->
    #xmlText{value = Url} = Content,
    {url, Url};
get_config_source(#xmlElement{name = source,
                              content = [#xmlElement{name = Tag}]}) ->
    case Tag of
        url ->
            throw({parse_error, {bad_element, url}});
        _ ->
            Tag
    end.

%% @private
source(#xmlElement{name = source,
                   content = [#xmlElement{name = url,
                                          content = [Content]}]}) ->
    #xmlText{value = Url} = Content,
    {url, Url};
source(#xmlElement{name = source,
                   content = [#xmlElement{name = config,
                                          content = [Config]}]}) ->
    {xml, Config};
source(#xmlElement{name = source,
                   content = [#xmlElement{name = Tag}]}) ->
    case Tag of
        url ->
            throw({parse_error, {bad_element, url}});
        _ ->
            Tag
    end.

%% @private
target(#xmlElement{name = target,
                   content = [#xmlElement{name = url,
                                          content = [Content]}]}) ->
    #xmlText{value = Url} = Content,
    {url, Url};
target(#xmlElement{name = target,
                   content = [#xmlElement{name = Tag}]}) ->
    case Tag of
        url ->
            throw({parse_error, {bad_element, url}});
        _ ->
            Tag
    end.

%% @private
filter(undefined) ->
    undefined;
filter(#xmlElement{name = filter, attributes = Attrs, content = Content}) ->
    Type = get_attr(type, Attrs, atom),
    case Type of
        subtree ->
            %% FIXME: Impossible with current schema. Will crash.
            Subtree = get_child(Content),
            {subtree, Subtree};
        xpath ->
            case get_attr(select, Attrs) of
                undefined ->
                    throw({parse_error,
                           {missing_attribute, select, filter}});
                Select ->
                    {xpath, Select}
            end
    end.

%% @private
config(#xmlElement{name = config,
                   content = [#xmlElement{name = url,
                                          content = [Content]}]}) ->
    #xmlText{value = Url} = Content,
    {url, Url};
config(#xmlElement{name = config, content = [Content]}) ->
    case Content of
        url ->
            throw({parse_error, {bad_element, url}});
        {url, _} ->
            throw({parse_error, {bad_element, url}});
        {url, _, _} ->
            throw({parse_error, {bad_element, url}});
        _ ->
            {xml, Content}
    end.

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

%% @private
get_child([]) ->
    undefined;
get_child([Child]) ->
    Child.

%% @private
get_child(Name, Children) ->
    case lists:keyfind(Name, #xmlElement.name, Children) of
        false ->
            undefined;
        Child ->
            Child
    end.

%% @private
get_attr(Name, Attrs) ->
    case lists:keyfind(Name, #xmlAttribute.name, Attrs) of
        #xmlAttribute{name = Name, value = Value} ->
            Value;
        false ->
            undefined
    end.

%% @private
get_attr(Name, Attrs, ConvertTo) ->
    Attr = get_attr(Name, Attrs),
    convert(Attr, ConvertTo).

%% @private
get_text(Name, Content) ->
    case lists:keyfind(Name, #xmlElement.name, Content) of
        #xmlElement{name = Name, content = [#xmlText{value = Value}]} ->
            Value;
        false ->
            undefined
    end.

%% @private
get_text(Name, Content, ConvertTo) ->
    Text = get_text(Name, Content),
    convert(Text, ConvertTo).

%% @private
get_text_array(Name, Content) ->
    #xmlElement{content = List} = lists:keyfind(Name, #xmlElement.name,
                                                Content),
    Fun = fun(#xmlElement{content = [#xmlText{value = Value}]}, Acc) ->
                  [Value | Acc]
          end,
    lists:reverse(lists:foldl(Fun, [], List)).

%% @private
convert(Text, To) ->
    case Text of
        undefined ->
            undefined;
        Text ->
            case To of
                string ->
                    Text;
                integer ->
                    list_to_integer(Text);
                atom ->
                    list_to_atom(Text)
            end
    end.

%% @private
process(#xmlElement{attributes = Attrs, content = Content} = XML) ->
    XML#xmlElement{attributes = attributes(Attrs, []),
                   content = content(Content, [])};
process(#xmlText{value = Value} = XML) ->
    XML#xmlText{value = string:strip(Value)}.

%% @private
attributes([], Attrs) ->
    lists:reverse(Attrs);
attributes([#xmlAttribute{value = Value} = Attr | Rest], Attrs) ->
    NewAttr = Attr#xmlAttribute{value = string:strip(Value)},
    attributes(Rest, [NewAttr | Attrs]).

%% @private
content([], Content) ->
    lists:reverse(Content);
content([#xmlElement{} = Element | Rest], Content) ->
    content(Rest, [process(Element) | Content]);
content([#xmlText{} = Text | Rest], Content) ->
    NewText = process(Text),
    case NewText#xmlText.value of
        "" ->
            content(Rest, Content);
        _Else ->
            content(Rest, [NewText | Content])
    end;
content([_ | Rest], Content) ->
    content(Rest, Content).

%% @private
simplify_error([{_, _, {element_not_in_schema, [Element, _, _]}} | _]) ->
    {unknown_element, Element};
simplify_error([{_, _, {required_attribute_missed, #xmlElement{name = Element},
                        {Attribute, _, _}}} | _]) ->
    {missing_attribute, Attribute, Element};
simplify_error([{_, _, {missing_mandatory_elements,
                        {_, {{Element, _, _}, _}}}} | _]) ->
    case Element of
        rpcOperation ->
            %% Note: As returing a bad-element is mandatory and there is more
            %%       than one operation possible here, let's take a random one.
            {missing_element, 'get-config'};
        _ ->
            {missing_element, Element}
    end;
simplify_error([{_, _, {empty_content_not_allowed,
                        [{_, {[{_, {{Element, _, _}, _}} | _], _}}
                         | _]}} | _]) ->
    {missing_element, Element};
simplify_error([{_, _, {cannot_contain_text,
                        #xmlText{parents = [{Element, _} | _]}, _}} | _]) ->
    {bad_element, Element};
simplify_error([{_, _, {no_element_matching_choice,
                        [#xmlElement{parents = [{Element, _} | _]}
                         | _]}} | _]) ->
    {bad_element, Element};
simplify_error([{value_not_anyURI, _} | _]) ->
    {bad_element, url};
simplify_error([{_, _, {match_failure,
                        [#xmlElement{parents = [{Element, _} | _]} | _],
                        _, _}} | _]) ->
    {bad_element, Element};
simplify_error([{_, _, {match_failure,
                        [#xmlText{parents = [{Element, _} | _]} | _],
                        _, _}} | _]) ->
    {bad_element, Element};
simplify_error([{_, _, {unexpected_rest,
                        [#xmlElement{parents = [{Element, _} | _]}
                         | _]}} | _]) ->
    {bad_element, Element};
simplify_error(_Else) ->
    invalid_value.
