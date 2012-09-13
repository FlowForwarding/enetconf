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
        {ScannedXML, _Rest} = xmerl_scan:string(XML),

        %% Validated XML against the schema
        case xmerl_xsd:validate(ScannedXML, Schema) of
            {error, ValidationError} ->
                throw({error, {validate, ValidationError}});
            {ValidatedXML, _} ->
                %% Convert to simple form
                SimpleXML = enetconf_xml:to_simple_form(ValidatedXML),

                do_parse(SimpleXML)
        end
    catch
        _:{'EXIT', {fatal, {ScanError, _, _, _}}} ->
            throw({error, {scan, ScanError}});
        throw:{parse_error, ParseError} ->
            throw({error, {parse, ParseError}})
    end.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

%% @private
do_parse({rpc, Attrs, [Content]}) ->
    MessageId = get_attr('message-id', Attrs),
    Operation = operation(Content),
    {ok, #rpc{message_id = MessageId,
              operation = Operation}};
do_parse({hello, _, Content}) ->
    Capabilities = get_text_array(capabilities, Content),
    SessionId = get_text('session-id', Content, integer),
    {ok, #hello{capabilities = Capabilities,
                session_id = SessionId}}.

%% @private
operation({'edit-config', _, Content}) ->
    Target = target(get_child(target, Content)),
    Operation = get_text('default-operation', Content, atom),
    Test = get_text('test-option', Content, atom),
    Error = get_text('error-option', Content, atom),
    #edit_config{target = Target,
                 default_operation = Operation,
                 test_option = Test,
                 error_option = Error};
operation({'get-config', _, Content}) ->
    Source = get_config_source(get_child(source, Content)),
    Filter = filter(get_child(filter, Content)),
    #get_config{source = Source,
                filter = Filter};
operation({'copy-config', _, Content}) ->
    Source = source(get_child(source, Content)),
    Target = target(get_child(target, Content)),
    #copy_config{source = Source,
                 target = Target};
operation({'delete-config', _, [Content]}) ->
    #delete_config{target = target(Content)}.

%% @private
get_config_source({source, _, [{url, _, [Url]}]}) ->
    {url, Url};
get_config_source({source, _, [{Tag, _, _}]}) ->
    Tag.

%% @private
source({source, _, [{url, _, [Url]}]}) ->
    {url, Url};
source({source, _, [{config, _, [Config]}]}) ->
    Config;
source({source, _, [{Tag, _, _}]}) ->
    Tag.

%% @private
target({target, _, [{url, _, [Url]}]}) ->
    {url, Url};
target({target, _, [{Tag, _, _}]}) ->
    Tag.

%% @private
filter(undefined) ->
    undefined;
filter({filter, Attrs, Content}) ->
    Type = get_attr(type, Attrs, atom),
    Select = get_attr(select, Attrs),
    Subtree = get_child(Content),
    #filter{type = Type,
            select = Select,
            subtree = Subtree}.

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
    case lists:keyfind(Name, 1, Children) of
        false ->
            undefined;
        Child ->
            Child
    end.

%% @private
get_attr(Name, Attrs) ->
    case lists:keyfind(Name, 1, Attrs) of
        {Name, Value} ->
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
    case lists:keyfind(Name, 1, Content) of
        {Name, _, [Value]} ->
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
    {Name, List} = lists:keyfind(Name, 1, Content),
    lists:reverse(lists:foldl(fun({_, [Elem]}, Acc) -> 
                                      [Elem | Acc]
                              end, [], List)).

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
