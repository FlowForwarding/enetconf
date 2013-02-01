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
%% @doc Behaviour for handling NETCONF configurations.
%% @private
-module(gen_netconf).

-include("enetconf.hrl").

-type error() :: {error, Reason :: term()}.

-callback handle_get_config(SessionId :: integer(),
                            Source :: get_config_source(),
                            Filter :: filter()) -> {ok, Config :: xml()} |
                                                   error().

-callback handle_edit_config(SessionId :: integer(), Target :: target(),
                             Config :: config(),
                             DefaultOperation :: default_operation(),
                             ErrorOption :: error_option()) -> ok | error().

-callback handle_copy_config(SessionId :: integer(), Source :: source(),
                             Target :: target()) -> ok | error().

-callback handle_delete_config(SessionId :: integer(),
                               Target :: target()) -> ok | error().

-callback handle_lock(SessionId :: integer(),
                      Target :: target()) -> ok | error().

-callback handle_unlock(SessionId :: integer(),
                        Target :: target()) -> ok | error().

-callback handle_get(SessionId :: integer(),
                     Filter :: filter()) -> {ok, Config :: xml()} | error().
