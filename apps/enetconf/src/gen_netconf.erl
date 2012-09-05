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
%% @doc Behaviour for handling custom NETCONF configurations.
%% @private
-module(gen_netconf).

-callback handle_get_config(Source :: atom()) -> ok.

-callback handle_edit_config(Target :: atom(), XmlConfig :: tuple()) -> ok.

-callback handle_delete_config(Target :: atom()) -> ok.

-callback handle_copy_config(Source :: atom(), Target :: atom()) -> ok.

%% Other operations
%% -callback handle_lock() -> ok.
%% -callback handle_unlock() -> ok.
%% -callback handle_get() -> ok.
%% -callback handle_close_session() -> ok.
%% -callback handle_kill_session() -> ok.
