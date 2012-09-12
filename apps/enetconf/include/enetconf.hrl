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

-define(INFO(Msg, Args), error_logger:info_msg(Msg, Args)).
-define(WARNING(Msg, Args), error_logger:warning_msg(Msg, Args)).
-define(ERROR(Msg, Args), error_logger:error_msg(Msg, Args)).

-define(BASE_CAPABILITY, "urn:ietf:params:netconf:base:1.1").
-define(BASE_CAPABILITY_OLD, "urn:ietf:params:netconf:base:1.0").

-type config() :: startup
                | candidate
                | running.

-type filter() :: subtree
                | xpath.

-type default_operation() :: merge
                           | replace
                           | none.

-type test_option() :: 'test-then-set'
                     | set.

-type error_option() :: 'stop-on-error'
                      | 'continue-on-error'
                      | 'rollback-on-error'.

-record(filter, {
          type = subtree :: filter(),
          select :: string() | undefined
         }).

-record(edit_config, {
          target :: config(),
          default_operation = merge :: default_operation(),
          test_option :: test_option(),
          error_option :: error_option()
         }).

-record(get_config, {
          source = running :: config(),
          filter :: #filter{} | undefined
         }).

-record(copy_config, {
          source :: config() | {url, string()},
          target :: config()
         }).

-record(delete_config, {
          target :: config()
         }).

-type operation() :: #edit_config{}
                   | #get_config{}
                   | #copy_config{}
                   | #delete_config{}.

-record(rpc, {
          message_id :: string(),
          operation :: operation()
         }).

-record(rpc_reply, {
          message_id :: string()
         }).

-type rpc() :: #rpc{}
             | #rpc_reply{}.

-record(hello, {
          capabilities = [] :: [string()],
          session_id :: integer() | undefined
         }).
