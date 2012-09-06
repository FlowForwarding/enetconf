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
%% @copyright 2012 FlowForwarding.org
%% @doc SSH Netconf backend module.
-module(enetconf_ssh).

%% API
-export([spawn_link/0]).

spawn_link() ->
    proc_lib:spawn_link(fun sshd_loop/0).

%%%===================================================================
%%% Internal functions
%%%===================================================================

sshd_loop() ->
    Data = io:get_line(''),
    io:put_chars(Data),
    sshd_loop().
