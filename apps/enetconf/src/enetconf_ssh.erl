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

-behaviour(ssh_channel).

-include_lib("enetconf/include/enetconf.hrl").

%% API
-export([]).

%% ssh_channel callbacks
-export([init/1,
         handle_msg/2,
         handle_ssh_msg/2,
         terminate/2]).

-record(state, {
          channel_id :: ssh_channel:channel_id(),
          connection_ref :: sh_channel:connection_ref()
         }).

-define(DATA_TYPE_CODE, 0).

init(_) ->
    {ok, #state{}}.

handle_msg({ssh_channel_up, ChannelId, ConnRef}, State) ->
    ?INFO("SSH channel up. Id: ~p  Ref: ~p", [ChannelId, ConnRef]),
    {ok, State#state{channel_id = ChannelId,
                     connection_ref= ConnRef}};
handle_msg({'EXIT', Reason}, #state{channel_id = ChannelId} = State) ->
    ?INFO("Channel ~p exit with reason : ~p",
          [ChannelId, Reason]),
    {stop, ChannelId, State}.

handle_ssh_msg({ssh_cm, ConnRef, {data, ChannelId,
                             ?DATA_TYPE_CODE,  Data}},
               #state{connection_ref= ConnRef, channel_id = ChannelId} = State) ->
    ?INFO("Handle SSH msg: ~p", [Data, State]),
    ssh_connection:send(ConnRef, ChannelId, Data),
    {ok, State};
handle_ssh_msg({ssh_cm, ConnRef, {eof, ChannelId}},
               #state{connection_ref= ConnRef, channel_id = ChannelId} = State) ->
    {ok, State};
handle_ssh_msg({ssh_cm, ConnRef, {signal, ChannelId, _Signal}},
               #state{connection_ref= ConnRef, channel_id = ChannelId} = State) ->
    {ok, State};
handle_ssh_msg({ssh_cm, ConnRef, {exit_signal, ChannelId,
                                  _ExitSignal, _ErrorMsg, _LanguageString}},
               #state{connection_ref= ConnRef, channel_id = ChannelId} = State) ->
    {ok, State};
handle_ssh_msg({ssh_cm, ConnRef, {exit_status, ChannelId, _ExitStatus}},
               #state{connection_ref= ConnRef, channel_id = ChannelId} = State) ->
    {ok, State}.

terminate(Subsystem, State) ->
    ?INFO("SSH connection with subsystem: ~p terminated with state: ~p",
          [Subsystem, State]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
