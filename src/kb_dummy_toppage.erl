%%
%% Copyright 2013 Joaquim Rocha
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% Modifications copyright (C) 2017 Sysvision, Lda.
%%

-module(kb_dummy_toppage).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Data, Opts) ->
	ServerName = proplists:get_value(server, Opts),
	Host = proplists:get_value(host, Opts),
	Output = io_lib:format("Kill Bill server ~p running for host [~p]", [ServerName, Host]),
	Data2 = cowboy_req:reply(200, #{}, Output, Data),
	{ok, Data2, {ServerName, Host}}.

terminate(_Reason, _Data, _State) ->
	ok.