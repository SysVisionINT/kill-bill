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

-module(kb_cowboy_toppage).

-behaviour(cowboy_http_handler).

-include("kill_bill.hlr").

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, Opts) ->
	ResourceServer = proplists:get_value(resource_server, Opts),
	TemplateConfig = proplists:get_value(template_config, Opts),
	Context = proplists:get_value(context, Opts),
	{ok, Req, {ResourceServer, TemplateConfig#template_config.top_page, list_to_binary(Context)}}.

handle(Req, {ResourceServer, TopPage, Context}) ->
	Req2 = kb_template_util:execute(Context, TopPage, ResourceServer, Req),
	{ok, Req2, {ResourceServer, TopPage, Context}}.

terminate(_Reason, _Req, _State) ->
	ok.