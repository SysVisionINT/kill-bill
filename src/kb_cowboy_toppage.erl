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

-module(kb_cowboy_toppage).

-behaviour(cowboy_handler).

-include("kill_bill.hrl").

-export([init/2, terminate/3]).

init(Data, Opts) ->
	ResourceServer = proplists:get_value(resource_server, Opts),
	TopPage = proplists:get_value(top_page, Opts),
	Context = proplists:get_value(context, Opts),
	SessionManager = proplists:get_value(session_manager, Opts),
	Static = proplists:get_value(static, Opts),
	BaseRequest = #kb_request{context=list_to_binary(Context), 
	                          resource_server=ResourceServer,
	                          session_manager=SessionManager,
	                          static=list_to_binary(Static)},
	Request = BaseRequest#kb_request{data=Data},
	Data2 = kb_template_util:execute(TopPage, Request),
	{ok, Data2, {BaseRequest, TopPage}}.

terminate(_Reason, _Data, _State) ->
	ok.