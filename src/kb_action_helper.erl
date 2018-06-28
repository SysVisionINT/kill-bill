%%
%% Copyright 2013-14 Joaquim Rocha
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
%% Modifications copyright (C) 2017-18 Sysvision, Lda.
%%

-module(kb_action_helper).

-include("kill_bill.hrl").

-define(CONTENT_TYPE_HEADER, <<"content-type">>).
-define(ACCEPT_HEADER, <<"accept">>).
-define(FORM_CONTENT_TYPE, <<"application/x-www-form-urlencoded">>).
-define(JSON_CONTENT_TYPE, <<"application/json">>).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	get_context/1,
	get_action_prefix/1,
	get_headers/1,
	get_content_type/1,
	get_accept_header/1,
	get_args/1, 
	get_json/1, 
	get_body/1,
	get_session/1,
	set_session/2,
	get_attributes/1,
	invalidate_session/1,
	set_cookie/4, 
	get_cookie/2,
	get_locales/1,
	set_locale/2,
	remove_locale/1,
	best_locale/3,
	get_message/2,
	get_message/3
	]).

-spec get_context(Req :: #kb_request{}) -> binary().
get_context(Req) ->
	Req#kb_request.context.

-spec get_action_prefix(Req :: #kb_request{}) -> binary().
get_action_prefix(Req) ->
	Req#kb_request.action_prefix.

-spec get_headers(Req :: #kb_request{}) -> #{binary() => binary()}.
get_headers(Req) ->
	cowboy_req:headers(Req#kb_request.data).

-spec get_content_type(Req :: #kb_request{}) -> binary().
get_content_type(Req) ->
	Headers = get_headers(Req),
	case maps:find(?CONTENT_TYPE_HEADER, Headers) of
		{ok, Content} -> Content;
		_ -> <<>>
	end.

-spec get_accept_header(Req :: #kb_request{}) -> [binary(), ...].
get_accept_header(Req) ->
	case cowboy_req:parse_header(?ACCEPT_HEADER, Req#kb_request.data, <<"*">>) of
		<<"*">> -> [<<"*">>];
		undefined -> [<<"*">>];
		Accept -> prioritize_accept(Accept)
	end.

-spec get_args(Req :: #kb_request{}) -> {Args, #kb_request{}}
	when Args :: [{binary(), binary()}, ...].
get_args(Req=#kb_request{log_id=LogID, log_actions=LogFlag, method=Method, data=Data}) ->
	QSVals = cowboy_req:parse_qs(Data),
	{BodyQS1, Data2} = case Method of
		<<"POST">> ->
			{ok, BodyQS, Data1} = cowboy_req:read_urlencoded_body(Data),
			kb_util:log(LogFlag, LogID, "get_args: BodyQS", BodyQS),
			{BodyQS, Data1};
		_ -> {[], Data}
	end,
	QS = mergeQS(QSVals, BodyQS1, []),
	{QS, Req#kb_request{data=Data2}}.

-spec get_json(Req :: #kb_request{}) -> {jsx:json_term(), #kb_request{}}.
get_json(Req=#kb_request{log_id=LogID, log_actions=LogFlag, method=Method, data=Data}) ->
	case Method of
		<<"GET">> ->
			{Args, Req1} = get_args(Req),
			JSon = kb_json:from_proplist(Args),
			{JSon, Req1};
		_ ->
			ContentType = get_content_type(Req),
			case mime_type(ContentType) of
				?JSON_CONTENT_TYPE ->
					{ok, Body, Data1} = cowboy_req:read_body(Data),
					kb_util:log(LogFlag, LogID, "get_json: Body", Body),
					JSon = kb_json:decode(Body),
					{JSon, Req#kb_request{data=Data1}};
				?FORM_CONTENT_TYPE ->
					{ok, BodyQS, Data1} = cowboy_req:read_urlencoded_body(Data),
					kb_util:log(LogFlag, LogID, "get_json: BodyQS", BodyQS),
					JSon = jsondoc:from_proplist(BodyQS),
					{JSon, Req#kb_request{data=Data1}};
				_ -> {[], Req}
			end
	end.

-spec get_body(Req :: #kb_request{}) -> {binary(), #kb_request{}}.
get_body(Req=#kb_request{log_id=LogID, log_actions=LogFlag, data=Data}) ->
	{ok, Body, Data1} = cowboy_req:read_body(Data),
	kb_util:log(LogFlag, LogID, "get_body: Body", Body),
	{Body, Req#kb_request{data=Data1}}.

-spec get_session(Req :: #kb_request{}) -> {SessionData, #kb_request{}}
	when SessionData :: [{any(), any()}, ...].
get_session(Req) ->
	kb_session_util:get_user_data(Req).

-spec set_session(SessionData, Req :: #kb_request{}) -> #kb_request{}
	when SessionData :: [{any(), any()}, ...].
set_session(UserData, Req) ->
	kb_session_util:set_user_data(UserData, Req).

-spec get_attributes(Req :: #kb_request{}) -> list().
get_attributes(Req) -> Req#kb_request.attributes.

-spec set_cookie(Name :: binary(), Value :: term(), MaxAge :: integer(), Req :: #kb_request{}) -> #kb_request{}.
set_cookie(Name, Value, MaxAge, Req) when is_binary(Value) ->
	Data1 = kb_http:set_cookie(Req#kb_request.context, Name, Value, MaxAge, Req#kb_request.data),
	Req#kb_request{data=Data1};
set_cookie(Name, Value, MaxAge, Req) ->
	set_cookie(Name, term_to_binary(Value), MaxAge, Req).

-spec invalidate_session(Req :: #kb_request{}) -> #kb_request{}.
invalidate_session(Req) ->
	case Req#kb_request.session_key of
		none -> 
			{ok, Data1} = kb_session:invalidate_session(Req#kb_request.session_manager, Req#kb_request.data),
			Req#kb_request{session_saved=yes, data=Data1};
		SessionID -> 
			{ok, Data1} = kb_session:invalidate_session(Req#kb_request.session_manager, SessionID, Req#kb_request.data),
			Req#kb_request{session_saved=yes, data=Data1}
	end.	

-spec get_cookie(Name :: binary(), Req :: #kb_request{}) -> binary().
get_cookie(Name, Req) ->
	kb_http:get_cookie(Name, Req#kb_request.data).

-spec get_locales(Req :: #kb_request{}) -> {any_locale | Locales, #kb_request{}}
	when Locales :: [Locale, ...],
	Locale :: {Language :: binary(), Country :: binary()}.
get_locales(Req) ->
	kb_locale:get_locales(Req).				

-spec set_locale(Locale, Req :: #kb_request{}) -> #kb_request{}
	when Locale :: {Language :: binary(), Country :: binary()}.
set_locale(Locale, Req) ->
	kb_locale:set_locale(Locale, Req).

-spec remove_locale(Req :: #kb_request{}) -> #kb_request{}.
remove_locale(Req) -> kb_locale:remove_locale(Req).

-spec get_message(MsgId :: iolist(), Req :: #kb_request{}) -> {Response, #kb_request{}}
	when Response :: no_resource
	| message_not_found
	| iolist().
get_message(MsgId, Req) ->
	kb_resource_util:get_message(MsgId, Req).

-spec get_message(MsgId :: iolist(), Args, Req :: #kb_request{}) -> {Response, #kb_request{}}
	when Args :: [Arg, ...],
	Arg :: {Search :: binary(), Replace :: binary()},
	Response :: no_resource
	| message_not_found
	| iolist().
get_message(MsgId, Args, Req) ->
	kb_resource_util:get_message(MsgId, Args, Req).

-spec best_locale(List :: list(), Default :: Locale, Req :: #kb_request{}) -> {Locale, #kb_request{}}
	when Locale :: {Language :: binary(), Country :: binary()}.
best_locale(List, Default, Req) ->
	{Locales, Req1} = get_locales(Req),
	Best = best(Locales, List, Default),
	{Best, Req1}.

%% ====================================================================
%% Internal functions
%% ====================================================================

mime_type(ContentType) ->
	Parts = binary:split(ContentType, <<";">>),
	[Mime|_] = Parts,
	Mime.

best(any_locale, _List, Default) -> Default;
best(Locales, List, Default) -> 
	Valid = lists:filter(fun(Locale = {Languale, _}) ->
					case lists:member(Locale, List) of
						true -> true;
						false -> lists:member({Languale, ?NO_COUNTRY_IN_LOCALE}, List)
					end
			end, Locales),
	case Valid of
		[] -> Default;
		[Locale|_] -> Locale
	end.

mergeQS([H={Key, _}|T], BodyQS, Output) ->
	{KV, BQS1} = case lists:keyfind(Key, 1, BodyQS) of
		{_, Value} -> 
			BQS = lists:keydelete(Key, 1, BodyQS),
			{{Key, Value}, BQS};
		false -> {H, BodyQS}
	end,
	mergeQS(T, BQS1, [KV|Output]);
mergeQS([], BodyQS, Output) -> BodyQS ++ Output.

%%
% Copied from https://github.com/ninenines/cowboy/blob/1.0.x/src/cowboy_rest.erl
prioritize_accept(Accept) ->
	Ordered = lists:sort(
		fun ({MediaTypeA, Quality, _AcceptParamsA}, {MediaTypeB, Quality, _AcceptParamsB}) -> prioritize_mediatype(MediaTypeA, MediaTypeB);
			({_MediaTypeA, QualityA, _AcceptParamsA}, {_MediaTypeB, QualityB, _AcceptParamsB}) -> QualityA > QualityB
		end, Accept),
	lists:map(fun({{Type, SubType, _Params}, _Quality, _AcceptParams}) -> 
			<<Type/binary, $/, SubType/binary>>
		end, Ordered).

prioritize_mediatype({TypeA, SubTypeA, ParamsA}, {TypeB, SubTypeB, ParamsB}) ->
	case TypeB of
		TypeA ->
			case SubTypeB of
				SubTypeA -> length(ParamsA) > length(ParamsB);
				<<"*">> -> true;
				_Any -> false
			end;
		<<"*">> -> true;
		_Any -> false
	end.
%%
