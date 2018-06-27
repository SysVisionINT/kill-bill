# Changelog

## [1.2.0] - 2018-06-27

- Log requests for debugging purposes

### Changed

```erlang
- kb_cowboy_action:init(...) - check the flag before logging and add an unique id to the log string.
- kill_bill_sup:init(...) - load the flag from applications' env if exists
- kill_bill.erl - new function log_actions/2 to change the log flag
```

## [1.1.0] - 2017-12-18

- Upgraded to rebar3.
- Upgraded cowboy (tag 2.2.0) and erlydtl (tag 0.12.1).

### Changed
- The following functions no longer return the changed request

```erlang
kb_action_helper:get_accept_header
kb_action_helper:get_content_type
kb_action_helper:get_cookie
kb_action_helper:get_headers
kb_http:get_cookie
kb_http:get_header
kb_locale:get_accept_languages
kb_session:get_session_id
kb_session:get_session
kb_session:touch_session
```

- Renamed

```erlang
kb_session:touch_session(CacheName :: atom(), SessionID :: binary(), Data :: cowboy_req:req())
```
to

```erlang
kb_session:touch_session_id(CacheName :: atom(), SessionID :: binary())
```
- Renamed custom tag kb_dtl_tag:context to context_path

