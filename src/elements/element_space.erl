%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_space).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
    ]).


-spec reflect() -> [atom()].
reflect() -> record_info(fields, space).

-spec render_element(#space{}) -> body().
render_element(_Record = #space{}) -> "&nbsp".
