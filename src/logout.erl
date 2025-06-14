-module(logout).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").


main() -> 
	wf:clear_session(),
	wf:redirect("/index").