-module(pusherl_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    application:ensure_all_started(nitrogen_core),
    application:ensure_all_started(nitro_cache),
    application:ensure_all_started(crypto),
    application:ensure_all_started(nprocreg),
    application:ensure_all_started(simple_bridge),

    Children = [
            {auth_server, {auth_server, start_link, []}, permanent, 5000, worker, [auth_server]},
            {api_server, {api_server, start_link, []}, permanent, 5000, worker, [api_server]}
            ],
    {ok, {{one_for_one, 5, 10}, Children}}.
