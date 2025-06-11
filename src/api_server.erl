-module(api_server).

-include("include/domain.hrl").

-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([register_app/3, send_notification/4]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.

register_app(Username, Token, AppName) ->
    gen_server:call(?MODULE, {register_app, Username, Token, AppName}).

send_notification(Username, Channel, Message, Token) ->
    gen_server:call(?MODULE, {send_notification, Username, Channel, Message, Token}).

handle_call({register_app, _Username, Token, AppName}, _From, State) ->
    case validate_api_token(Token) of
        {ok, User} ->
            AppId = generate_id(),
            App = #app{
                    id = AppId,
                    name = AppName,
                    user_id = User#user.id,
                    channels = [],
                    firebase_key = undefined,
                    apple_key = undefined,
                    created_at = calendar:local_time()
                    },
            case mnesia:dirty_write(App) of
                ok -> {reply, {ok, AppId}, State};
                Error -> {reply, {error, Error}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({send_notification, Username, Channel, Message, Token}, _From, State) ->
    case validate_api_token(Token) of
        {ok, User} ->
            {reply, {ok, notification_sent}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

validate_api_token(Token) ->
    case mnesia:dirty_match_object(#user{api_key = Token, _ = '_'}) of
        [User] -> {ok, User};
        [] -> {error, invalid_token}
    end.

generate_id() ->
    erlang:integer_to_binary(erlang:unique_integer([positive])).