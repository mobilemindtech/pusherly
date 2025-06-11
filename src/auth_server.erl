-module(auth_server).
-behaviour(gen_server).
-include("include/domain.hrl").
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([authenticate/2, generate_api_key/1, validate_session/1, create_default_root_user/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% Criar usuário root padrão 
    %erlang:send_after(0, self(), create_default_root_user),
    {ok, #{}}.

authenticate(Email, Password) ->
    gen_server:call(?MODULE, {authenticate, Email, Password}).


generate_api_key(UserId) ->
    gen_server:call(?MODULE, {generate_api_key, UserId}).

validate_session(SessionId) ->
    gen_server:call(?MODULE, {validate_session, SessionId}).

handle_call({authenticate, Email, Password}, _From, State) ->
    case mnesia:dirty_match_object(#user{email = Email, _ = '_'}) of
        [User] ->

            logger:info("user found!"),

            case support:check_password(Password, User#user.password_hash) of
                true ->
                    SessionId = support:generate_session_id(),
                    ExpiresAt = calendar:datetime_to_gregorian_seconds(calendar:local_time()) + 3600 * 24,
                    Session = #session{
                            session_id = SessionId,
                            user_id = User#user.id,
                            created_at = calendar:local_time(),
                            expires_at = ExpiresAt
                            },
                    mnesia:dirty_write(Session),
                    {reply, {ok, SessionId, User}, State};
                false ->
                    logger:info("user ~p NOT found!", [Email]),
                    {reply, {error, invalid_credentials}, State}
            end;
        [] ->
            logger:info("user ~p NOT found!", [Email]),
            {reply, {error, user_not_found}, State}
    end;



handle_call({generate_api_key, UserId}, _From, State) ->
    case mnesia:dirty_read(user, UserId) of
        [User] ->
            NewApiKey = support:generate_uuid(),
            UpdatedUser = User#user{api_key = NewApiKey},
            mnesia:dirty_write(UpdatedUser),
            {reply, {ok, NewApiKey}, State};
        [] ->
            {reply, {error, user_not_found}, State}
    end;

handle_call({validate_session, SessionId}, _From, State) ->
    case mnesia:dirty_read(session, SessionId) of
        [Session] ->
            Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
            if Session#session.expires_at > Now ->
                    case mnesia:dirty_read(user, Session#session.user_id) of
                        [User] -> {reply, {ok, User}, State};
                        [] -> {reply, {error, user_not_found}, State}
                    end;
                true ->
                    mnesia:dirty_delete(session, SessionId),
                    {reply, {error, session_expired}, State}
            end;
        [] ->
            {reply, {error, invalid_session}, State}
    end.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% 
create_default_root_user() ->
    case mnesia:dirty_match_object(#user{type = root, _ = '_'}) of
        [] ->
            user_service:create_user("Root Admin", "admin@example.com", "admin123", root);
        _ ->
            ok
    end.

