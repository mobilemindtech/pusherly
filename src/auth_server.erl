-module(auth_server).
-behaviour(gen_server).
-include("include/domain.hrl").
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([authenticate/2, create_user/4, get_user/1, generate_api_key/1, validate_session/1, create_default_root_user/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% Criar usuário root padrão 
    %erlang:send_after(0, self(), create_default_root_user),
    {ok, #{}}.

authenticate(Email, Password) ->
    gen_server:call(?MODULE, {authenticate, Email, Password}).

create_user(Name, Email, Password, Type) ->
    gen_server:call(?MODULE, {create_user, Name, Email, Password, Type}).

get_user(UserId) ->
    gen_server:call(?MODULE, {get_user, UserId}).

generate_api_key(UserId) ->
    gen_server:call(?MODULE, {generate_api_key, UserId}).

validate_session(SessionId) ->
    gen_server:call(?MODULE, {validate_session, SessionId}).

handle_call({authenticate, Email, Password}, _From, State) ->
    case mnesia:dirty_match_object(#user{email = Email, _ = '_'}) of
        [User] ->

            logger:info("user found!"),

            case check_password(Password, User#user.password_hash) of
                true ->
                    SessionId = generate_session_id(),
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

handle_call({create_user, Name, Email, Password, Type}, _From, State) ->
    UserId = generate_id(),
    PasswordHash = hash_password(Password),
    ApiKey = generate_uuid(),
    User = #user{
            id = UserId,
            name = Name,
            email = Email,
            password_hash = PasswordHash,
            type = Type,
            api_key = ApiKey,
            created_at = calendar:local_time()
            },
    case mnesia:dirty_write(User) of
        ok -> {reply, {ok, User}, State};
        Error -> {reply, {error, Error}, State}
    end;

handle_call({get_user, UserId}, _From, State) ->
    case mnesia:dirty_read(user, UserId) of
        [User] -> {reply, {ok, User}, State};
        [] -> {reply, {error, not_found}, State}
    end;

handle_call({generate_api_key, UserId}, _From, State) ->
    case mnesia:dirty_read(user, UserId) of
        [User] ->
            NewApiKey = generate_uuid(),
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
            create_user("Root Admin", "admin@example.com", "admin123", root);
        _ ->
            ok
    end.

hash_password(Password) ->
    crypto:hash(sha256, Password).

check_password(Password, Hash) ->
    crypto:hash(sha256, Password) =:= Hash.

generate_id() ->
    erlang:integer_to_binary(erlang:unique_integer([positive])).

generate_session_id() ->
    generate_uuid().

generate_uuid() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", 
            [A, B, C, D, E])).