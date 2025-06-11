-module(user_service).
-behaviour(gen_server).
-include("include/domain.hrl").
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([find_by_id/1, save/1, create/4, delete/1]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.

find_by_id(Id) ->
    gen_server:call(?MODULE, {find_by_id, Id}).

save(#user{} = User) ->
	gen_server:call(?MODULE, {save, User}).

delete(UserId) ->
	gen_server:call(?MODULE, {delete, UserId}).

create(Name, Email, Password, Type) ->
    gen_server:call(?MODULE, {create, Name, Email, Password, Type}).


handle_call({save, #user{} = User}, _From, State) ->
	case validate(User) of
		ok ->
		    case mnesia:dirty_write(User) of
		        ok -> {reply, {ok, User}, State};
		        Error -> {reply, {error, Error}, State}
		    end;
		Error -> {reply, {error, Error}, State}
	end;

handle_call({delete, UserId}, _From, State) ->
	case mnesia:dirty_delete(user, UserId) of
		ok -> {reply, ok, State};
		Error -> {reply, {error, Error}, State}
	end; 

handle_call({create, Name, Email, Password, Type}, _From, State) ->
    UserId = support:generate_id(),
    PasswordHash = support:hash_password(Password),
    ApiKey = support:generate_uuid(),
    User = #user{
            id = UserId,
            name = Name,
            email = Email,
            password_hash = PasswordHash,
            type = Type,
            api_key = ApiKey,
            created_at = calendar:local_time()
            },
    handle_call({save, User}, _From, State);	

handle_call({find_by_id, Id}, _From, State) ->
    case mnesia:dirty_read(user, Id) of
        [User] -> {reply, {ok, User}, State};
        [] -> {reply, {error, not_found}, State}
    end.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

validate(#user{email = Email}) ->
	case mnesia:dirty_match_object(#user{email = Email, _ = '_'}) of
        [] ->
            ok;
        _ ->
            {error, user_already_exists}
    end.	