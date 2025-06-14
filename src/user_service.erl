-module(user_service).
-behaviour(gen_server).
-include("include/domain.hrl").
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([find_by_id/1, save/1, create/4, delete/1, count/0]).


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

count() ->
    gen_server:call(?MODULE, count).

handle_call({find_by_id, Id}, _From, State) ->
    case mnesia:dirty_read(user, Id) of
        [User] -> {reply, {ok, User}, State};
        [] -> {reply, {error, not_found}, State}
    end;

handle_call({save, #user{} = User}, _From, State) ->
    case internal_save(User) of
        ok -> {reply, {ok, User}, State};
        {validation, Validations} -> {reply, {validation, Validations}, State};
        {error, Reason} -> {reply, {error, Reason}, State}
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
    case internal_save(User) of
        ok -> {reply, {ok, User}, State};
        {validation, Validations} -> {reply, {validation, Validations}, State};
        {error, Reason} -> {reply, {error, Reason}, State}
    end;        

handle_call({delete, UserId}, _From, State) ->
    case mnesia:dirty_delete(user, UserId) of
        ok -> {reply, ok, State};
        Error -> {reply, {error, Error}, State}
    end;

handle_call(count, _From, State) ->
    Size = mnesia:table_info(user, size),
    {reply, {ok, Size}, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

internal_save(User = #user{}) ->
    case validate(User) of
        ok -> mnesia:dirty_write(User);
        Error -> Error
    end.    

validate(#user{email = Email}) ->    
    ToValidate = [{Email, "Informe o nome de usuário"}],    
    case validator:non_empty(ToValidate) of
        [] ->
            
            case mnesia:dirty_match_object(#user{email = Email, _ = '_'}) of
                [] ->
                    [];
                _ ->
                    [{error, "O nome de usuário jpa está sendo usado"}]
            end;
        Validations -> Validations
    end.