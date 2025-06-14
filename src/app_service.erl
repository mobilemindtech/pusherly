-module(app_service).
-behaviour(gen_server).
-include("include/domain.hrl").
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([find_by_id/2, save/1, create/5, delete/2, list/1, count/1, count_all/0]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.

find_by_id(AppId, UserId) ->
    gen_server:call(?MODULE, {find_by_id, AppId, UserId}).

save(#app{} = App) ->
    gen_server:call(?MODULE, {save, App}).

delete(AppId, UserId) ->
    gen_server:call(?MODULE, {delete, AppId, UserId}).

create(Name, Channels, FirebaseKey, AppleKey, UserId) ->
    gen_server:call(?MODULE, {create, Name, Channels, FirebaseKey, AppleKey, UserId}).

list(UserId) ->
    gen_server:call(?MODULE, {list, UserId}).

count(UserId) ->
    gen_server:call(?MODULE, {count, UserId}).

count_all() ->
    gen_server:call(?MODULE, count_all).

handle_call({find_by_id, AppId, UserId}, _From, State) ->
    case internal_find_by_id(AppId, UserId) of
        {ok, App} -> {reply, {ok, App}, State};
        {error, Reason} -> {reply, {error, Reason}, State}
    end;

handle_call({delete, AppId, UserId}, _From, State) ->
    case internal_find_by_id(AppId, UserId) of
        {ok, App} ->
            case mnesia:dirty_delete_object(App) of
                ok -> {reply, ok, State};
                Error -> {reply, {error, Error}, State}
            end; 
        Error -> {reply, {error, Error}, State}
    end;

handle_call({create, Name, Channels, FirebaseKey, AppleKey, UserId}, _From, State) ->
    AppId = support:generate_id(),    
    App = #app{
            id = AppId,
            name = Name,
            user_id = UserId,
            channels = Channels,
            firebase_key = case FirebaseKey of "" -> undefined; _ -> FirebaseKey end,
            apple_key = case AppleKey of "" -> undefined; _ -> AppleKey end,
            created_at = calendar:local_time()
            },
    case internal_save(App) of
        ok -> {reply, {ok, App}, State};
        {validation, Validations} -> {reply, {validation, Validations}, State};
        Error -> {reply, {error, Error}, State}
    end;        

handle_call({save, #app{} = App}, _From, State) ->
    case internal_save(App) of
        ok -> {reply, {ok, App}, State};
        {validation, Validations} -> {reply, {validation, Validations}, State};
        Error -> {reply, {error, Error}, State}
    end; 

handle_call({list, UserId}, _From, State) ->
    case mnesia:dirty_match_object(#app{ user_id = UserId, _ = '_' }) of
        {aborted, Reason} -> {reply, {error, Reason}, State};
        Results -> {reply, {ok, Results}, State}
    end;


handle_call({count, UserId}, _From, State) ->
    {atomic, Count} = mnesia:transaction(fun() ->
                    mnesia:foldl(
                        fun(#app{user_id = UID}, Acc) when UID =:= UserId ->
                                Acc + 1;
                            (_, Acc) ->
                                Acc
                        end,
                        0,
                        app
                        )
            end),
    {reply, {ok, Count}, State};

handle_call(count_all, _From, State) ->
    Size = mnesia:table_info(app, size),
    {reply, {ok, Size}, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

internal_save(App = #app{}) ->
    case validate(App) of
        [] -> mnesia:dirty_write(App);
        Validations -> {validation, Validations}
    end.    

internal_find_by_id(AppId, UserId) ->
    case mnesia:dirty_match_object(#app{ id = AppId, user_id = UserId, _ = '_' }) of
        [App] -> {ok, App};
        [] -> {error, not_found}
    end.


validate(#app{name = Name, user_id = UserId}) ->    
    ToValidate = [{Name, "Informe um nome para o canal"}],    
    case validator:non_empty(ToValidate) of
        [] ->
            
            case mnesia:dirty_match_object(#app{name = Name, user_id = UserId , _ = '_'}) of
                [] ->
                    [];
                _ ->
                    [{error, "O nome do app já está sendo usado"}]
            end;
        Validations -> Validations
    end.
