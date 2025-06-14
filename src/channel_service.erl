-module(channel_service).
-behaviour(gen_server).
-include("include/domain.hrl").
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([find_by_id/2, save/1, create/3, delete/2, list/1, count/1, count_all/0]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.

find_by_id(ChannelId, UserId) ->
    gen_server:call(?MODULE, {find_by_id, ChannelId, UserId}).

save(#channel{} = Channel) ->
    gen_server:call(?MODULE, {save, Channel}).

delete(ChannelId, UserId) ->
    gen_server:call(?MODULE, {delete, ChannelId, UserId}).

create(Name, Description, UserId) ->
    gen_server:call(?MODULE, {create, Name, Description, UserId}).

list(UserId) ->
    gen_server:call(?MODULE, {list, UserId}).

count(UserId) ->
    gen_server:call(?MODULE, {count, UserId}).

count_all() ->
    gen_server:call(?MODULE, count_all).

handle_call({find_by_id, ChannelId, UserId}, _From, State) ->
    case internal_find_by_id(ChannelId, UserId) of
        {ok, Channel} -> {reply, {ok, Channel}, State};
        {error, Reason} -> {reply, {error, Reason}, State}
    end;

handle_call({delete, ChannelId, UserId}, _From, State) ->
    case internal_find_by_id(ChannelId, UserId) of
        {ok, Channel} ->
            case mnesia:dirty_delete_object(Channel) of
                ok -> {reply, ok, State};
                Error -> {reply, {error, Error}, State}
            end; 
        Error -> {reply, {error, Error}, State}
    end;

handle_call({create, Name, Description, UserId}, _From, State) ->
    ChannelId = support:generate_id(),
    Channel = #channel{
            id = ChannelId,
            name = Name,
            description =  Description,
            user_id = UserId,
            created_at = calendar:local_time()
            },    
    case internal_save(Channel) of
        ok -> {reply, {ok, Channel}, State};
        {validation, Validations} -> {reply, {validation, Validations}, State};
        Error -> {reply, {error, Error}, State}
    end;        

handle_call({save, #channel{} = Channel}, _From, State) ->
    case internal_save(Channel) of
        ok -> {reply, {ok, Channel}, State};
        {validation, Validations} -> {reply, {validation, Validations}, State};
        Error -> {reply, {error, Error}, State}
    end; 

handle_call({list, UserId}, _From, State) ->
    case mnesia:dirty_match_object(#channel{ user_id = UserId, _ = '_' }) of
        {aborted, Reason} -> {reply, {error, Reason}, State};
        Results -> {reply, {ok, Results}, State}
    end;


handle_call({count, UserId}, _From, State) ->    
    {atomic, Count} = mnesia:transaction(fun() ->
                    mnesia:foldl(
                        fun(#channel{user_id = UID}, Acc) when UID =:= UserId ->
                                Acc + 1;
                            (_, Acc) ->
                                Acc
                        end,
                        0,
                        channel
                        )
            end),
    {reply, {ok, Count}, State};

handle_call(count_all, _From, State) ->
    Size = mnesia:table_info(channel, size),
    {reply, {ok, Size}, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

internal_save(Channel = #channel{}) ->
    case validate(Channel) of
        [] -> mnesia:dirty_write(Channel);
        Validations -> {validation, Validations}
    end.    

internal_find_by_id(ChannelId, UserId) ->
    case mnesia:dirty_match_object(#channel{ id = ChannelId, user_id = UserId, _ = '_' }) of
        [Channel] -> {ok, Channel};
        [] -> {error, not_found}
    end.


validate(#channel{name = Name, description = Description, user_id = UserId}) ->    
    
    ToValidate = [{Name, "Informe um nome para o canal"}, {Description, "Informe uma descrição para o canal"}],
    
    case validator:non_empty(ToValidate) of
        [] ->
            
            case mnesia:dirty_match_object(#channel{name = Name, user_id = UserId , _ = '_'}) of
                [] ->
                    [];
                _ ->
                    [{error, "O nome do canal já está sendo usado"}]
            end;
        Validations -> Validations
    end.
