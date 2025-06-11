-module(pusherl_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    %% Inicializar Mnesia
    mnesia:create_schema([node()]),
    mnesia:start(),
    
    %% Criar tabelas
    create_tables(),
    
    %% Iniciar supervisores
    pusherl_sup:start_link().

stop(_State) ->
    mnesia:stop(),
    ok.

create_tables() ->
    %% Tabela de usuários
    mnesia:create_table(user, [
            {attributes, [id, name, email, password_hash, type, api_key, created_at]},
            {disc_copies, [node()]},
            {type, set}
            ]),
    
    %% Tabela de canais
    mnesia:create_table(channel, [
            {attributes, [id, name, description, user_id, created_at]},
            {disc_copies, [node()]},
            {type, set}
            ]),
    
    %% Tabela de apps
    mnesia:create_table(app, [
            {attributes, [id, name, user_id, channels, firebase_key, apple_key, created_at]},
            {disc_copies, [node()]},
            {type, set}
            ]),
    
    %% Tabela de sessões
    mnesia:create_table(session, [
            {attributes, [session_id, user_id, created_at, expires_at]},
            {disc_copies, [node()]},
            {type, set}
            ]).