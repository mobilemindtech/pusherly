-module(dashboard_admin).

-compile(export_all).

-include("include/domain.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() -> 
    case check_auth(admin) of
        true -> #template { file="./site/templates/dashboard.html" };
        false -> wf:redirect("/login")
    end.

title() -> "Dashboard Admin - Push Notification Manager".

body() ->
    #panel{
        class="min-h-screen bg-gray-50",
        body=[
            dashboard_header("Dashboard Admin"),
            
            #panel{
                class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8",
                body=[
                    %% Tabs
                    #panel{
                        class="mb-8",
                        body=[
                            #panel{
                                class="border-b border-gray-200",
                                body=[
                                    #panel{
                                        class="flex space-x-8",
                                        body=[
                                            #link{
                                                id=channels_tab,
                                                class="py-2 px-1 border-b-2 border-blue-500 text-blue-600 font-medium text-sm",
                                                text="Canais",
                                                postback={show_tab, channels}
                                            },
                                            #link{
                                                id=apps_tab,
                                                class="py-2 px-1 border-b-2 border-transparent text-gray-500 hover:text-gray-700 font-medium text-sm cursor-pointer",
                                                text="Apps",
                                                postback={show_tab, apps}
                                            },
                                            #link{
                                                id=api_tab,
                                                class="py-2 px-1 border-b-2 border-transparent text-gray-500 hover:text-gray-700 font-medium text-sm cursor-pointer",
                                                text="API Keys",
                                                postback={show_tab, api}
                                            }
                                        ]
                                    }
                                ]
                            }
                        ]
                    },

                    %% Tab Content
                    #panel{
                        id=tab_content,
                        body=[channels_content()]
                    }
                ]
            }
        ]
    }.

channels_content() ->
    #panel{
        class="bg-white rounded-2xl shadow-lg border border-gray-200 overflow-hidden",
        body=[
            #panel{
                class="px-6 py-4 bg-gradient-to-r from-green-600 to-blue-600",
                body=[
                    #h2{class="text-xl font-bold text-white", text="Gerenciar Canais"}
                ]
            },
            #panel{
                class="p-6",
                body=[
                    %% Add Channel Form
                    #panel{
                        id=channel_form,
                        body=[
                            #panel{
                                class="grid grid-cols-1 md:grid-cols-3 gap-4 mb-6",
                                body=[
                                    #textbox{
                                        id=channel_name,
                                        class="px-4 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-green-500",
                                        placeholder="Nome do canal"
                                    },
                                    #textbox{
                                        id=channel_description,
                                        class="px-4 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-green-500",
                                        placeholder="Descrição"
                                    },
                                    #button{
                                        class="bg-gradient-to-r from-green-600 to-blue-600 hover:from-green-700 hover:to-blue-700 text-white px-6 py-2 rounded-lg font-medium transition-all duration-200",
                                        text="Criar Canal",
                                        postback=create_channel
                                    }
                                ]
                            }
                        ]
                    },
                    #hr{class="my-8 border-gray-200"},
                    %% Channels List
                    channels_list()
                ]
            }
        ]
    }.

apps_content() ->
    #panel{
        class="bg-white rounded-2xl shadow-lg border border-gray-200 overflow-hidden",
        body=[
            #panel{
                class="px-6 py-4 bg-gradient-to-r from-purple-600 to-pink-600",
                body=[
                    #h2{class="text-xl font-bold text-white", text="Gerenciar Apps"}
                ]
            },
            #panel{
                class="p-6",
                body=[
                    %% Add App Form
                    #panel{
                        id=app_form,
                        body=[
                            #panel{
                                class="space-y-6",
                                body=[
                                    #panel{
                                        class="grid grid-cols-1 md:grid-cols-2 gap-4",
                                        body=[
                                            #textbox{
                                                id=app_name,
                                                class="px-4 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-purple-500",
                                                placeholder="Nome do app"
                                            },
                                            #dropdown{
                                                id=app_channels,
                                                class="px-4 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-purple-500",
                                                multiple=true,
                                                options=get_channel_options()
                                            }
                                        ]
                                    },
                                    #panel{
                                        class="grid grid-cols-1 md:grid-cols-2 gap-4",
                                        body=[
                                            #panel{
                                                body=[
                                                    #label{class="block text-sm font-medium text-gray-700 mb-2", text="Chave Firebase"},
                                                    #textarea{
                                                        id=firebase_key,
                                                        class="w-full px-4 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-purple-500",
                                                        placeholder="Cole sua chave Firebase aqui...",
                                                        rows=4
                                                    }
                                                ]
                                            },
                                            #panel{
                                                body=[
                                                    #label{class="block text-sm font-medium text-gray-700 mb-2", text="Chave Apple"},
                                                    #textarea{
                                                        id=apple_key,
                                                        class="w-full px-4 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-purple-500",
                                                        placeholder="Cole sua chave Apple aqui...",
                                                        rows=4
                                                    }
                                                ]
                                            }
                                        ]
                                    },
                                    #button{
                                        class="bg-gradient-to-r from-purple-600 to-pink-600 hover:from-purple-700 hover:to-pink-700 text-white px-6 py-2 rounded-lg font-medium transition-all duration-200",
                                        text="Criar App",
                                        postback=create_app
                                    }
                                ]
                            }
                        ]
                    },
                    #hr{class="my-8 border-gray-200"},
                    %% Apps List
                    apps_list()
                ]
            }
        ]
    }.

api_content() ->
    UserId = wf:session(user_id),
    {ok, User} = auth_server:get_user(UserId),
    
    #panel{
        class="bg-white rounded-2xl shadow-lg border border-gray-200 overflow-hidden",
        body=[
            #panel{
                class="px-6 py-4 bg-gradient-to-r from-orange-600 to-red-600",
                body=[
                    #h2{class="text-xl font-bold text-white", text="Chaves de API"}
                ]
            },
            #panel{
                class="p-6",
                body=[
                    #panel{
                        class="bg-gray-50 rounded-lg p-6 mb-6",
                        body=[
                            #h3{class="text-lg font-semibold text-gray-900 mb-4", text="Sua Chave de API"},
                            #panel{
                                class="flex items-center space-x-4",
                                body=[
                                    #panel{
                                        class="flex-1 bg-white p-3 rounded-lg border border-gray-200 font-mono text-sm text-gray-700",
                                        body=[
                                            #span{id=api_key_display, text=User#user.api_key}
                                        ]
                                    },
                                    #button{
                                        class="bg-blue-600 hover:bg-blue-700 text-white px-4 py-2 rounded-lg font-medium transition-colors duration-200",
                                        text="Copiar",
                                        postback=copy_api_key
                                    },
                                    #button{
                                        class="bg-orange-600 hover:bg-orange-700 text-white px-4 py-2 rounded-lg font-medium transition-colors duration-200",
                                        text="Regenerar",
                                        postback=regenerate_api_key
                                    }
                                ]
                            }
                        ]
                    },
                    
                    #panel{
                        class="bg-blue-50 rounded-lg p-6",
                        body=[
                            #h3{class="text-lg font-semibold text-blue-900 mb-4", text="Como usar"},
                            #panel{
                                class="space-y-4",
                                body=[
                                    #panel{
                                        body=[
                                            #h4{class="font-medium text-blue-800 mb-2", text="1. Registrar App"},
                                            #pre{
                                                class="bg-gray-800 text-green-400 p-4 rounded-lg text-sm overflow-x-auto",
                                                text="curl -X POST https://api.pushnotify.com/register \\\n  -H \"Authorization: Bearer YOUR_API_KEY\" \\\n  -H \"Content-Type: application/json\" \\\n  -d '{\"username\": \"seu_usuario\", \"app_name\": \"MeuApp\"}'"
                                            }
                                        ]
                                    },
                                    #panel{
                                        body=[
                                            #h4{class="font-medium text-blue-800 mb-2", text="2. Enviar Notificação"},
                                            #pre{
                                                class="bg-gray-800 text-green-400 p-4 rounded-lg text-sm overflow-x-auto",
                                                text="curl -X POST https://api.pushnotify.com/send \\\n  -H \"Authorization: Bearer YOUR_API_KEY\" \\\n  -H \"Content-Type: application/json\" \\\n  -d '{\"username\": \"seu_usuario\", \"channel\": \"geral\", \"message\": \"Olá mundo!\"}'"
                                            }
                                        ]
                                    }
                                ]
                            }
                        ]
                    }
                ]
            }
        ]
    }.

channels_list() ->
    UserId = wf:session(user_id),
    Channels = get_user_channels(UserId),
    
    case Channels of
        [] ->
            #panel{
                class="text-center py-12 text-gray-500",
                body=[
                    #span{class="text-4xl block mb-4", text="📢"},
                    #p{text="Nenhum canal criado ainda"}
                ]
            };
        _ ->
            #panel{
                class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6",
                body=[channel_card(Channel) || Channel <- Channels]
            }
    end.

channel_card(Channel) ->
    #panel{
        class="bg-gradient-to-br from-green-50 to-blue-50 rounded-xl p-6 border border-gray-200 hover:shadow-lg transition-all duration-200",
        body=[
            #panel{
                class="flex items-start justify-between mb-4",
                body=[
                    #panel{
                        body=[
                            #h3{class="text-lg font-semibold text-gray-900", text=Channel#channel.name},
                            #p{class="text-sm text-gray-600 mt-1", text=Channel#channel.description}
                        ]
                    },
                    #button{
                        class="text-red-600 hover:text-red-800 text-sm",
                        text="🗑️",
                        postback={delete_channel, Channel#channel.id}
                    }
                ]
            },
            #panel{
                class="flex items-center text-xs text-gray-500",
                body=[
                    #span{text="Criado em: " ++ format_date(Channel#channel.created_at)}
                ]
            }
        ]
    }.

apps_list() ->
    UserId = wf:session(user_id),
    Apps = get_user_apps(UserId),
    
    case Apps of
        [] ->
            #panel{
                class="text-center py-12 text-gray-500",
                body=[
                    #span{class="text-4xl block mb-4", text="📱"},
                    #p{text="Nenhum app criado ainda"}
                ]
            };
        _ ->
            #panel{
                class="space-y-6",
                body=[app_card(App) || App <- Apps]
            }
    end.

app_card(App) ->
    #panel{
        class="bg-gradient-to-r from-purple-50 to-pink-50 rounded-xl p-6 border border-gray-200 hover:shadow-lg transition-all duration-200",
        body=[
            #panel{
                class="flex items-start justify-between mb-4",
                body=[
                    #panel{
                        body=[
                            #h3{class="text-lg font-semibold text-gray-900", text=App#app.name},
                            #panel{
                                class="flex items-center space-x-4 mt-2",
                                body=[
                                    #span{
                                        class="bg-green-100 text-green-800 px-2 py-1 rounded-full text-xs font-medium",
                                        text="Firebase: " ++ case App#app.firebase_key of undefined -> "❌"; _ -> "✅" end
                                    },
                                    #span{
                                        class="bg-blue-100 text-blue-800 px-2 py-1 rounded-full text-xs font-medium",
                                        text="Apple: " ++ case App#app.apple_key of undefined -> "❌"; _ -> "✅" end
                                    }
                                ]
                            }
                        ]
                    },
                    #button{
                        class="text-red-600 hover:text-red-800 text-sm",
                        text="🗑️",
                        postback={delete_app, App#app.id}
                    }
                ]
            },
            #panel{
                class="text-xs text-gray-500",
                body=[
                    #span{text="Criado em: " ++ format_date(App#app.created_at)}
                ]
            }
        ]
    }.

%% Events
event({show_tab, Tab}) ->
    %% Update tab styles
    wf:update(channels_tab, #link{
        class=case Tab of
            channels -> "py-2 px-1 border-b-2 border-blue-500 text-blue-600 font-medium text-sm";
            _ -> "py-2 px-1 border-b-2 border-transparent text-gray-500 hover:text-gray-700 font-medium text-sm cursor-pointer"
        end,
        text="Canais",
        postback={show_tab, channels}
    }),
    wf:update(apps_tab, #link{
        class=case Tab of
            apps -> "py-2 px-1 border-b-2 border-blue-500 text-blue-600 font-medium text-sm";
            _ -> "py-2 px-1 border-b-2 border-transparent text-gray-500 hover:text-gray-700 font-medium text-sm cursor-pointer"
        end,
        text="Apps",
        postback={show_tab, apps}
    }),
    wf:update(api_tab, #link{
        class=case Tab of
            api -> "py-2 px-1 border-b-2 border-blue-500 text-blue-600 font-medium text-sm";
            _ -> "py-2 px-1 border-b-2 border-transparent text-gray-500 hover:text-gray-700 font-medium text-sm cursor-pointer"
        end,
        text="API Keys",
        postback={show_tab, api}
    }),
    
    %% Update content
    Content = case Tab of
        channels -> channels_content();
        apps -> apps_content();
        api -> api_content()
    end,
    wf:update(tab_content, Content);

event(create_channel) ->
    Name = wf:q(channel_name),
    Description = wf:q(channel_description),
    UserId = wf:session(user_id),
    
    ChannelId = generate_id(),
    Channel = #channel{
        id = ChannelId,
        name = Name,
        description = Description,
        user_id = UserId,
        created_at = calendar:local_time()
    },
    
    case mnesia:dirty_write(Channel) of
        ok ->
            wf:flash([{type, success}, {text, "Canal criado com sucesso!"}]),
            wf:redirect("/dashboard/admin");
        Error ->
            wf:flash([{type, error}, {text, "Erro ao criar canal"}])
    end;

event(create_app) ->
    Name = wf:q(app_name),
    Channels = wf:q(app_channels),
    FirebaseKey = wf:q(firebase_key),
    AppleKey = wf:q(apple_key),
    UserId = wf:session(user_id),
    
    AppId = generate_id(),
    App = #app{
        id = AppId,
        name = Name,
        user_id = UserId,
        channels = Channels,
        firebase_key = case FirebaseKey of "" -> undefined; _ -> FirebaseKey end,
        apple_key = case AppleKey of "" -> undefined; _ -> AppleKey end,
        created_at = calendar:local_time()
    },
    
    case mnesia:dirty_write(App) of
        ok ->
            wf:flash([{type, success}, {text, "App criado com sucesso!"}]),
            wf:redirect("/dashboard/admin");
        Error ->
            wf:flash([{type, error}, {text, "Erro ao criar app"}])
    end;

event(regenerate_api_key) ->
    UserId = wf:session(user_id),
    case auth_server:generate_api_key(UserId) of
        {ok, NewKey} ->
            wf:update(api_key_display, #span{text=NewKey}),
            wf:flash([{type, success}, {text, "Nova chave gerada com sucesso!"}]);
        Error ->
            wf:flash([{type, error}, {text, "Erro ao gerar nova chave"}])
    end;

event(copy_api_key) ->
    UserId = wf:session(user_id),
    {ok, User} = auth_server:get_user(UserId),
    logger:info("copy_api_key"),
    wf:wire("copyToClipboard('"++ User#user.api_key ++"')"),
    wf:flash([{type, info}, {text, "Chave copiada para a área de transferência!"}]);

event({delete_channel, ChannelId}) ->
    mnesia:dirty_delete(channel, ChannelId),
    wf:flash([{type, success}, {text, "Canal removido com sucesso!"}]),
    wf:redirect("/dashboard/admin");

event({delete_app, AppId}) ->
    mnesia:dirty_delete(app, AppId),
    wf:flash([{type, success}, {text, "App removido com sucesso!"}]),
    wf:redirect("/dashboard/admin").

%% Helper functions
check_auth(RequiredType) ->
    case wf:session(user_id) of
        undefined -> false;
        UserId ->
            case auth_server:get_user(UserId) of
                {ok, User} -> User#user.type =:= RequiredType orelse User#user.type =:= root;
                _ -> false
            end
    end.

dashboard_header(Title) ->
    #panel{
        class="bg-white shadow-sm border-b border-gray-200",
        body=[
            #panel{
                class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8",
                body=[
                    #panel{
                        class="flex justify-between items-center py-6",
                        body=[
                            #h1{class="text-2xl font-bold text-gray-900", text=Title},
                            #link{
                                url="/logout",
                                class="bg-red-600 hover:bg-red-700 text-white px-4 py-2 rounded-lg font-medium transition-colors duration-200",
                                text="Sair"
                            }
                        ]
                    }
                ]
            }
        ]
    }.

get_user_channels(UserId) ->
    mnesia:dirty_match_object(#channel{user_id = UserId, _ = '_'}).

get_user_apps(UserId) ->
    mnesia:dirty_match_object(#app{user_id = UserId, _ = '_'}).

get_channel_options() ->
    UserId = wf:session(user_id),
    Channels = get_user_channels(UserId),
    [#option{text=Channel#channel.name, value=Channel#channel.id} || Channel <- Channels].

format_date(DateTime) ->
    {{Y, M, D}, {H, Min, _}} = DateTime,
    io_lib:format("~2..0w/~2..0w/~4..0w ~2..0w:~2..0w", [D, M, Y, H, Min]).

generate_id() ->
    erlang:integer_to_binary(erlang:unique_integer([positive])).