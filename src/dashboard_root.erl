-module(dashboard_root).

-compile(export_all).

-include("include/domain.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() -> 
    case check_auth(root) of
        true -> #template { file="./site/templates/dashboard.html" };
        false -> wf:redirect("/login")
    end.

title() -> "Dashboard Root - Push Notification Manager".

body() ->
    #panel{
        class="min-h-screen bg-gray-50",
        body=[
            dashboard_header("Dashboard Root"),
            
            #panel{
                class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8",
                body=[
                    %% Stats Cards
                    #panel{
                        class="grid grid-cols-1 md:grid-cols-4 gap-6 mb-8",
                        body=[
                            stat_card("👥", "Usuários", get_user_count(), "text-blue-600"),
                            stat_card("📱", "Apps", get_app_count(), "text-green-600"),
                            stat_card("📢", "Canais", get_channel_count(), "text-purple-600"),
                            stat_card("🚀", "Notificações", get_notification_count(), "text-orange-600")
                        ]
                    },

                    %% User Management
                    #panel{
                        class="bg-white rounded-2xl shadow-lg border border-gray-200 overflow-hidden",
                        body=[
                            #panel{
                                class="px-6 py-4 bg-gradient-to-r from-blue-600 to-purple-600",
                                body=[
                                    #h2{class="text-xl font-bold text-white", text="Gerenciar Usuários"}
                                ]
                            },
                            #panel{
                                class="p-6",
                                body=[
                                    %% Add User Form
                                    user_form(),
                                    #hr{class="my-8 border-gray-200"},
                                    %% Users Table
                                    users_table()
                                ]
                            }
                        ]
                    }
                ]
            }
        ]
    }.

user_form() ->
    #panel{
        id=user_form,
        body=[
            #panel{
                class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4 mb-6",
                body=[
                    #textbox{
                        id=user_name,
                        class="px-4 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500",
                        placeholder="Nome do usuário"
                    },
                    #textbox{
                        id=user_email,
                        class="px-4 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500",
                        placeholder="Email"
                    },
                    #dropdown{
                        id=user_type,
                        class="px-4 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500",
                        options=[
                            #option{text="Admin", value="admin"},
                            #option{text="Root", value="root"}
                        ]
                    },
                    #button{
                        class="bg-gradient-to-r from-blue-600 to-purple-600 hover:from-blue-700 hover:to-purple-700 text-white px-6 py-2 rounded-lg font-medium transition-all duration-200",
                        text="Criar Usuário",
                        postback=create_user
                    }
                ]
            }
        ]
    }.

users_table() ->
    Users = get_all_users(),
    #table{
        class="w-full",
        rows=[
            #tablerow{
                class="border-b border-gray-200 bg-gray-50",
                cells=[
                    #tablecell{class="px-4 py-3 font-medium text-gray-900", text="Nome"},
                    #tablecell{class="px-4 py-3 font-medium text-gray-900", text="Email"},
                    #tablecell{class="px-4 py-3 font-medium text-gray-900", text="Tipo"},
                    #tablecell{class="px-4 py-3 font-medium text-gray-900", text="Ações"}
                ]
            },
            [user_row(User) || User <- Users]
        ]
    }.

user_row(User) ->    
    Type = if 
        is_atom(User#user.type) -> User#user.type;
        true -> list_to_atom(User#user.type)
    end,
    #tablerow{
        class="border-b border-gray-100 hover:bg-gray-50",
        cells=[
            #tablecell{class="px-4 py-3 text-gray-900", text=User#user.name},
            #tablecell{class="px-4 py-3 text-gray-500", text=User#user.email},
            #tablecell{class="px-4 py-3", body=[
                #span{
                    class=case Type of
                        root  -> "bg-red-100 text-red-800 px-2 py-1 rounded-full text-xs font-medium";
                        admin -> "bg-blue-100 text-blue-800 px-2 py-1 rounded-full text-xs font-medium"
                    end,
                    text=string:to_upper(atom_to_list(Type))
                }
            ]},
            #tablecell{
                class="px-4 py-3",
                body=[
                    #button{
                        class="text-red-600 hover:text-red-800 text-sm font-medium",
                        text="Remover",
                        postback={delete_user, User#user.id}
                    }
                ]
            }
        ]
    }.

event(create_user) ->
    Name = wf:q(user_name),
    Email = wf:q(user_email),
    Type = list_to_atom(wf:q(user_type)),
    Password = generate_temp_password(),
    
    case auth_server:create_user(Name, Email, Password, Type) of
        {ok, _User} ->
            wf:flash([{type, success}, {text, "Usuário criado com sucesso! Senha temporária: " ++ Password}]),
            wf:redirect("/dashboard/root");
        {error, _} ->
            wf:flash([{type, error}, {text, "Erro ao criar usuário"}])
    end;

event({delete_user, UserId}) ->
    mnesia:dirty_delete(user, UserId),
    wf:flash([{type, success}, {text, "Usuário removido com sucesso"}]),
    wf:redirect("/dashboard/root").

%% Helper functions
check_auth(RequiredType) ->
    case wf:session(user_id) of
        undefined -> false;
        UserId ->
            case auth_server:get_user(UserId) of
                {ok, User} -> User#user.type =:= RequiredType;
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

stat_card(Icon, Label, Value, ColorClass) ->
    #panel{
        class="bg-white rounded-xl shadow-lg border border-gray-200 p-6 hover:shadow-xl transition-all duration-200",
        body=[
            #panel{
                class="flex items-center justify-between",
                body=[
                    #panel{
                        body=[
                            #span{class="text-3xl mb-2 block", text=Icon},
                            #p{class="text-sm text-gray-600", text=Label},
                            #p{class=ColorClass ++ " text-2xl font-bold", text=integer_to_list(Value)}
                        ]
                    }
                ]
            }
        ]
    }.

get_user_count() -> 
    length(mnesia:dirty_match_object(#user{_ = '_'})).

get_app_count() -> 
    length(mnesia:dirty_match_object(#app{_ = '_'})).

get_channel_count() -> 
    length(mnesia:dirty_match_object(#channel{_ = '_'})).

get_notification_count() -> 
    %% Placeholder - seria implementado com tabela de notificações
    1247.

get_all_users() ->
    mnesia:dirty_match_object(#user{_ = '_'}).

generate_temp_password() ->
    "temp" ++ integer_to_list(rand:uniform(10000)).