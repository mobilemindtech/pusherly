-module(register).
-compile(export_all).

-include("include/domain.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


main() -> #template { file="./site/templates/bare.html" }.

title() -> "Pusherl :: Register".

body() ->
    #panel{
        class="min-h-screen bg-gradient-to-br from-blue-900 via-purple-900 to-indigo-900 flex items-center justify-center px-4",
        body=[
            #panel{
                class="bg-white/10 backdrop-blur-lg rounded-3xl p-8 w-full max-w-md border border-white/20 shadow-2xl",
                body=[
                    %% Header
                    #panel{
                        class="text-center mb-8",
                        body=[
                            #h1{class="text-3xl font-bold text-white mb-2", text="Nova conta"},
                            #p{class="text-gray-300", text="Entre com seus dados cadastrair"}
                            ]
                        },
                    
                    %% Form
                    #panel{
                        id=register_form,
                        body=[
                            #panel{
                                class="space-y-6",
                                body=[
                                    %% Email
                                    #panel{
                                        body=[
                                            #label{
                                                class="block text-sm font-medium text-gray-200 mb-2",
                                                text="Name"
                                                },
                                            #textbox{
                                                id=name,
                                                class="w-full px-4 py-3 bg-white/10 border border-white/20 rounded-xl text-white placeholder-gray-400 focus:outline-none focus:ring-2 focus:ring-purple-500 focus:border-transparent transition-all duration-200",
                                                placeholder="fulano"
                                                }
                                            ]
                                        },
                                    #panel{
                                        body=[
                                            #label{
                                                class="block text-sm font-medium text-gray-200 mb-2",
                                                text="Email"
                                                },
                                            #textbox{
                                                id=email,
                                                class="w-full px-4 py-3 bg-white/10 border border-white/20 rounded-xl text-white placeholder-gray-400 focus:outline-none focus:ring-2 focus:ring-purple-500 focus:border-transparent transition-all duration-200",
                                                placeholder="seu@email.com"
                                                }
                                            ]
                                        },
                                    
                                    %% Password
                                    #panel{
                                        body=[
                                            #label{
                                                class="block text-sm font-medium text-gray-200 mb-2",
                                                text="Senha"
                                                },
                                            #password{
                                                id=password,
                                                class="w-full px-4 py-3 bg-white/10 border border-white/20 rounded-xl text-white placeholder-gray-400 focus:outline-none focus:ring-2 focus:ring-purple-500 focus:border-transparent transition-all duration-200",
                                                placeholder="••••••••"
                                                }
                                            ]
                                        },
                                    
                                    %% Submit Button
                                    #button{
                                        id=register_btn,
                                        class="w-full bg-gradient-to-r from-purple-600 to-blue-600 hover:from-purple-700 hover:to-blue-700 text-white py-3 rounded-xl font-semibold transition-all duration-200 shadow-lg hover:shadow-purple-500/25 hover:scale-105",
                                        text="Register",
                                        postback=register
                                        }
                                    ]
                                }
                            ]
                        }
                    ]
                }
            ]
        }.

event(register) ->
    Email = wf:q(email),
    Password = wf:q(password),
    Name = wf:q(name),
    case user_service:create(Name, Email, Password, admin) of
        {ok, User} ->
            authenticate(Email, Password),
            wf:flash([{type, success}, {text, "Sua conta foi criada com sucesso!"}]);            
        {error, Error} ->
            wf:flash([{type, error}, {text, "Ocorreu um erro ao criar conta: " ++ Error}])
    end.

authenticate(Email, Password) ->
    case auth_server:authenticate(Email, Password) of
        {ok, SessionId, User} ->
            wf:session(user_id, User#user.id),
            wf:session(session_id, SessionId),
            case User#user.type of
                root -> wf:redirect("/dashboard/root");
                admin -> wf:redirect("/dashboard/admin");
                _ -> 
                    wf:flash([{type, error}, {text, "Tipo de conta não encontrado"}])
            end;
        {error, _} ->
            wf:flash([{type, error}, {text, "Email ou senha inválidos"}])
    end.