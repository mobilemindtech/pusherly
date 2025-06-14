-module(login).
-compile(export_all).

-include("include/domain.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


main() -> #template { file="./site/templates/bare.html" }.

title() -> "Pusherl :: Login".

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
                            #h1{class="text-3xl font-bold text-white mb-2", text="Bem-vindo de volta"},
                            #p{class="text-gray-300", text="Entre com suas credenciais"}
                            ]
                        },
                    
                    %% Form
                    #panel{
                        id=login_form,
                        body=[
                            #panel{
                                class="space-y-6",
                                body=[
                                    %% Email
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
                                        id=login_btn,
                                        class="w-full bg-gradient-to-r from-purple-600 to-blue-600 hover:from-purple-700 hover:to-blue-700 text-white py-3 rounded-xl font-semibold transition-all duration-200 shadow-lg hover:shadow-purple-500/25 hover:scale-105",
                                        text="Entrar",
                                        postback=login
                                        }
                                    ]
                                }
                            ]
                        },
                    
                    %% Links
                    #panel{
                        class="mt-8 text-center space-y-4",
                        body=[
                            #link{
                                url="/forgot-password",
                                class="text-purple-300 hover:text-purple-200 text-sm transition-colors duration-200",
                                text="Esqueceu sua senha?"
                                },
                            #hr{class="border-white/20"},
                            #p{
                                class="text-gray-300 text-sm",
                                body=[
                                    "Não tem uma conta? ",
                                    #link{
                                        url="/register",
                                        class="text-purple-300 hover:text-purple-200 font-medium transition-colors duration-200",
                                        text="Cadastre-se"
                                        }
                                    ]
                                }
                            ]
                        }
                    ]
                }
            ]
        }.

event(login) ->
    Email = wf:q(email),
    Password = wf:q(password),
    case auth_server:authenticate(Email, Password) of
        {ok, SessionId, User} ->
            wf:session(user_id, User#user.id),
            wf:session(user_name, User#user.name),
            wf:session(user_username, User#user.email),
            wf:session(session_id, SessionId),
            case User#user.type of
                root -> wf:redirect("/dashboard/root");
                admin -> wf:redirect("/dashboard/admin");
                _ -> 
                    wf:flash([{type, error}, {text, "Tipo de usuário inválido"}])
            end;
        {error, _} ->
            wf:flash([{type, error}, {text, "Email ou senha inválidos"}])
    end.