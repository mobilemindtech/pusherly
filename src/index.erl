-module(index).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Pusherl".

body() ->
    
    #panel{
        class="min-h-screen bg-gradient-to-br from-blue-900 via-purple-900 to-indigo-900",
        body=[
            %% Navigation
            #panel{
                class="bg-white/10 backdrop-blur-lg border-b border-white/20",
                body=[
                    #panel{
                        class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8",
                        body=[
                            #panel{
                                class="flex justify-between items-center py-6",
                                body=[
                                    #panel{
                                        class="flex items-center",
                                        body=[
                                            #span{class="text-2xl font-bold text-white", text="🚀 Pusherl"}
                                            ]
                                        },
                                    #panel{
                                        class="flex items-center space-x-4",
                                        body=[
                                            #link{
                                                url="/login",
                                                class="bg-white/20 hover:bg-white/30 text-white px-6 py-2 rounded-lg font-medium transition-all duration-200",
                                                text="Login"
                                                },
                                            #link{
                                                url="/register",
                                                class="bg-gradient-to-r from-purple-600 to-blue-600 hover:from-purple-700 hover:to-blue-700 text-white px-6 py-2 rounded-lg font-medium transition-all duration-200 shadow-lg",
                                                text="Começar"
                                                }
                                            ]
                                        }
                                    ]
                                }
                            ]
                        }
                    ]
                },
            
            %% Hero Section
            #panel{
                class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-20",
                body=[
                    #panel{
                        class="text-center",
                        body=[
                            #h1{
                                class="text-5xl md:text-7xl font-bold text-white mb-6 leading-tight",
                                text="Notificações Push"
                                },
                            #h2{
                                class="text-2xl md:text-3xl font-light text-purple-200 mb-8",
                                text="Simples e Poderosas"
                                },
                            #p{
                                class="text-xl text-gray-300 mb-12 max-w-3xl mx-auto leading-relaxed",
                                text="Gerencie suas notificações push para iOS e Android de forma centralizada. Crie canais, configure apps e envie mensagens para milhões de usuários com nossa plataforma intuitiva."
                                },
                            #panel{
                                class="flex flex-col sm:flex-row gap-4 justify-center items-center",
                                body=[
                                    #link{
                                        url="/register",
                                        class="bg-gradient-to-r from-purple-600 to-blue-600 hover:from-purple-700 hover:to-blue-700 text-white px-8 py-4 rounded-xl font-semibold text-lg transition-all duration-200 shadow-2xl hover:shadow-purple-500/25 hover:scale-105",
                                        text="Criar Conta Gratuita"
                                        },
                                    #link{
                                        url="/demo",
                                        class="bg-white/10 hover:bg-white/20 text-white px-8 py-4 rounded-xl font-semibold text-lg transition-all duration-200 backdrop-blur-sm border border-white/20",
                                        text="Ver Demo"
                                        }
                                    ]
                                }
                            ]
                        }
                    ]
                },
            
            %% Features Section
            #panel{
                class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-20",
                body=[
                    #panel{
                        class="grid md:grid-cols-3 gap-8",
                        body=[
                            feature_card("🎯", "Canais Inteligentes", "Organize suas notificações em canais personalizados para diferentes tipos de conteúdo"),
                            feature_card("🔧", "APIs Simples", "Integre facilmente com nossa API RESTful documentada e SDKs para múltiplas linguagens"),
                            feature_card("📊", "Analytics Avançado", "Acompanhe entregas, aberturas e conversões em tempo real com dashboards detalhados")
                            ]
                        }
                    ]
                },
            
            %% Footer
            #panel{
                class="bg-black/20 backdrop-blur-lg border-t border-white/10 mt-20",
                body=[
                    #panel{
                        class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12",
                        body=[
                            #panel{
                                class="text-center text-gray-400",
                                body=[
                                    #p{text="© 2025 Pusherl. Todos os direitos reservados."}
                                    ]
                                }
                            ]
                        }
                    ]
                }
            ]
        }.

feature_card(Icon, Title, Description) ->
    #panel{
        class="bg-white/5 backdrop-blur-lg rounded-2xl p-8 border border-white/10 hover:bg-white/10 transition-all duration-300 hover:scale-105",
        body=[
            #span{class="text-4xl mb-4 block", text=Icon},
            #h3{class="text-xl font-bold text-white mb-4", text=Title},
            #p{class="text-gray-300 leading-relaxed", text=Description}
            ]
        }.