-module(ui).
-include("include/domain.hrl").
-include("include/records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

-export([dashboard_header/1]).


dashboard_header(Title) ->
	Name = wf:session(user_name),
	#panel{
		class="bg-white shadow-sm border-b border-gray-200",
		body=[
			#panel{
				class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8",
				body=[
					#panel{
						class="flex justify-between items-center py-6",
						body=[
							#h1{
								class="text-2xl font-bold text-gray-900", 
								body=[
									#span{text=Title},
									#space{},
									#span{text="Olá, " ++ Name, class="text-sm font-normal"}
									]},
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