%% ===================================================================
%% FUNÇÕES DE CONVENIÊNCIA
%% Arquivo: lib/flash_helper.erl
%% ===================================================================

-module(fflash_helper).
-include("include/records.hrl").
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").

% Funções de conveniência para criar notificações
flash_success(Message) ->
    flash_success(Message, []).

flash_success(Message, Options) ->
    show_flash(success, Message, "", Options).

flash_error(Message) ->
    flash_error(Message, []).

flash_error(Message, Options) ->
    show_flash(error, Message, "", Options).

flash_warning(Message) ->
    flash_warning(Message, []).

flash_warning(Message, Options) ->
    show_flash(warning, Message, "", Options).

flash_info(Message) ->
    flash_info(Message, []).

flash_info(Message, Options) ->
    show_flash(info, Message, "", Options).

% Função principal para mostrar flash
show_flash(Type, Message, Title, Options) ->
    FlashId = wf:temp_id(),
    Duration = proplists:get_value(duration, Options, 10000),
    Position = proplists:get_value(position, Options, top_right),
    Closable = proplists:get_value(closable, Options, true),
    Animation = proplists:get_value(animation, Options, slide),
    Icon = proplists:get_value(icon, Options, auto),
    
    Flash = #fflash{
            id = FlashId,
            type = Type,
            message = Message,
            title = Title,
            duration = Duration,
            position = Position,
            closable = Closable,
            animation = Animation,
            icon = Icon
            },
    
    
    JsId = wf:normalize_id(FlashId),
    Script = wf:f(
            "CustomFlash.show('~s', '~s', ~w, ~s);",
            [JsId, atom_to_list(Position), Duration, 
                case Closable of true -> "true"; false -> "false" end]
            ),    
    % Adicionar ao container de flash
    wf:insert_bottom(flash_container, Flash),
    wf:wire(Script).

% Função para limpar todas as notificações
clear_all_flashes() ->
    wf:wire("CustomFlash.clearAll();").

% Função para configurar container (chamar no template principal)
setup_flash_container() ->
    setup_flash_container([]).

setup_flash_container(Options) ->
    Position = proplists:get_value(default_position, Options, top_right),
    MaxItems = proplists:get_value(max_items, Options, 5),
    
    #panel{
        id = flash_container,
        class = "flash-container flash-container-" ++ atom_to_list(Position),
        data_fields = [
            {"data-max-items", integer_to_list(MaxItems)}
            ]
        }.