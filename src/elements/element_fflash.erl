%% ===================================================================
%% ELEMENTO FLASH PERSONALIZADO
%% Arquivo: elements/element_fflash.erl
%% ===================================================================

-module(element_fflash).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-export([
    reflect/0,
    render_element/1
    ]).


-spec reflect() -> [atom()].
reflect() -> record_info(fields, fflash).

-spec render_element(#fflash{}) -> body().
render_element(Record = #fflash{}) ->    
    Type = Record#fflash.type,
    Message = wf:html_encode(Record#fflash.message),
    Title = wf:html_encode(Record#fflash.title),
    Duration = Record#fflash.duration,
    Closable = Record#fflash.closable,
    Position = Record#fflash.position,
    Animation = Record#fflash.animation,
    Id = Record#fflash.id,
    Icon = get_icon(Record#fflash.icon, Type),
    
    % Gerar classes CSS
    Classes = string:join([
                "custom-flash-message",
                "flash-" ++ atom_to_list(Type),
                "flash-" ++ atom_to_list(Position),
                "flash-anim-" ++ atom_to_list(Animation)
                ], " "),
    
    % Botão de fechar (se habilitado)
    CloseButton = case Closable of
        true -> 
            #button{
                class = "flash-close-btn",
                text = "×",
                click = #event{
                    type = click
                    %actions = #script{script = wf:f("CustomFlash.close('~s')", [FlashId])}
                    }
                };
        false -> ""
    end,    
    
    % Conteúdo da mensagem
    Content = [
            case Icon of
                false -> "";
                _ -> #span{class = "flash-icon", text = Icon}
            end,
            #panel{class = "flash-content", body = [
                    case Title of
                        "" -> "";
                        _ -> #h4{class = "flash-title", text = Title}
                    end,
                    #panel{class = "flash-message", text = Message}
                    ]},
            CloseButton,
            case Duration > 0 of
                true -> #panel{class = "flash-progress", 
                        style = io_lib:format("animation-duration: ~wms", [Duration])};
                false -> ""
            end
            ],
    
    #panel{
        id = Id,
        class = Classes,
        style = "display: none;",
        body = Content
        }.

% Função auxiliar para ícones
get_icon(auto, success) -> "✓";
get_icon(auto, error) -> "✕";
get_icon(auto, warning) -> "⚠";
get_icon(auto, info) -> "ℹ";
get_icon(auto, _) -> "ℹ";
get_icon(false, _) -> false;
get_icon(Icon, _) when is_list(Icon) -> Icon;
get_icon(Icon, _) -> Icon.
