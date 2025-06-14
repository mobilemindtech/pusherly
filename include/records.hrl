%% Include the automatically generated plugins directory
-include("plugins.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
%% Include any application-specific custom elements, actions, or validators below

-record(space, {?ELEMENT_BASE(element_space)}).


-record(fflash, {?ELEMENT_BASE(element_fflash),
		type = info,           % success, error, warning, info
		message = "",
		duration = 5000,       % tempo em ms (0 = permanente)
		closable = true,       % se pode fechar manualmente
		position = top_right,  % top_right, top_left, top_center, bottom_right, etc
		animation = slide,     % slide, fade, bounce
		icon = auto          % auto, custom icon, or false		
		}).