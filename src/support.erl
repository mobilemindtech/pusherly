-module(support).

-include("include/domain.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

-export([
    hash_password/1, 
    check_password/2, 
    generate_id/0, 
    generate_session_id/0, 
    generate_uuid/0,
    check_auth/1]).

hash_password(Password) ->
    crypto:hash(sha256, Password).

check_password(Password, Hash) ->
    crypto:hash(sha256, Password) =:= Hash.

generate_id() ->
    erlang:integer_to_binary(erlang:unique_integer([positive])).

generate_session_id() ->
    generate_uuid().

generate_uuid() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", 
            [A, B, C, D, E])).    

%% Helper functions
check_auth(RequiredType) ->
    case wf:session(user_id) of
        undefined -> false;
        UserId ->
            case user_service:find_by_id(UserId) of
                {ok, User} -> User#user.type =:= RequiredType;
                _ -> false
            end
    end.    