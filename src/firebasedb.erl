-module(firebasedb).
-export([
  on_update/2,
  on_remove/1
]).

-ifdef(use_specs).

-callback on_update(Key :: binary(), Data :: map()) -> ok | any().

-callback on_remove(Key :: binary()) -> ok | any().

-else.

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{on_update, 2}, {on_remove, 1}];
behaviour_info(_Other) ->
    undefined.

-endif.

on_update(Key, Data) ->
  io:format("[default:on_update] Key: ~p || Data: ~p~n", [Key, Data]).

on_remove(Key) ->
  io:format("[default:on_remove] Key: ~p~n", [Key]).
