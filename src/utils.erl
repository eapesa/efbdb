-module(utils).
-export([
  ensure_binary/1,
  ensure_integer/1,
  ensure_list/1
]).

ensure_binary(Term) when is_binary(Term) ->
  Term;
ensure_binary(Term) when is_integer(Term) ->
  integer_to_binary(Term);
ensure_binary(Term) when is_list(Term) ->
  list_to_binary(Term);
ensure_binary(Term) when is_atom(Term) ->
  atom_to_binary(Term, utf8).

ensure_integer(Term) when is_integer(Term) ->
  Term;
ensure_integer(Term) when is_binary(Term) ->
  binary_to_integer(Term);
ensure_integer(Term) when is_list(Term) ->
  list_to_integer(Term).

ensure_list(Term) when is_list(Term) ->
  Term;
ensure_list(Term) when is_binary(Term) ->
  binary_to_list(Term);
ensure_list(Term) when is_integer(Term) ->
  integer_to_list(Term).
