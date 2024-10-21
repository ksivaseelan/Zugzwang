-module(stoiridh@internal@int).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([is_positive/1]).

-spec is_positive(integer()) -> {ok, integer()} | {error, binary()}.
is_positive(Value) ->
    case Value of
        Value@1 when Value@1 >= 0 ->
            {ok, Value@1};

        _ ->
            {error,
                <<(gleam@int:to_string(Value))/binary,
                    " is not a positive integer."/utf8>>}
    end.
