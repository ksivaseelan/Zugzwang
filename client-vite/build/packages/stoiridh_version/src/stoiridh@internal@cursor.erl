-module(stoiridh@internal@cursor).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/0, next/1]).
-export_type([cursor/0]).

-type cursor() :: {cursor, stoiridh@internal@types:version_part()}.

-spec new() -> cursor().
new() ->
    {cursor, major}.

-spec next(cursor()) -> cursor().
next(Cursor) ->
    case Cursor of
        {cursor, major} ->
            {cursor, minor};

        {cursor, minor} ->
            {cursor, patch};

        {cursor, patch} ->
            {cursor, patch}
    end.
