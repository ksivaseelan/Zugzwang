-module(stoiridh@internal@types).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export_type([constraint_result/0, version_part/0]).

-type constraint_result() :: {partial, version_part()} |
    strict |
    {wildcard, version_part()}.

-type version_part() :: major | minor | patch.


