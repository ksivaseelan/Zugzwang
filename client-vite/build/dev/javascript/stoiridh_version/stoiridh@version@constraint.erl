-module(stoiridh@version@constraint).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([check/2, new/1]).
-export_type([constraint/0, constraint_error/0, operator/0]).

-opaque constraint() :: {constraint,
        list({operator(), stoiridh@version:version()})}.

-type constraint_error() :: invalid_constraint.

-type operator() :: equal |
    less_than |
    less_than_or_equal_to |
    greater_than |
    greater_than_or_equal_to |
    caret |
    tilde.

-spec check(constraint(), stoiridh@version:version()) -> boolean().
check(Constraint, Version) ->
    _pipe = erlang:element(2, Constraint),
    gleam@list:all(_pipe, fun(C) -> case C of
                {caret, V} ->
                    gleam@io:println_error(
                        <<<<<<<<"^"/utf8,
                                        (stoiridh@version:to_string(V))/binary>>/binary,
                                    " was not interpreted correctly. Please, open an issue at"/utf8>>/binary,
                                " https://gitlab.com/stoiridh-project/stoiridh-version/-/issues"/utf8>>/binary,
                            " with a reproductible example."/utf8>>
                    ),
                    false;

                {tilde, V@1} ->
                    gleam@io:println_error(
                        <<<<<<<<"~"/utf8,
                                        (stoiridh@version:to_string(V@1))/binary>>/binary,
                                    " was not interpreted correctly. Please, open an issue at"/utf8>>/binary,
                                " https://gitlab.com/stoiridh-project/stoiridh-version/-/issues"/utf8>>/binary,
                            " with a reproductible example."/utf8>>
                    ),
                    false;

                {equal, V@2} ->
                    stoiridh@version:compare(Version, V@2) =:= eq;

                {less_than, V@3} ->
                    stoiridh@version:compare(Version, V@3) =:= lt;

                {less_than_or_equal_to, V@4} ->
                    case stoiridh@version:compare(Version, V@4) of
                        lt ->
                            true;

                        eq ->
                            true;

                        gt ->
                            false
                    end;

                {greater_than, V@5} ->
                    stoiridh@version:compare(Version, V@5) =:= gt;

                {greater_than_or_equal_to, V@6} ->
                    case stoiridh@version:compare(Version, V@6) of
                        gt ->
                            true;

                        eq ->
                            true;

                        lt ->
                            false
                    end
            end end).

-spec transform_to_version_constraint(
    {stoiridh@internal@cursor:cursor(),
        stoiridh@internal@types:constraint_result(),
        binary()}
) -> {ok,
        {stoiridh@internal@types:constraint_result(),
            stoiridh@version:version()}} |
    {error, constraint_error()}.
transform_to_version_constraint(State) ->
    {Cursor, Result, Version} = State,
    case Result of
        strict ->
            case Cursor of
                {cursor, major} ->
                    gleam@result:'try'(
                        begin
                            _pipe = stoiridh@version:parse(
                                <<Version/binary, ".0.0"/utf8>>
                            ),
                            gleam@result:map_error(
                                _pipe,
                                fun(_) -> invalid_constraint end
                            )
                        end,
                        fun(Version@1) ->
                            {ok, {{partial, major}, Version@1}}
                        end
                    );

                {cursor, minor} ->
                    gleam@result:'try'(
                        begin
                            _pipe@1 = stoiridh@version:parse(
                                <<Version/binary, ".0"/utf8>>
                            ),
                            gleam@result:map_error(
                                _pipe@1,
                                fun(_) -> invalid_constraint end
                            )
                        end,
                        fun(Version@2) ->
                            {ok, {{partial, minor}, Version@2}}
                        end
                    );

                {cursor, patch} ->
                    gleam@result:'try'(
                        begin
                            _pipe@2 = stoiridh@version:parse(Version),
                            gleam@result:map_error(
                                _pipe@2,
                                fun(_) -> invalid_constraint end
                            )
                        end,
                        fun(Version@3) -> {ok, {strict, Version@3}} end
                    )
            end;

        {wildcard, patch} ->
            gleam@result:'try'(
                begin
                    _pipe@3 = stoiridh@version:parse(Version),
                    gleam@result:map_error(
                        _pipe@3,
                        fun(_) -> invalid_constraint end
                    )
                end,
                fun(Version@4) -> {ok, {{wildcard, patch}, Version@4}} end
            );

        {wildcard, minor} ->
            gleam@result:'try'(
                begin
                    _pipe@4 = stoiridh@version:parse(
                        <<Version/binary, ".0"/utf8>>
                    ),
                    gleam@result:map_error(
                        _pipe@4,
                        fun(_) -> invalid_constraint end
                    )
                end,
                fun(Version@5) -> {ok, {{wildcard, minor}, Version@5}} end
            );

        {wildcard, major} ->
            gleam@result:'try'(
                begin
                    _pipe@5 = stoiridh@version:parse(
                        <<Version/binary, ".0.0"/utf8>>
                    ),
                    gleam@result:map_error(
                        _pipe@5,
                        fun(_) -> invalid_constraint end
                    )
                end,
                fun(Version@6) -> {ok, {{wildcard, major}, Version@6}} end
            );

        {partial, patch} ->
            {error, invalid_constraint};

        {partial, minor} ->
            {error, invalid_constraint};

        {partial, major} ->
            {error, invalid_constraint}
    end.

-spec parse_loosely(binary()) -> {ok,
        {stoiridh@internal@types:constraint_result(),
            stoiridh@version:version()}} |
    {error, constraint_error()}.
parse_loosely(Constraint) ->
    _pipe = Constraint,
    _pipe@1 = gleam@string:trim(_pipe),
    _pipe@2 = gleam@string:to_graphemes(_pipe@1),
    _pipe@3 = gleam@list:try_fold(
        _pipe@2,
        {stoiridh@internal@cursor:new(), strict, <<""/utf8>>},
        fun(State, Current_char) ->
            {Cursor, Result, Version} = State,
            case Current_char of
                <<"."/utf8>> ->
                    {ok,
                        {stoiridh@internal@cursor:next(Cursor),
                            Result,
                            <<Version/binary, "."/utf8>>}};

                <<"*"/utf8>> ->
                    gleam@bool:guard(
                        not (gleam@string:is_empty(Version) orelse gleam@string:ends_with(
                            Version,
                            <<"."/utf8>>
                        )),
                        {error, invalid_constraint},
                        fun() ->
                            New_result = case Cursor of
                                {cursor, major} ->
                                    {wildcard, major};

                                {cursor, minor} ->
                                    {wildcard, minor};

                                {cursor, patch} ->
                                    {wildcard, patch}
                            end,
                            {ok,
                                {Cursor,
                                    New_result,
                                    <<Version/binary, "0"/utf8>>}}
                        end
                    );

                <<"x"/utf8>> ->
                    gleam@bool:guard(
                        not (gleam@string:is_empty(Version) orelse gleam@string:ends_with(
                            Version,
                            <<"."/utf8>>
                        )),
                        {error, invalid_constraint},
                        fun() ->
                            New_result = case Cursor of
                                {cursor, major} ->
                                    {wildcard, major};

                                {cursor, minor} ->
                                    {wildcard, minor};

                                {cursor, patch} ->
                                    {wildcard, patch}
                            end,
                            {ok,
                                {Cursor,
                                    New_result,
                                    <<Version/binary, "0"/utf8>>}}
                        end
                    );

                <<"X"/utf8>> ->
                    gleam@bool:guard(
                        not (gleam@string:is_empty(Version) orelse gleam@string:ends_with(
                            Version,
                            <<"."/utf8>>
                        )),
                        {error, invalid_constraint},
                        fun() ->
                            New_result = case Cursor of
                                {cursor, major} ->
                                    {wildcard, major};

                                {cursor, minor} ->
                                    {wildcard, minor};

                                {cursor, patch} ->
                                    {wildcard, patch}
                            end,
                            {ok,
                                {Cursor,
                                    New_result,
                                    <<Version/binary, "0"/utf8>>}}
                        end
                    );

                C ->
                    case gleam@int:parse(C) of
                        {ok, _} ->
                            {ok, {Cursor, Result, <<Version/binary, C/binary>>}};

                        {error, _} ->
                            {error, invalid_constraint}
                    end
            end
        end
    ),
    gleam@result:then(_pipe@3, fun transform_to_version_constraint/1).

-spec next_minor_version(stoiridh@version:version()) -> {ok,
        stoiridh@version:version()} |
    {error, stoiridh@version:version_error()}.
next_minor_version(Version) ->
    stoiridh@version:new(
        stoiridh@version:major(Version),
        stoiridh@version:minor(Version) + 1,
        0
    ).

-spec next_major_version(stoiridh@version:version()) -> {ok,
        stoiridh@version:version()} |
    {error, stoiridh@version:version_error()}.
next_major_version(Version) ->
    stoiridh@version:new(stoiridh@version:major(Version) + 1, 0, 0).

-spec parse_version_constraint(operator(), binary()) -> {ok,
        list({operator(), stoiridh@version:version()})} |
    {error, constraint_error()}.
parse_version_constraint(Operator, Constraint) ->
    gleam@result:'try'(
        begin
            _pipe = Constraint,
            _pipe@1 = parse_loosely(_pipe),
            gleam@result:then(
                _pipe@1,
                fun(State) ->
                    {Constraint_result, Version} = State,
                    case Constraint_result of
                        strict ->
                            {ok, [{Operator, Version}]};

                        {wildcard, major} ->
                            {error, invalid_constraint};

                        {partial, patch} ->
                            case Operator of
                                less_than_or_equal_to ->
                                    gleam@result:'try'(
                                        begin
                                            _pipe@2 = next_minor_version(
                                                Version
                                            ),
                                            gleam@result:map_error(
                                                _pipe@2,
                                                fun(_) -> invalid_constraint end
                                            )
                                        end,
                                        fun(Nmv) -> {ok, [{less_than, Nmv}]} end
                                    );

                                _ ->
                                    {ok, [{Operator, Version}]}
                            end;

                        {wildcard, patch} ->
                            case Operator of
                                less_than_or_equal_to ->
                                    gleam@result:'try'(
                                        begin
                                            _pipe@2 = next_minor_version(
                                                Version
                                            ),
                                            gleam@result:map_error(
                                                _pipe@2,
                                                fun(_) -> invalid_constraint end
                                            )
                                        end,
                                        fun(Nmv) -> {ok, [{less_than, Nmv}]} end
                                    );

                                _ ->
                                    {ok, [{Operator, Version}]}
                            end;

                        {partial, minor} ->
                            case Operator of
                                greater_than ->
                                    gleam@result:'try'(
                                        begin
                                            _pipe@3 = next_minor_version(
                                                Version
                                            ),
                                            gleam@result:map_error(
                                                _pipe@3,
                                                fun(_) -> invalid_constraint end
                                            )
                                        end,
                                        fun(Nmv@1) ->
                                            {ok,
                                                [{greater_than_or_equal_to,
                                                        Nmv@1}]}
                                        end
                                    );

                                less_than_or_equal_to ->
                                    gleam@result:'try'(
                                        begin
                                            _pipe@4 = next_minor_version(
                                                Version
                                            ),
                                            gleam@result:map_error(
                                                _pipe@4,
                                                fun(_) -> invalid_constraint end
                                            )
                                        end,
                                        fun(Nmv@2) ->
                                            {ok, [{less_than, Nmv@2}]}
                                        end
                                    );

                                _ ->
                                    {ok, [{Operator, Version}]}
                            end;

                        {partial, major} ->
                            case Operator of
                                greater_than ->
                                    gleam@result:'try'(
                                        begin
                                            _pipe@5 = next_major_version(
                                                Version
                                            ),
                                            gleam@result:map_error(
                                                _pipe@5,
                                                fun(_) -> invalid_constraint end
                                            )
                                        end,
                                        fun(Nmv@3) ->
                                            {ok,
                                                [{greater_than_or_equal_to,
                                                        Nmv@3}]}
                                        end
                                    );

                                less_than_or_equal_to ->
                                    gleam@result:'try'(
                                        begin
                                            _pipe@6 = next_major_version(
                                                Version
                                            ),
                                            gleam@result:map_error(
                                                _pipe@6,
                                                fun(_) -> invalid_constraint end
                                            )
                                        end,
                                        fun(Nmv@4) ->
                                            {ok, [{less_than, Nmv@4}]}
                                        end
                                    );

                                _ ->
                                    {ok, [{Operator, Version}]}
                            end;

                        {wildcard, minor} ->
                            case Operator of
                                greater_than ->
                                    gleam@result:'try'(
                                        begin
                                            _pipe@5 = next_major_version(
                                                Version
                                            ),
                                            gleam@result:map_error(
                                                _pipe@5,
                                                fun(_) -> invalid_constraint end
                                            )
                                        end,
                                        fun(Nmv@3) ->
                                            {ok,
                                                [{greater_than_or_equal_to,
                                                        Nmv@3}]}
                                        end
                                    );

                                less_than_or_equal_to ->
                                    gleam@result:'try'(
                                        begin
                                            _pipe@6 = next_major_version(
                                                Version
                                            ),
                                            gleam@result:map_error(
                                                _pipe@6,
                                                fun(_) -> invalid_constraint end
                                            )
                                        end,
                                        fun(Nmv@4) ->
                                            {ok, [{less_than, Nmv@4}]}
                                        end
                                    );

                                _ ->
                                    {ok, [{Operator, Version}]}
                            end
                    end
                end
            )
        end,
        fun(Constraints) -> {ok, Constraints} end
    ).

-spec parse_equality_version_constraint(binary()) -> {ok,
        list({operator(), stoiridh@version:version()})} |
    {error, constraint_error()}.
parse_equality_version_constraint(Constraint) ->
    gleam@result:'try'(
        begin
            _pipe = Constraint,
            _pipe@1 = parse_loosely(_pipe),
            gleam@result:then(
                _pipe@1,
                fun(State) ->
                    {Constraint_result, Version} = State,
                    case Constraint_result of
                        strict ->
                            {ok, [{equal, Version}]};

                        {wildcard, patch} ->
                            gleam@result:'try'(
                                begin
                                    _pipe@2 = next_minor_version(Version),
                                    gleam@result:map_error(
                                        _pipe@2,
                                        fun(_) -> invalid_constraint end
                                    )
                                end,
                                fun(Nmv) ->
                                    {ok,
                                        [{greater_than_or_equal_to, Version},
                                            {less_than, Nmv}]}
                                end
                            );

                        {partial, patch} ->
                            gleam@result:'try'(
                                begin
                                    _pipe@2 = next_minor_version(Version),
                                    gleam@result:map_error(
                                        _pipe@2,
                                        fun(_) -> invalid_constraint end
                                    )
                                end,
                                fun(Nmv) ->
                                    {ok,
                                        [{greater_than_or_equal_to, Version},
                                            {less_than, Nmv}]}
                                end
                            );

                        {wildcard, minor} ->
                            gleam@result:'try'(
                                begin
                                    _pipe@3 = next_major_version(Version),
                                    gleam@result:map_error(
                                        _pipe@3,
                                        fun(_) -> invalid_constraint end
                                    )
                                end,
                                fun(Nmv@1) ->
                                    {ok,
                                        [{greater_than_or_equal_to, Version},
                                            {less_than, Nmv@1}]}
                                end
                            );

                        {partial, minor} ->
                            gleam@result:'try'(
                                begin
                                    _pipe@4 = next_minor_version(Version),
                                    gleam@result:map_error(
                                        _pipe@4,
                                        fun(_) -> invalid_constraint end
                                    )
                                end,
                                fun(Nmv@2) ->
                                    {ok,
                                        [{greater_than_or_equal_to, Version},
                                            {less_than, Nmv@2}]}
                                end
                            );

                        {wildcard, major} ->
                            case stoiridh@version:to_string(Version) of
                                <<"0.0.0"/utf8>> ->
                                    {ok, [{greater_than_or_equal_to, Version}]};

                                _ ->
                                    gleam@result:'try'(
                                        begin
                                            _pipe@5 = next_major_version(
                                                Version
                                            ),
                                            gleam@result:map_error(
                                                _pipe@5,
                                                fun(_) -> invalid_constraint end
                                            )
                                        end,
                                        fun(Nmv@3) ->
                                            {ok,
                                                [{greater_than_or_equal_to,
                                                        Version},
                                                    {less_than, Nmv@3}]}
                                        end
                                    )
                            end;

                        {partial, major} ->
                            gleam@result:'try'(
                                begin
                                    _pipe@6 = next_major_version(Version),
                                    gleam@result:map_error(
                                        _pipe@6,
                                        fun(_) -> invalid_constraint end
                                    )
                                end,
                                fun(Nmv@4) ->
                                    {ok,
                                        [{greater_than_or_equal_to, Version},
                                            {less_than, Nmv@4}]}
                                end
                            )
                    end
                end
            )
        end,
        fun(Constraints) -> {ok, Constraints} end
    ).

-spec parse_caret_version_constraint(binary()) -> {ok,
        list({operator(), stoiridh@version:version()})} |
    {error, constraint_error()}.
parse_caret_version_constraint(Constraint) ->
    gleam@result:'try'(
        begin
            _pipe = Constraint,
            _pipe@1 = parse_loosely(_pipe),
            gleam@result:then(
                _pipe@1,
                fun(State) ->
                    {Constraint_result, Version} = State,
                    case Constraint_result of
                        strict ->
                            Major = begin
                                _pipe@2 = Version,
                                stoiridh@version:major(_pipe@2)
                            end,
                            Minor = begin
                                _pipe@3 = Version,
                                stoiridh@version:minor(_pipe@3)
                            end,
                            case {Major, Minor} of
                                {0, 0} ->
                                    {ok, [{equal, Version}]};

                                {0, _} ->
                                    gleam@result:'try'(
                                        begin
                                            _pipe@4 = Version,
                                            _pipe@5 = next_minor_version(
                                                _pipe@4
                                            ),
                                            gleam@result:map_error(
                                                _pipe@5,
                                                fun(_) -> invalid_constraint end
                                            )
                                        end,
                                        fun(Nmv) ->
                                            {ok,
                                                [{greater_than_or_equal_to,
                                                        Version},
                                                    {less_than, Nmv}]}
                                        end
                                    );

                                {_, _} ->
                                    gleam@result:'try'(
                                        begin
                                            _pipe@6 = Version,
                                            _pipe@7 = next_major_version(
                                                _pipe@6
                                            ),
                                            gleam@result:map_error(
                                                _pipe@7,
                                                fun(_) -> invalid_constraint end
                                            )
                                        end,
                                        fun(Nmv@1) ->
                                            {ok,
                                                [{greater_than_or_equal_to,
                                                        Version},
                                                    {less_than, Nmv@1}]}
                                        end
                                    )
                            end;

                        {partial, minor} ->
                            Major@1 = begin
                                _pipe@8 = Version,
                                stoiridh@version:major(_pipe@8)
                            end,
                            gleam@result:'try'(case Major@1 of
                                    0 ->
                                        gleam@result:map(
                                            begin
                                                _pipe@9 = Version,
                                                _pipe@10 = next_minor_version(
                                                    _pipe@9
                                                ),
                                                gleam@result:map_error(
                                                    _pipe@10,
                                                    fun(_) ->
                                                        invalid_constraint
                                                    end
                                                )
                                            end,
                                            fun(Nmv@2) -> Nmv@2 end
                                        );

                                    _ ->
                                        gleam@result:map(
                                            begin
                                                _pipe@11 = Version,
                                                _pipe@12 = next_major_version(
                                                    _pipe@11
                                                ),
                                                gleam@result:map_error(
                                                    _pipe@12,
                                                    fun(_) ->
                                                        invalid_constraint
                                                    end
                                                )
                                            end,
                                            fun(Nmv@3) -> Nmv@3 end
                                        )
                                end, fun(Nmv@4) ->
                                    {ok,
                                        [{greater_than_or_equal_to, Version},
                                            {less_than, Nmv@4}]}
                                end);

                        {wildcard, patch} ->
                            Major@1 = begin
                                _pipe@8 = Version,
                                stoiridh@version:major(_pipe@8)
                            end,
                            gleam@result:'try'(case Major@1 of
                                    0 ->
                                        gleam@result:map(
                                            begin
                                                _pipe@9 = Version,
                                                _pipe@10 = next_minor_version(
                                                    _pipe@9
                                                ),
                                                gleam@result:map_error(
                                                    _pipe@10,
                                                    fun(_) ->
                                                        invalid_constraint
                                                    end
                                                )
                                            end,
                                            fun(Nmv@2) -> Nmv@2 end
                                        );

                                    _ ->
                                        gleam@result:map(
                                            begin
                                                _pipe@11 = Version,
                                                _pipe@12 = next_major_version(
                                                    _pipe@11
                                                ),
                                                gleam@result:map_error(
                                                    _pipe@12,
                                                    fun(_) ->
                                                        invalid_constraint
                                                    end
                                                )
                                            end,
                                            fun(Nmv@3) -> Nmv@3 end
                                        )
                                end, fun(Nmv@4) ->
                                    {ok,
                                        [{greater_than_or_equal_to, Version},
                                            {less_than, Nmv@4}]}
                                end);

                        {wildcard, major} ->
                            {error, invalid_constraint};

                        _ ->
                            gleam@result:'try'(
                                begin
                                    _pipe@13 = Version,
                                    _pipe@14 = next_major_version(_pipe@13),
                                    gleam@result:map_error(
                                        _pipe@14,
                                        fun(_) -> invalid_constraint end
                                    )
                                end,
                                fun(Nmv@5) ->
                                    {ok,
                                        [{greater_than_or_equal_to, Version},
                                            {less_than, Nmv@5}]}
                                end
                            )
                    end
                end
            )
        end,
        fun(Constraints) -> {ok, Constraints} end
    ).

-spec parse_tilde_version_constraint(binary()) -> {ok,
        list({operator(), stoiridh@version:version()})} |
    {error, constraint_error()}.
parse_tilde_version_constraint(Constraint) ->
    gleam@result:'try'(
        begin
            _pipe = Constraint,
            _pipe@1 = parse_loosely(_pipe),
            gleam@result:then(
                _pipe@1,
                fun(State) ->
                    {Constraint_result, Version} = State,
                    case Constraint_result of
                        strict ->
                            gleam@result:'try'(
                                begin
                                    _pipe@2 = Version,
                                    _pipe@3 = next_minor_version(_pipe@2),
                                    gleam@result:map_error(
                                        _pipe@3,
                                        fun(_) -> invalid_constraint end
                                    )
                                end,
                                fun(Nmv) ->
                                    {ok,
                                        [{greater_than_or_equal_to, Version},
                                            {less_than, Nmv}]}
                                end
                            );

                        {partial, minor} ->
                            gleam@result:'try'(
                                begin
                                    _pipe@2 = Version,
                                    _pipe@3 = next_minor_version(_pipe@2),
                                    gleam@result:map_error(
                                        _pipe@3,
                                        fun(_) -> invalid_constraint end
                                    )
                                end,
                                fun(Nmv) ->
                                    {ok,
                                        [{greater_than_or_equal_to, Version},
                                            {less_than, Nmv}]}
                                end
                            );

                        {wildcard, patch} ->
                            gleam@result:'try'(
                                begin
                                    _pipe@2 = Version,
                                    _pipe@3 = next_minor_version(_pipe@2),
                                    gleam@result:map_error(
                                        _pipe@3,
                                        fun(_) -> invalid_constraint end
                                    )
                                end,
                                fun(Nmv) ->
                                    {ok,
                                        [{greater_than_or_equal_to, Version},
                                            {less_than, Nmv}]}
                                end
                            );

                        {wildcard, major} ->
                            {error, invalid_constraint};

                        _ ->
                            gleam@result:'try'(
                                begin
                                    _pipe@4 = Version,
                                    _pipe@5 = next_major_version(_pipe@4),
                                    gleam@result:map_error(
                                        _pipe@5,
                                        fun(_) -> invalid_constraint end
                                    )
                                end,
                                fun(Nmv@1) ->
                                    {ok,
                                        [{greater_than_or_equal_to, Version},
                                            {less_than, Nmv@1}]}
                                end
                            )
                    end
                end
            )
        end,
        fun(Constraints) -> {ok, Constraints} end
    ).

-spec parse_constraint(binary()) -> {ok,
        list({operator(), stoiridh@version:version()})} |
    {error, constraint_error()}.
parse_constraint(Constraint) ->
    case Constraint of
        <<"^"/utf8, C/binary>> ->
            gleam@result:'try'(
                parse_caret_version_constraint(C),
                fun(Versions) -> {ok, Versions} end
            );

        <<"~"/utf8, C@1/binary>> ->
            gleam@result:'try'(
                parse_tilde_version_constraint(C@1),
                fun(Versions@1) -> {ok, Versions@1} end
            );

        <<"<="/utf8, C@2/binary>> ->
            gleam@result:'try'(
                parse_version_constraint(less_than_or_equal_to, C@2),
                fun(Versions@2) -> {ok, Versions@2} end
            );

        <<"<"/utf8, C@3/binary>> ->
            gleam@result:'try'(
                parse_version_constraint(less_than, C@3),
                fun(Versions@3) -> {ok, Versions@3} end
            );

        <<">="/utf8, C@4/binary>> ->
            gleam@result:'try'(
                parse_version_constraint(greater_than_or_equal_to, C@4),
                fun(Versions@4) -> {ok, Versions@4} end
            );

        <<">"/utf8, C@5/binary>> ->
            gleam@result:'try'(
                parse_version_constraint(greater_than, C@5),
                fun(Versions@5) -> {ok, Versions@5} end
            );

        <<"="/utf8, C@6/binary>> ->
            gleam@result:'try'(
                parse_equality_version_constraint(C@6),
                fun(Versions@6) -> {ok, Versions@6} end
            );

        C@6 ->
            gleam@result:'try'(
                parse_equality_version_constraint(C@6),
                fun(Versions@6) -> {ok, Versions@6} end
            )
    end.

-spec do_parse_constraints(list(binary())) -> {ok, constraint()} |
    {error, constraint_error()}.
do_parse_constraints(Constraints) ->
    gleam@result:'try'(
        begin
            _pipe = Constraints,
            gleam@list:try_fold(
                _pipe,
                {constraint, []},
                fun(Cs, C) ->
                    gleam@result:'try'(
                        parse_constraint(gleam@string:trim(C)),
                        fun(Parsed_constraints) ->
                            {ok,
                                {constraint,
                                    lists:append(
                                        erlang:element(2, Cs),
                                        Parsed_constraints
                                    )}}
                        end
                    )
                end
            )
        end,
        fun(Parsed_constraints@1) -> {ok, Parsed_constraints@1} end
    ).

-spec parse_constraints(binary()) -> {ok, constraint()} |
    {error, constraint_error()}.
parse_constraints(Constraint) ->
    case Constraint of
        <<"=*"/utf8>> ->
            gleam@result:'try'(
                begin
                    _pipe = stoiridh@version:new(0, 0, 0),
                    gleam@result:map_error(
                        _pipe,
                        fun(_) -> invalid_constraint end
                    )
                end,
                fun(Version) ->
                    {ok, {constraint, [{greater_than_or_equal_to, Version}]}}
                end
            );

        <<"*"/utf8>> ->
            gleam@result:'try'(
                begin
                    _pipe = stoiridh@version:new(0, 0, 0),
                    gleam@result:map_error(
                        _pipe,
                        fun(_) -> invalid_constraint end
                    )
                end,
                fun(Version) ->
                    {ok, {constraint, [{greater_than_or_equal_to, Version}]}}
                end
            );

        _ ->
            Constraints = gleam@string:split(Constraint, <<","/utf8>>),
            case gleam@list:is_empty(Constraints) of
                true ->
                    do_parse_constraints([Constraint]);

                false ->
                    do_parse_constraints(Constraints)
            end
    end.

-spec new(binary()) -> {ok, constraint()} | {error, constraint_error()}.
new(Constraints) ->
    parse_constraints(Constraints).
