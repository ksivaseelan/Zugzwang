-module(stoiridh@version).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/3, with_build_metadata/2, parse/1, with_prerelease/2, major/1, minor/1, patch/1, prerelease/1, build_metadata/1, to_string/1, compare/2]).
-export_type([version_error/0, version/0, parser/0]).

-type version_error() :: invalid_version |
    invalid_major_version |
    invalid_minor_version |
    invalid_patch_version |
    invalid_prerelease |
    invalid_build_metadata |
    {negative_value, binary()}.

-opaque version() :: {version,
        integer(),
        integer(),
        integer(),
        gleam@option:option(binary()),
        gleam@option:option(binary())}.

-type parser() :: {parser, list(binary()), gleam@dict:dict(binary(), binary())}.

-spec new(integer(), integer(), integer()) -> {ok, version()} |
    {error, version_error()}.
new(Major, Minor, Patch) ->
    gleam@result:'try'(
        begin
            _pipe = Major,
            _pipe@1 = stoiridh@internal@int:is_positive(_pipe),
            gleam@result:map_error(
                _pipe@1,
                fun(Error) -> {negative_value, Error} end
            )
        end,
        fun(Major@1) ->
            gleam@result:'try'(
                begin
                    _pipe@2 = Minor,
                    _pipe@3 = stoiridh@internal@int:is_positive(_pipe@2),
                    gleam@result:map_error(
                        _pipe@3,
                        fun(Error@1) -> {negative_value, Error@1} end
                    )
                end,
                fun(Minor@1) ->
                    gleam@result:'try'(
                        begin
                            _pipe@4 = Patch,
                            _pipe@5 = stoiridh@internal@int:is_positive(_pipe@4),
                            gleam@result:map_error(
                                _pipe@5,
                                fun(Error@2) -> {negative_value, Error@2} end
                            )
                        end,
                        fun(Patch@1) ->
                            {ok,
                                {version, Major@1, Minor@1, Patch@1, none, none}}
                        end
                    )
                end
            )
        end
    ).

-spec check_version_parts(list(integer())) -> {ok,
        {integer(), integer(), integer()}} |
    {error, version_error()}.
check_version_parts(Version_parts) ->
    case Version_parts of
        [Major, Minor, Patch] ->
            {ok, {Major, Minor, Patch}};

        [_, _] ->
            {error, invalid_patch_version};

        [_] ->
            {error, invalid_minor_version};

        [] ->
            {error, invalid_major_version};

        [_, _, _ | _] ->
            {error, invalid_version}
    end.

-spec do_parse_version_numbers(list(binary()), binary(), list(integer())) -> {ok,
        parser()} |
    {error, version_error()}.
do_parse_version_numbers(Input, Buffer, Version_parts) ->
    case Input of
        [Elem] ->
            Version_number = <<Buffer/binary, Elem/binary>>,
            gleam@bool:guard(
                gleam@string:starts_with(Version_number, <<"00"/utf8>>),
                {error, invalid_version},
                fun() -> case gleam@int:parse(Version_number) of
                        {ok, Version_part} ->
                            do_parse_version_numbers(
                                [],
                                <<""/utf8>>,
                                begin
                                    _pipe = Version_parts,
                                    lists:append(_pipe, [Version_part])
                                end
                            );

                        {error, _} ->
                            {error, invalid_version}
                    end end
            );

        [Elem@1, Delimiter | Remaining_graphemes] when (Delimiter =:= <<"-"/utf8>>) orelse (Delimiter =:= <<"+"/utf8>>) ->
            case gleam@int:parse(<<Buffer/binary, Elem@1/binary>>) of
                {ok, Version_part@1} ->
                    Version_parts@1 = begin
                        _pipe@1 = Version_parts,
                        lists:append(_pipe@1, [Version_part@1])
                    end,
                    _pipe@2 = Version_parts@1,
                    _pipe@3 = check_version_parts(_pipe@2),
                    gleam@result:map(
                        _pipe@3,
                        fun(Version_parts@2) ->
                            {Major, Minor, Patch} = Version_parts@2,
                            {parser,
                                [Delimiter | Remaining_graphemes],
                                begin
                                    _pipe@4 = gleam@dict:new(),
                                    _pipe@5 = gleam@dict:insert(
                                        _pipe@4,
                                        <<"major"/utf8>>,
                                        gleam@int:to_string(Major)
                                    ),
                                    _pipe@6 = gleam@dict:insert(
                                        _pipe@5,
                                        <<"minor"/utf8>>,
                                        gleam@int:to_string(Minor)
                                    ),
                                    gleam@dict:insert(
                                        _pipe@6,
                                        <<"patch"/utf8>>,
                                        gleam@int:to_string(Patch)
                                    )
                                end}
                        end
                    );

                {error, _} ->
                    {error, invalid_version}
            end;

        [Elem@2, <<"."/utf8>> | Remaining_graphemes@1] ->
            Version_number@1 = <<Buffer/binary, Elem@2/binary>>,
            gleam@bool:guard(
                gleam@string:starts_with(Version_number@1, <<"00"/utf8>>),
                {error, invalid_version},
                fun() -> case gleam@int:parse(Version_number@1) of
                        {ok, Version_part@2} ->
                            do_parse_version_numbers(
                                Remaining_graphemes@1,
                                <<""/utf8>>,
                                begin
                                    _pipe@7 = Version_parts,
                                    lists:append(_pipe@7, [Version_part@2])
                                end
                            );

                        {error, _} ->
                            {error, invalid_version}
                    end end
            );

        [<<"."/utf8>> | Remaining_graphemes@2] ->
            do_parse_version_numbers(
                Remaining_graphemes@2,
                Buffer,
                Version_parts
            );

        [Elem@3 | Remaining_graphemes@3] ->
            case gleam@int:parse(Elem@3) of
                {ok, _} ->
                    do_parse_version_numbers(
                        Remaining_graphemes@3,
                        <<Buffer/binary, Elem@3/binary>>,
                        Version_parts
                    );

                {error, _} ->
                    {error, invalid_version}
            end;

        [] ->
            _pipe@8 = Version_parts,
            _pipe@9 = check_version_parts(_pipe@8),
            gleam@result:map(
                _pipe@9,
                fun(Version_parts@3) ->
                    {Major@1, Minor@1, Patch@1} = Version_parts@3,
                    {parser,
                        [],
                        begin
                            _pipe@10 = gleam@dict:new(),
                            _pipe@11 = gleam@dict:insert(
                                _pipe@10,
                                <<"major"/utf8>>,
                                gleam@int:to_string(Major@1)
                            ),
                            _pipe@12 = gleam@dict:insert(
                                _pipe@11,
                                <<"minor"/utf8>>,
                                gleam@int:to_string(Minor@1)
                            ),
                            gleam@dict:insert(
                                _pipe@12,
                                <<"patch"/utf8>>,
                                gleam@int:to_string(Patch@1)
                            )
                        end}
                end
            )
    end.

-spec parse_version_numbers(list(binary())) -> {ok, parser()} |
    {error, version_error()}.
parse_version_numbers(Input) ->
    do_parse_version_numbers(Input, <<""/utf8>>, []).

-spec transform_to_version(gleam@dict:dict(binary(), binary())) -> {ok,
        version()} |
    {error, version_error()}.
transform_to_version(Input) ->
    gleam@result:'try'(
        begin
            _pipe = Input,
            _pipe@1 = gleam@dict:get(_pipe, <<"major"/utf8>>),
            _pipe@2 = gleam@result:map_error(
                _pipe@1,
                fun(_) -> invalid_major_version end
            ),
            gleam@result:then(
                _pipe@2,
                fun(Major) -> case gleam@int:parse(Major) of
                        {ok, Major@1} ->
                            {ok, Major@1};

                        {error, _} ->
                            {error, invalid_major_version}
                    end end
            )
        end,
        fun(Major@2) ->
            gleam@result:'try'(
                begin
                    _pipe@3 = Input,
                    _pipe@4 = gleam@dict:get(_pipe@3, <<"minor"/utf8>>),
                    _pipe@5 = gleam@result:map_error(
                        _pipe@4,
                        fun(_) -> invalid_minor_version end
                    ),
                    gleam@result:then(
                        _pipe@5,
                        fun(Minor) -> case gleam@int:parse(Minor) of
                                {ok, Minor@1} ->
                                    {ok, Minor@1};

                                {error, _} ->
                                    {error, invalid_minor_version}
                            end end
                    )
                end,
                fun(Minor@2) ->
                    gleam@result:'try'(
                        begin
                            _pipe@6 = Input,
                            _pipe@7 = gleam@dict:get(_pipe@6, <<"patch"/utf8>>),
                            _pipe@8 = gleam@result:map_error(
                                _pipe@7,
                                fun(_) -> invalid_patch_version end
                            ),
                            gleam@result:then(
                                _pipe@8,
                                fun(Patch) -> case gleam@int:parse(Patch) of
                                        {ok, Patch@1} ->
                                            {ok, Patch@1};

                                        {error, _} ->
                                            {error, invalid_patch_version}
                                    end end
                            )
                        end,
                        fun(Patch@2) ->
                            Prerelease = begin
                                _pipe@9 = Input,
                                _pipe@10 = gleam@dict:get(
                                    _pipe@9,
                                    <<"prerelease"/utf8>>
                                ),
                                gleam@option:from_result(_pipe@10)
                            end,
                            Build_metadata = begin
                                _pipe@11 = Input,
                                _pipe@12 = gleam@dict:get(
                                    _pipe@11,
                                    <<"build_metadata"/utf8>>
                                ),
                                gleam@option:from_result(_pipe@12)
                            end,
                            {ok,
                                {version,
                                    Major@2,
                                    Minor@2,
                                    Patch@2,
                                    Prerelease,
                                    Build_metadata}}
                        end
                    )
                end
            )
        end
    ).

-spec validate_identifiers(list(binary()), fun((binary()) -> boolean())) -> {ok,
        gleam@option:option(binary())} |
    {error, version_error()}.
validate_identifiers(Identifiers, Predicate) ->
    case begin
        _pipe = Identifiers,
        gleam@list:all(_pipe, Predicate)
    end of
        true ->
            {ok, {some, gleam@string:join(Identifiers, <<"."/utf8>>)}};

        false ->
            {error, invalid_prerelease}
    end.

-spec is_valid_identifier(binary()) -> boolean().
is_valid_identifier(Identifier) ->
    _pipe = Identifier,
    _pipe@1 = gleam@string:to_graphemes(_pipe),
    gleam@list:all(
        _pipe@1,
        fun(C) ->
            C@1 = begin
                _assert_subject = gleam@string:to_utf_codepoints(C),
                [Char | _] = case _assert_subject of
                    [_ | _] -> _assert_subject;
                    _assert_fail ->
                        erlang:error(#{gleam_error => let_assert,
                                    message => <<"Assertion pattern match failed"/utf8>>,
                                    value => _assert_fail,
                                    module => <<"stoiridh/version"/utf8>>,
                                    function => <<"is_valid_identifier"/utf8>>,
                                    line => 598})
                end,
                _pipe@2 = Char,
                gleam@string:utf_codepoint_to_int(_pipe@2)
            end,
            (((C@1 =:= 45) orelse ((C@1 >= 48) andalso (C@1 =< 57))) orelse ((C@1
            >= 65)
            andalso (C@1 =< 90)))
            orelse ((C@1 >= 97) andalso (C@1 =< 122))
        end
    ).

-spec parse_build_metadata(parser()) -> {ok,
        gleam@dict:dict(binary(), binary())} |
    {error, version_error()}.
parse_build_metadata(Parser) ->
    case gleam@list:first(erlang:element(2, Parser)) of
        {ok, <<"+"/utf8>>} ->
            case gleam@list:rest(erlang:element(2, Parser)) of
                {ok, Input} ->
                    gleam@result:'try'(
                        begin
                            _pipe = Input,
                            _pipe@1 = gleam@string:join(_pipe, <<""/utf8>>),
                            _pipe@2 = gleam@string:split(_pipe@1, <<"."/utf8>>),
                            _pipe@3 = validate_identifiers(
                                _pipe@2,
                                fun(I) ->
                                    not gleam@string:is_empty(I) andalso is_valid_identifier(
                                        I
                                    )
                                end
                            ),
                            gleam@result:replace_error(
                                _pipe@3,
                                invalid_build_metadata
                            )
                        end,
                        fun(Build_metadata) -> case Build_metadata of
                                {some, Bm} ->
                                    {ok,
                                        begin
                                            _pipe@4 = erlang:element(3, Parser),
                                            gleam@dict:insert(
                                                _pipe@4,
                                                <<"build_metadata"/utf8>>,
                                                Bm
                                            )
                                        end};

                                none ->
                                    {error, invalid_build_metadata}
                            end end
                    );

                {error, _} ->
                    {error, invalid_build_metadata}
            end;

        _ ->
            {ok, erlang:element(3, Parser)}
    end.

-spec with_build_metadata({ok, version()} | {error, version_error()}, binary()) -> {ok,
        version()} |
    {error, version_error()}.
with_build_metadata(Version, Build_metadata) ->
    gleam@result:'try'(
        begin
            _pipe = Build_metadata,
            _pipe@1 = gleam@string:split(_pipe, <<"."/utf8>>),
            _pipe@2 = validate_identifiers(
                _pipe@1,
                fun(I) ->
                    not gleam@string:is_empty(I) andalso is_valid_identifier(I)
                end
            ),
            gleam@result:replace_error(_pipe@2, invalid_build_metadata)
        end,
        fun(Build_metadata@1) -> _pipe@3 = Version,
            gleam@result:map(
                _pipe@3,
                fun(V) -> erlang:setelement(6, V, Build_metadata@1) end
            ) end
    ).

-spec has_leading_zeros(binary()) -> boolean().
has_leading_zeros(Identifier) ->
    _pipe = Identifier,
    gleam@string:starts_with(_pipe, <<"00"/utf8>>).

-spec do_parse_prerelease(parser(), list(binary()), binary()) -> {ok, parser()} |
    {error, version_error()}.
do_parse_prerelease(Parser, Input, Buffer) ->
    case Input of
        [<<"+"/utf8>> | Remaining_graphemes] ->
            gleam@result:'try'(
                begin
                    _pipe = Buffer,
                    _pipe@1 = gleam@string:split(_pipe, <<"."/utf8>>),
                    _pipe@2 = validate_identifiers(
                        _pipe@1,
                        fun(I) ->
                            not (gleam@string:is_empty(I) orelse has_leading_zeros(
                                I
                            ))
                            andalso is_valid_identifier(I)
                        end
                    ),
                    gleam@result:replace_error(_pipe@2, invalid_prerelease)
                end,
                fun(Prerelease) -> case Prerelease of
                        {some, P} ->
                            {ok,
                                {parser,
                                    [<<"+"/utf8>> | Remaining_graphemes],
                                    begin
                                        _pipe@3 = erlang:element(3, Parser),
                                        gleam@dict:insert(
                                            _pipe@3,
                                            <<"prerelease"/utf8>>,
                                            P
                                        )
                                    end}};

                        none ->
                            {error, invalid_prerelease}
                    end end
            );

        [Elem | Remaining_graphemes@1] ->
            do_parse_prerelease(
                Parser,
                Remaining_graphemes@1,
                <<Buffer/binary, Elem/binary>>
            );

        [] ->
            gleam@result:'try'(
                begin
                    _pipe@4 = Buffer,
                    _pipe@5 = gleam@string:split(_pipe@4, <<"."/utf8>>),
                    _pipe@6 = validate_identifiers(
                        _pipe@5,
                        fun(I@1) ->
                            not (gleam@string:is_empty(I@1) orelse has_leading_zeros(
                                I@1
                            ))
                            andalso is_valid_identifier(I@1)
                        end
                    ),
                    gleam@result:replace_error(_pipe@6, invalid_prerelease)
                end,
                fun(Prerelease@1) -> case Prerelease@1 of
                        {some, P@1} ->
                            {ok,
                                {parser,
                                    [],
                                    begin
                                        _pipe@7 = erlang:element(3, Parser),
                                        gleam@dict:insert(
                                            _pipe@7,
                                            <<"prerelease"/utf8>>,
                                            P@1
                                        )
                                    end}};

                        none ->
                            {error, invalid_prerelease}
                    end end
            )
    end.

-spec parse_prerelease(parser()) -> {ok, parser()} | {error, version_error()}.
parse_prerelease(Parser) ->
    case gleam@list:first(erlang:element(2, Parser)) of
        {ok, <<"-"/utf8>>} ->
            case gleam@list:rest(erlang:element(2, Parser)) of
                {ok, Input} ->
                    do_parse_prerelease(Parser, Input, <<""/utf8>>);

                {error, _} ->
                    {error, invalid_prerelease}
            end;

        _ ->
            {ok, Parser}
    end.

-spec parse(binary()) -> {ok, version()} | {error, version_error()}.
parse(Input) ->
    Graphemes = begin
        _pipe = Input,
        gleam@string:to_graphemes(_pipe)
    end,
    gleam@bool:guard(
        gleam@list:is_empty(Graphemes),
        {error, invalid_version},
        fun() -> _pipe@1 = Graphemes,
            _pipe@2 = parse_version_numbers(_pipe@1),
            _pipe@3 = gleam@result:then(_pipe@2, fun parse_prerelease/1),
            _pipe@4 = gleam@result:then(_pipe@3, fun parse_build_metadata/1),
            gleam@result:then(_pipe@4, fun transform_to_version/1) end
    ).

-spec with_prerelease({ok, version()} | {error, version_error()}, binary()) -> {ok,
        version()} |
    {error, version_error()}.
with_prerelease(Version, Prerelease) ->
    gleam@result:'try'(
        begin
            _pipe = Prerelease,
            _pipe@1 = gleam@string:split(_pipe, <<"."/utf8>>),
            validate_identifiers(
                _pipe@1,
                fun(I) ->
                    not (gleam@string:is_empty(I) orelse has_leading_zeros(I))
                    andalso is_valid_identifier(I)
                end
            )
        end,
        fun(Prerelease@1) -> _pipe@2 = Version,
            gleam@result:map(
                _pipe@2,
                fun(V) -> erlang:setelement(5, V, Prerelease@1) end
            ) end
    ).

-spec major(version()) -> integer().
major(Version) ->
    erlang:element(2, Version).

-spec minor(version()) -> integer().
minor(Version) ->
    erlang:element(3, Version).

-spec patch(version()) -> integer().
patch(Version) ->
    erlang:element(4, Version).

-spec prerelease(version()) -> gleam@option:option(binary()).
prerelease(Version) ->
    erlang:element(5, Version).

-spec build_metadata(version()) -> gleam@option:option(binary()).
build_metadata(Version) ->
    erlang:element(6, Version).

-spec to_string(version()) -> binary().
to_string(Version) ->
    V = <<<<<<<<(gleam@int:to_string(erlang:element(2, Version)))/binary,
                    "."/utf8>>/binary,
                (gleam@int:to_string(erlang:element(3, Version)))/binary>>/binary,
            "."/utf8>>/binary,
        (gleam@int:to_string(erlang:element(4, Version)))/binary>>,
    P@1 = begin
        _pipe = Version,
        _pipe@1 = prerelease(_pipe),
        _pipe@2 = gleam@option:then(
            _pipe@1,
            fun(P) -> {some, gleam@string:concat([<<"-"/utf8>>, P])} end
        ),
        gleam@option:unwrap(_pipe@2, <<""/utf8>>)
    end,
    B@1 = begin
        _pipe@3 = Version,
        _pipe@4 = build_metadata(_pipe@3),
        _pipe@5 = gleam@option:then(
            _pipe@4,
            fun(B) -> {some, gleam@string:concat([<<"+"/utf8>>, B])} end
        ),
        gleam@option:unwrap(_pipe@5, <<""/utf8>>)
    end,
    gleam@string:concat([V, P@1, B@1]).

-spec equal_to(version(), version()) -> boolean().
equal_to(A, B) ->
    ((erlang:element(2, A) =:= erlang:element(2, B)) andalso (erlang:element(
        3,
        A
    )
    =:= erlang:element(3, B)))
    andalso (erlang:element(4, A) =:= erlang:element(4, B)).

-spec less(version(), version()) -> boolean().
less(A, B) ->
    ((erlang:element(2, A) < erlang:element(2, B)) orelse ((erlang:element(2, A)
    =:= erlang:element(2, B))
    andalso (erlang:element(3, A) < erlang:element(3, B))))
    orelse (((erlang:element(2, A) =:= erlang:element(2, B)) andalso (erlang:element(
        3,
        A
    )
    =:= erlang:element(3, B)))
    andalso (erlang:element(4, A) < erlang:element(4, B))).

-spec compare_identifiers(binary(), binary()) -> gleam@order:order().
compare_identifiers(A, B) ->
    Left = gleam@int:parse(A),
    Right = gleam@int:parse(B),
    case {Left, Right} of
        {{ok, L}, {ok, R}} ->
            gleam@int:compare(L, R);

        {{ok, _}, {error, _}} ->
            lt;

        {{error, _}, {ok, _}} ->
            gt;

        {{error, _}, {error, _}} ->
            gleam@string:compare(A, B)
    end.

-spec do_compare_prerelease(list(binary()), list(binary())) -> gleam@order:order().
do_compare_prerelease(A, B) ->
    case {A, B} of
        {[A@1], [B@1]} ->
            compare_identifiers(A@1, B@1);

        {[A@2 | Arest], [B@2 | Brest]} ->
            case compare_identifiers(A@2, B@2) of
                eq ->
                    do_compare_prerelease(Arest, Brest);

                gt ->
                    gt;

                lt ->
                    lt
            end;

        {[_], []} ->
            gt;

        {[_ | _], []} ->
            gt;

        {[], [_]} ->
            lt;

        {[], [_ | _]} ->
            lt;

        {[], []} ->
            eq
    end.

-spec compare_prerelease(version(), version()) -> gleam@order:order().
compare_prerelease(A, B) ->
    case {gleam@option:is_some(erlang:element(5, A)),
        gleam@option:is_some(erlang:element(5, B))} of
        {false, false} ->
            eq;

        {false, true} ->
            gt;

        {true, false} ->
            lt;

        {true, true} ->
            A_prerelease = begin
                _pipe = erlang:element(5, A),
                _pipe@1 = gleam@option:unwrap(_pipe, <<""/utf8>>),
                gleam@string:split(_pipe@1, <<"."/utf8>>)
            end,
            B_prerelease = begin
                _pipe@2 = erlang:element(5, B),
                _pipe@3 = gleam@option:unwrap(_pipe@2, <<""/utf8>>),
                gleam@string:split(_pipe@3, <<"."/utf8>>)
            end,
            do_compare_prerelease(A_prerelease, B_prerelease)
    end.

-spec compare(version(), version()) -> gleam@order:order().
compare(A, B) ->
    case equal_to(A, B) of
        true ->
            compare_prerelease(A, B);

        false ->
            case less(A, B) of
                true ->
                    lt;

                false ->
                    gt
            end
    end.
