-module(decipher).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([non_negative_int/1, int_string/1, float_string/1, number/1, number_string/1, non_empty_string/1, non_empty_list/1, all/1, arraylike/1, set/1, exact_set/1, tagged_union/2, enum/1, bool_string/1, iso_8601/1, unix_timestamp/1, http_date/1, uri/1, base16/1, base64/1, semver/1, base64_url_encoded/1, 'when'/2, json_string/1, keys/1, exact_object1/2, exact_object2/3, exact_object3/4, exact_object4/5, exact_object5/6, exact_object6/7, index/2, at/2]).

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 22).
-spec non_negative_int(gleam@dynamic:dynamic_()) -> {ok, integer()} |
    {error, list(gleam@dynamic:decode_error())}.
non_negative_int(Dynamic) ->
    gleam@result:'try'(gleam@dynamic:int(Dynamic), fun(Int) -> case Int >= 0 of
                true ->
                    {ok, Int};

                false ->
                    {error,
                        [{decode_error,
                                <<"A non-negative int"/utf8>>,
                                gleam@int:to_string(Int),
                                []}]}
            end end).

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 42).
-spec int_string(gleam@dynamic:dynamic_()) -> {ok, integer()} |
    {error, list(gleam@dynamic:decode_error())}.
int_string(Dynamic) ->
    gleam@result:'try'(
        gleam@dynamic:string(Dynamic),
        fun(String) -> _pipe = String,
            _pipe@1 = gleam@int:parse(_pipe),
            gleam@result:replace_error(
                _pipe@1,
                [{decode_error, <<"A stringified int"/utf8>>, String, []}]
            ) end
    ).

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 56).
-spec float_string(gleam@dynamic:dynamic_()) -> {ok, float()} |
    {error, list(gleam@dynamic:decode_error())}.
float_string(Dynamic) ->
    gleam@result:'try'(
        gleam@dynamic:string(Dynamic),
        fun(String) -> _pipe = String,
            _pipe@1 = gleam@float:parse(_pipe),
            gleam@result:replace_error(
                _pipe@1,
                [{decode_error, <<"A stringified float"/utf8>>, String, []}]
            ) end
    ).

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 69).
-spec number(gleam@dynamic:dynamic_()) -> {ok, float()} |
    {error, list(gleam@dynamic:decode_error())}.
number(Dynamic) ->
    (gleam@dynamic:any(
        [fun gleam@dynamic:float/1,
            fun(Dynamic@1) -> _pipe = gleam@dynamic:int(Dynamic@1),
                gleam@result:map(_pipe, fun gleam@int:to_float/1) end]
    ))(Dynamic).

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 84).
-spec number_string(gleam@dynamic:dynamic_()) -> {ok, float()} |
    {error, list(gleam@dynamic:decode_error())}.
number_string(Dynamic) ->
    (gleam@dynamic:any(
        [fun float_string/1, fun(Dynamic@1) -> _pipe = int_string(Dynamic@1),
                gleam@result:map(_pipe, fun gleam@int:to_float/1) end]
    ))(Dynamic).

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 134).
-spec non_empty_string(gleam@dynamic:dynamic_()) -> {ok, binary()} |
    {error, list(gleam@dynamic:decode_error())}.
non_empty_string(Dynamic) ->
    gleam@result:'try'(
        gleam@dynamic:string(Dynamic),
        fun(String) -> case String of
                <<""/utf8>> ->
                    {error,
                        [{decode_error,
                                <<"A non-empty string"/utf8>>,
                                <<"An empty string"/utf8>>,
                                []}]};

                _ ->
                    {ok, String}
            end end
    ).

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 193).
-spec non_empty_list(
    fun((gleam@dynamic:dynamic_()) -> {ok, HSY} |
        {error, list(gleam@dynamic:decode_error())})
) -> fun((gleam@dynamic:dynamic_()) -> {ok, list(HSY)} |
    {error, list(gleam@dynamic:decode_error())}).
non_empty_list(Decode) ->
    fun(Dynamic) ->
        gleam@result:'try'(
            (gleam@dynamic:list(Decode))(Dynamic),
            fun(List) -> case gleam@list:is_empty(List) of
                    true ->
                        {error,
                            [{decode_error,
                                    <<"A non-empty list"/utf8>>,
                                    <<"A list with at least 1 item"/utf8>>,
                                    []}]};

                    false ->
                        {ok, List}
                end end
        )
    end.

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 241).
-spec all(
    list(fun((gleam@dynamic:dynamic_()) -> {ok, HTG} |
        {error, list(gleam@dynamic:decode_error())}))
) -> fun((gleam@dynamic:dynamic_()) -> {ok, list(HTG)} |
    {error, list(gleam@dynamic:decode_error())}).
all(Decoders) ->
    fun(Dynamic) ->
        gleam@list:fold_right(
            Decoders,
            {ok, []},
            fun(List, Decoder) -> case {List, Decoder(Dynamic)} of
                    {{ok, Xs}, {ok, X}} ->
                        {ok, [X | Xs]};

                    {{ok, _}, {error, E}} ->
                        {error, E};

                    {{error, E@1}, {ok, _}} ->
                        {error, E@1};

                    {{error, E@2}, {error, X@1}} ->
                        {error, lists:append(E@2, X@1)}
                end end
        )
    end.

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 221).
-spec arraylike(
    fun((gleam@dynamic:dynamic_()) -> {ok, HTC} |
        {error, list(gleam@dynamic:decode_error())})
) -> fun((gleam@dynamic:dynamic_()) -> {ok, list(HTC)} |
    {error, list(gleam@dynamic:decode_error())}).
arraylike(Decoder) ->
    fun(Dynamic) ->
        gleam@result:'try'(
            (gleam@dynamic:field(<<"length"/utf8>>, fun gleam@dynamic:int/1))(
                Dynamic
            ),
            fun(Length) ->
                (all(
                    begin
                        List = gleam@list:range(0, Length - 1),
                        gleam@list:map(
                            List,
                            fun(Index) ->
                                gleam@dynamic:field(
                                    gleam@int:to_string(Index),
                                    Decoder
                                )
                            end
                        )
                    end
                ))(Dynamic)
            end
        )
    end.

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 156).
-spec set(
    fun((gleam@dynamic:dynamic_()) -> {ok, HSQ} |
        {error, list(gleam@dynamic:decode_error())})
) -> fun((gleam@dynamic:dynamic_()) -> {ok, gleam@set:set(HSQ)} |
    {error, list(gleam@dynamic:decode_error())}).
set(Decoder) ->
    fun(Dynamic) -> _pipe = Dynamic,
        _pipe@1 = (gleam@dynamic:any(
            [gleam@dynamic:list(Decoder), arraylike(Decoder)]
        ))(_pipe),
        gleam@result:map(_pipe@1, fun gleam@set:from_list/1) end.

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 168).
-spec exact_set(
    fun((gleam@dynamic:dynamic_()) -> {ok, HSU} |
        {error, list(gleam@dynamic:decode_error())})
) -> fun((gleam@dynamic:dynamic_()) -> {ok, gleam@set:set(HSU)} |
    {error, list(gleam@dynamic:decode_error())}).
exact_set(Decoder) ->
    fun(Dynamic) ->
        gleam@result:'try'(
            (gleam@dynamic:any(
                [gleam@dynamic:list(Decoder), arraylike(Decoder)]
            ))(Dynamic),
            fun(List) ->
                Length = erlang:length(List),
                Set = gleam@set:from_list(List),
                case gleam@set:size(Set) =:= Length of
                    true ->
                        {ok, Set};

                    false ->
                        {error,
                            [{decode_error,
                                    <<"A list with no duplicate values"/utf8>>,
                                    <<"A list with duplicate values"/utf8>>,
                                    []}]}
                end
            end
        )
    end.

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 468).
-spec tagged_union(
    fun((gleam@dynamic:dynamic_()) -> {ok, HVS} |
        {error, list(gleam@dynamic:decode_error())}),
    list({HVS,
        fun((gleam@dynamic:dynamic_()) -> {ok, HVU} |
            {error, list(gleam@dynamic:decode_error())})})
) -> fun((gleam@dynamic:dynamic_()) -> {ok, HVU} |
    {error, list(gleam@dynamic:decode_error())}).
tagged_union(Tag, Variants) ->
    Switch = maps:from_list(Variants),
    fun(Dynamic) ->
        gleam@result:'try'(
            Tag(Dynamic),
            fun(Kind) -> case gleam@dict:get(Switch, Kind) of
                    {ok, Decoder} ->
                        Decoder(Dynamic);

                    {error, _} ->
                        Tags = begin
                            _pipe = gleam@dict:keys(Switch),
                            _pipe@1 = gleam@list:map(
                                _pipe,
                                fun gleam@string:inspect/1
                            ),
                            gleam@string:join(_pipe@1, <<" | "/utf8>>)
                        end,
                        Path@1 = case Tag(gleam@dynamic:from(nil)) of
                            {error, [{decode_error, _, _, Path} | _]} ->
                                Path;

                            _ ->
                                []
                        end,
                        {error,
                            [{decode_error,
                                    Tags,
                                    gleam@string:inspect(Kind),
                                    Path@1}]}
                end end
        )
    end.

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 535).
-spec enum(list({binary(), HVY})) -> fun((gleam@dynamic:dynamic_()) -> {ok, HVY} |
    {error, list(gleam@dynamic:decode_error())}).
enum(Variants) ->
    tagged_union(
        fun gleam@dynamic:string/1,
        gleam@list:map(
            Variants,
            fun(_capture) ->
                gleam@pair:map_second(
                    _capture,
                    fun(Variant) -> fun(_) -> {ok, Variant} end end
                )
            end
        )
    ).

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 115).
-spec bool_string(gleam@dynamic:dynamic_()) -> {ok, boolean()} |
    {error, list(gleam@dynamic:decode_error())}.
bool_string(Dynamic) ->
    (enum(
        [{<<"true"/utf8>>, true},
            {<<"True"/utf8>>, true},
            {<<"on"/utf8>>, true},
            {<<"On"/utf8>>, true},
            {<<"yes"/utf8>>, true},
            {<<"Yes"/utf8>>, true},
            {<<"false"/utf8>>, false},
            {<<"False"/utf8>>, false},
            {<<"off"/utf8>>, false},
            {<<"Off"/utf8>>, false},
            {<<"no"/utf8>>, false},
            {<<"No"/utf8>>, false}]
    ))(Dynamic).

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 548).
-spec iso_8601(gleam@dynamic:dynamic_()) -> {ok, birl:time()} |
    {error, list(gleam@dynamic:decode_error())}.
iso_8601(Dynamic) ->
    gleam@result:'try'(
        gleam@dynamic:string(Dynamic),
        fun(String) -> case birl:parse(String) of
                {ok, Time} ->
                    {ok, Time};

                {error, _} ->
                    {error,
                        [{decode_error,
                                <<"An ISO 8601 date string"/utf8>>,
                                String,
                                []}]}
            end end
    ).

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 567).
-spec unix_timestamp(gleam@dynamic:dynamic_()) -> {ok, birl:time()} |
    {error, list(gleam@dynamic:decode_error())}.
unix_timestamp(Dynamic) ->
    _pipe = Dynamic,
    _pipe@1 = (gleam@dynamic:any([fun gleam@dynamic:int/1, fun int_string/1]))(
        _pipe
    ),
    gleam@result:map(_pipe@1, fun birl:from_unix/1).

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 577).
-spec http_date(gleam@dynamic:dynamic_()) -> {ok, birl:time()} |
    {error, list(gleam@dynamic:decode_error())}.
http_date(Dynamic) ->
    gleam@result:'try'(
        gleam@dynamic:string(Dynamic),
        fun(String) -> case birl:from_http(String) of
                {ok, Time} ->
                    {ok, Time};

                {error, _} ->
                    {error,
                        [{decode_error,
                                <<"An HTTP date string"/utf8>>,
                                String,
                                []}]}
            end end
    ).

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 592).
-spec uri(gleam@dynamic:dynamic_()) -> {ok, gleam@uri:uri()} |
    {error, list(gleam@dynamic:decode_error())}.
uri(Dynamic) ->
    gleam@result:'try'(
        gleam@dynamic:string(Dynamic),
        fun(String) -> case gleam@uri:parse(String) of
                {ok, Uri} ->
                    {ok, Uri};

                {error, _} ->
                    {error,
                        [{decode_error,
                                <<"A valid Gleam URI"/utf8>>,
                                String,
                                []}]}
            end end
    ).

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 606).
-spec base16(gleam@dynamic:dynamic_()) -> {ok, bitstring()} |
    {error, list(gleam@dynamic:decode_error())}.
base16(Dynamic) ->
    gleam@result:'try'(
        gleam@dynamic:string(Dynamic),
        fun(String) -> case gleam_stdlib:base16_decode(String) of
                {ok, Bit_array} ->
                    {ok, Bit_array};

                {error, _} ->
                    {error,
                        [{decode_error,
                                <<"A valid base16-encoded string"/utf8>>,
                                String,
                                []}]}
            end end
    ).

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 624).
-spec base64(gleam@dynamic:dynamic_()) -> {ok, bitstring()} |
    {error, list(gleam@dynamic:decode_error())}.
base64(Dynamic) ->
    gleam@result:'try'(
        gleam@dynamic:string(Dynamic),
        fun(String) -> case gleam@bit_array:base64_decode(String) of
                {ok, Bit_array} ->
                    {ok, Bit_array};

                {error, _} ->
                    {error,
                        [{decode_error,
                                <<"A valid base64-encoded string"/utf8>>,
                                String,
                                []}]}
            end end
    ).

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 643).
-spec semver(gleam@dynamic:dynamic_()) -> {ok, stoiridh@version:version()} |
    {error, list(gleam@dynamic:decode_error())}.
semver(Dynamic) ->
    gleam@result:'try'(
        gleam@dynamic:string(Dynamic),
        fun(String) -> case stoiridh@version:parse(String) of
                {ok, Version} ->
                    {ok, Version};

                {error, invalid_version} ->
                    {error,
                        [{decode_error,
                                <<"A valid semantic version"/utf8>>,
                                String,
                                []}]};

                {error, {negative_value, _}} ->
                    {error,
                        [{decode_error,
                                <<"A valid semantic version"/utf8>>,
                                String,
                                []}]};

                {error, invalid_major_version} ->
                    {error,
                        [{decode_error,
                                <<"A valid semantic version"/utf8>>,
                                <<"A semantic version with an invalid major version number"/utf8>>,
                                []}]};

                {error, invalid_minor_version} ->
                    {error,
                        [{decode_error,
                                <<"A valid semantic version"/utf8>>,
                                <<"A semantic version with an invalid minor version number"/utf8>>,
                                []}]};

                {error, invalid_patch_version} ->
                    {error,
                        [{decode_error,
                                <<"A valid semantic version"/utf8>>,
                                <<"A semantic version with an invalid patch version number"/utf8>>,
                                []}]};

                {error, invalid_prerelease} ->
                    {error,
                        [{decode_error,
                                <<"A valid semantic version"/utf8>>,
                                <<"A semantic version with an invalid prerelease"/utf8>>,
                                []}]};

                {error, invalid_build_metadata} ->
                    {error,
                        [{decode_error,
                                <<"A valid semantic version"/utf8>>,
                                <<"A semantic version with invalid build metadata"/utf8>>,
                                []}]}
            end end
    ).

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 702).
-spec base64_url_encoded(gleam@dynamic:dynamic_()) -> {ok, bitstring()} |
    {error, list(gleam@dynamic:decode_error())}.
base64_url_encoded(Dynamic) ->
    gleam@result:'try'(
        gleam@dynamic:string(Dynamic),
        fun(String) -> case gleam@bit_array:base64_url_decode(String) of
                {ok, Bit_array} ->
                    {ok, Bit_array};

                {error, _} ->
                    {error,
                        [{decode_error,
                                <<"A valid base64-url-encoded string"/utf8>>,
                                String,
                                []}]}
            end end
    ).

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 735).
-spec 'when'(
    fun((gleam@dynamic:dynamic_()) -> {ok, HWZ} |
        {error, list(gleam@dynamic:decode_error())}),
    fun((HWZ) -> boolean())
) -> fun((gleam@dynamic:dynamic_()) -> {ok, HWZ} |
    {error, list(gleam@dynamic:decode_error())}).
'when'(Decoder, Predicate) ->
    fun(Dynamic) ->
        gleam@result:'try'(
            Decoder(Dynamic),
            fun(Value) -> case Predicate(Value) of
                    true ->
                        {ok, Value};

                    false ->
                        {error,
                            [{decode_error,
                                    <<"A value that satisfies the predicate"/utf8>>,
                                    gleam@string:inspect(Value),
                                    []}]}
                end end
        )
    end.

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 757).
-spec json_string(
    fun((gleam@dynamic:dynamic_()) -> {ok, HXC} |
        {error, list(gleam@dynamic:decode_error())})
) -> fun((gleam@dynamic:dynamic_()) -> {ok, HXC} |
    {error, list(gleam@dynamic:decode_error())}).
json_string(Decoder) ->
    fun(Dynamic) ->
        gleam@result:'try'(
            gleam@dynamic:string(Dynamic),
            fun(Json) -> case gleam@json:decode(Json, Decoder) of
                    {ok, A} ->
                        {ok, A};

                    {error, {unexpected_format, Errors}} ->
                        {error, Errors};

                    {error, _} ->
                        {error,
                            [{decode_error,
                                    <<"A valid JSON-encoded string"/utf8>>,
                                    Json,
                                    []}]}
                end end
        )
    end.

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 778).
-spec keys(gleam@dynamic:dynamic_()) -> {ok, list(binary())} |
    {error, list(gleam@dynamic:decode_error())}.
keys(Dynamic) ->
    _pipe = Dynamic,
    _pipe@1 = (gleam@dynamic:dict(
        fun gleam@dynamic:string/1,
        fun gleam@dynamic:dynamic/1
    ))(_pipe),
    gleam@result:map(_pipe@1, fun gleam@dict:keys/1).

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 405).
-spec check_exact_object(HVN, gleam@set:set(binary()), gleam@dynamic:dynamic_()) -> {ok,
        HVN} |
    {error, list(gleam@dynamic:decode_error())}.
check_exact_object(Return, Expected, Dynamic) ->
    gleam@result:'try'(
        keys(Dynamic),
        fun(Keys) ->
            Found_keys = gleam@set:from_list(Keys),
            Difference = gleam@set:to_list(
                gleam@set:difference(Found_keys, Expected)
            ),
            case gleam@list:is_empty(Difference) of
                true ->
                    {ok, Return};

                false ->
                    Expected_keys = begin
                        _pipe = Expected,
                        _pipe@1 = gleam@set:to_list(_pipe),
                        gleam@string:join(_pipe@1, <<", "/utf8>>)
                    end,
                    Extra_keys = begin
                        _pipe@2 = Difference,
                        gleam@string:join(_pipe@2, <<", "/utf8>>)
                    end,
                    {error,
                        [{decode_error,
                                <<"An object with exactly these keys: "/utf8,
                                    Expected_keys/binary>>,
                                <<"An object with these extra keys: "/utf8,
                                    Extra_keys/binary>>,
                                []}]}
            end
        end
    ).

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 260).
-spec exact_object1(
    fun((HTL) -> HTM),
    {binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, HTL} |
            {error, list(gleam@dynamic:decode_error())})}
) -> fun((gleam@dynamic:dynamic_()) -> {ok, HTM} |
    {error, list(gleam@dynamic:decode_error())}).
exact_object1(Constructor, Field1) ->
    Expected_keys = gleam@set:from_list([erlang:element(1, Field1)]),
    fun(Dynamic) -> _pipe = Dynamic,
        _pipe@1 = (gleam@dynamic:decode1(
            Constructor,
            gleam@dynamic:field(
                erlang:element(1, Field1),
                erlang:element(2, Field1)
            )
        ))(_pipe),
        gleam@result:then(
            _pipe@1,
            fun(_capture) ->
                check_exact_object(_capture, Expected_keys, Dynamic)
            end
        ) end.

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 277).
-spec exact_object2(
    fun((HTP, HTQ) -> HTR),
    {binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, HTP} |
            {error, list(gleam@dynamic:decode_error())})},
    {binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, HTQ} |
            {error, list(gleam@dynamic:decode_error())})}
) -> fun((gleam@dynamic:dynamic_()) -> {ok, HTR} |
    {error, list(gleam@dynamic:decode_error())}).
exact_object2(Constructor, Field1, Field2) ->
    Expected_keys = gleam@set:from_list([erlang:element(1, Field1)]),
    fun(Dynamic) -> _pipe = Dynamic,
        _pipe@1 = (gleam@dynamic:decode2(
            Constructor,
            gleam@dynamic:field(
                erlang:element(1, Field1),
                erlang:element(2, Field1)
            ),
            gleam@dynamic:field(
                erlang:element(1, Field2),
                erlang:element(2, Field2)
            )
        ))(_pipe),
        gleam@result:then(
            _pipe@1,
            fun(_capture) ->
                check_exact_object(_capture, Expected_keys, Dynamic)
            end
        ) end.

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 299).
-spec exact_object3(
    fun((HTV, HTW, HTX) -> HTY),
    {binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, HTV} |
            {error, list(gleam@dynamic:decode_error())})},
    {binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, HTW} |
            {error, list(gleam@dynamic:decode_error())})},
    {binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, HTX} |
            {error, list(gleam@dynamic:decode_error())})}
) -> fun((gleam@dynamic:dynamic_()) -> {ok, HTY} |
    {error, list(gleam@dynamic:decode_error())}).
exact_object3(Constructor, Field1, Field2, Field3) ->
    Expected_keys = gleam@set:from_list(
        [erlang:element(1, Field1),
            erlang:element(1, Field2),
            erlang:element(1, Field3)]
    ),
    fun(Dynamic) -> _pipe = Dynamic,
        _pipe@1 = (gleam@dynamic:decode3(
            Constructor,
            gleam@dynamic:field(
                erlang:element(1, Field1),
                erlang:element(2, Field1)
            ),
            gleam@dynamic:field(
                erlang:element(1, Field2),
                erlang:element(2, Field2)
            ),
            gleam@dynamic:field(
                erlang:element(1, Field3),
                erlang:element(2, Field3)
            )
        ))(_pipe),
        gleam@result:then(
            _pipe@1,
            fun(_capture) ->
                check_exact_object(_capture, Expected_keys, Dynamic)
            end
        ) end.

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 323).
-spec exact_object4(
    fun((HUD, HUE, HUF, HUG) -> HUH),
    {binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, HUD} |
            {error, list(gleam@dynamic:decode_error())})},
    {binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, HUE} |
            {error, list(gleam@dynamic:decode_error())})},
    {binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, HUF} |
            {error, list(gleam@dynamic:decode_error())})},
    {binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, HUG} |
            {error, list(gleam@dynamic:decode_error())})}
) -> fun((gleam@dynamic:dynamic_()) -> {ok, HUH} |
    {error, list(gleam@dynamic:decode_error())}).
exact_object4(Constructor, Field1, Field2, Field3, Field4) ->
    Expected_keys = gleam@set:from_list(
        [erlang:element(1, Field1),
            erlang:element(1, Field2),
            erlang:element(1, Field3),
            erlang:element(1, Field4)]
    ),
    fun(Dynamic) -> _pipe = Dynamic,
        _pipe@1 = (gleam@dynamic:decode4(
            Constructor,
            gleam@dynamic:field(
                erlang:element(1, Field1),
                erlang:element(2, Field1)
            ),
            gleam@dynamic:field(
                erlang:element(1, Field2),
                erlang:element(2, Field2)
            ),
            gleam@dynamic:field(
                erlang:element(1, Field3),
                erlang:element(2, Field3)
            ),
            gleam@dynamic:field(
                erlang:element(1, Field4),
                erlang:element(2, Field4)
            )
        ))(_pipe),
        gleam@result:then(
            _pipe@1,
            fun(_capture) ->
                check_exact_object(_capture, Expected_keys, Dynamic)
            end
        ) end.

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 349).
-spec exact_object5(
    fun((HUN, HUO, HUP, HUQ, HUR) -> HUS),
    {binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, HUN} |
            {error, list(gleam@dynamic:decode_error())})},
    {binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, HUO} |
            {error, list(gleam@dynamic:decode_error())})},
    {binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, HUP} |
            {error, list(gleam@dynamic:decode_error())})},
    {binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, HUQ} |
            {error, list(gleam@dynamic:decode_error())})},
    {binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, HUR} |
            {error, list(gleam@dynamic:decode_error())})}
) -> fun((gleam@dynamic:dynamic_()) -> {ok, HUS} |
    {error, list(gleam@dynamic:decode_error())}).
exact_object5(Constructor, Field1, Field2, Field3, Field4, Field5) ->
    Expected_keys = gleam@set:from_list(
        [erlang:element(1, Field1),
            erlang:element(1, Field2),
            erlang:element(1, Field3),
            erlang:element(1, Field4),
            erlang:element(1, Field5)]
    ),
    fun(Dynamic) -> _pipe = Dynamic,
        _pipe@1 = (gleam@dynamic:decode5(
            Constructor,
            gleam@dynamic:field(
                erlang:element(1, Field1),
                erlang:element(2, Field1)
            ),
            gleam@dynamic:field(
                erlang:element(1, Field2),
                erlang:element(2, Field2)
            ),
            gleam@dynamic:field(
                erlang:element(1, Field3),
                erlang:element(2, Field3)
            ),
            gleam@dynamic:field(
                erlang:element(1, Field4),
                erlang:element(2, Field4)
            ),
            gleam@dynamic:field(
                erlang:element(1, Field5),
                erlang:element(2, Field5)
            )
        ))(_pipe),
        gleam@result:then(
            _pipe@1,
            fun(_capture) ->
                check_exact_object(_capture, Expected_keys, Dynamic)
            end
        ) end.

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 378).
-spec exact_object6(
    fun((HUZ, HVA, HVB, HVC, HVD, HVE) -> HVF),
    {binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, HUZ} |
            {error, list(gleam@dynamic:decode_error())})},
    {binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, HVA} |
            {error, list(gleam@dynamic:decode_error())})},
    {binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, HVB} |
            {error, list(gleam@dynamic:decode_error())})},
    {binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, HVC} |
            {error, list(gleam@dynamic:decode_error())})},
    {binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, HVD} |
            {error, list(gleam@dynamic:decode_error())})},
    {binary(),
        fun((gleam@dynamic:dynamic_()) -> {ok, HVE} |
            {error, list(gleam@dynamic:decode_error())})}
) -> fun((gleam@dynamic:dynamic_()) -> {ok, HVF} |
    {error, list(gleam@dynamic:decode_error())}).
exact_object6(Constructor, Field1, Field2, Field3, Field4, Field5, Field6) ->
    Expected_keys = gleam@set:from_list(
        [erlang:element(1, Field1),
            erlang:element(1, Field2),
            erlang:element(1, Field3),
            erlang:element(1, Field4),
            erlang:element(1, Field5),
            erlang:element(1, Field6)]
    ),
    fun(Dynamic) -> _pipe = Dynamic,
        _pipe@1 = (gleam@dynamic:decode6(
            Constructor,
            gleam@dynamic:field(
                erlang:element(1, Field1),
                erlang:element(2, Field1)
            ),
            gleam@dynamic:field(
                erlang:element(1, Field2),
                erlang:element(2, Field2)
            ),
            gleam@dynamic:field(
                erlang:element(1, Field3),
                erlang:element(2, Field3)
            ),
            gleam@dynamic:field(
                erlang:element(1, Field4),
                erlang:element(2, Field4)
            ),
            gleam@dynamic:field(
                erlang:element(1, Field5),
                erlang:element(2, Field5)
            ),
            gleam@dynamic:field(
                erlang:element(1, Field6),
                erlang:element(2, Field6)
            )
        ))(_pipe),
        gleam@result:then(
            _pipe@1,
            fun(_capture) ->
                check_exact_object(_capture, Expected_keys, Dynamic)
            end
        ) end.

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 799).
-spec index_list(
    integer(),
    fun((gleam@dynamic:dynamic_()) -> {ok, HXM} |
        {error, list(gleam@dynamic:decode_error())})
) -> fun((gleam@dynamic:dynamic_()) -> {ok, HXM} |
    {error, list(gleam@dynamic:decode_error())}).
index_list(Idx, Decoder) ->
    fun(Dynamic) ->
        gleam@result:'try'(
            (gleam@dynamic:list(fun gleam@dynamic:dynamic/1))(Dynamic),
            fun(List) -> case Idx >= 0 of
                    true ->
                        _pipe = List,
                        _pipe@1 = gleam@list:drop(_pipe, Idx),
                        _pipe@2 = gleam@list:first(_pipe@1),
                        _pipe@3 = gleam@result:replace_error(
                            _pipe@2,
                            [{decode_error,
                                    <<<<"A list with at least"/utf8,
                                            (gleam@int:to_string(Idx + 1))/binary>>/binary,
                                        "elements"/utf8>>,
                                    <<<<"A list with"/utf8,
                                            (gleam@int:to_string(
                                                erlang:length(List)
                                            ))/binary>>/binary,
                                        "elements"/utf8>>,
                                    [gleam@int:to_string(Idx)]}]
                        ),
                        gleam@result:then(_pipe@3, Decoder);

                    false ->
                        {error,
                            [{decode_error,
                                    <<"An 'index' decoder with a non-negative index"/utf8>>,
                                    gleam@int:to_string(Idx),
                                    []}]}
                end end
        )
    end.

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 791).
-spec index(
    integer(),
    fun((gleam@dynamic:dynamic_()) -> {ok, HXJ} |
        {error, list(gleam@dynamic:decode_error())})
) -> fun((gleam@dynamic:dynamic_()) -> {ok, HXJ} |
    {error, list(gleam@dynamic:decode_error())}).
index(Idx, Decoder) ->
    gleam@dynamic:any(
        [gleam@dynamic:element(Idx, Decoder),
            gleam@dynamic:field(gleam@int:to_string(Idx), Decoder),
            index_list(Idx, Decoder)]
    ).

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 839).
-spec do_at(
    list(binary()),
    fun((gleam@dynamic:dynamic_()) -> {ok, HXU} |
        {error, list(gleam@dynamic:decode_error())}),
    gleam@dynamic:dynamic_()
) -> {ok, HXU} | {error, list(gleam@dynamic:decode_error())}.
do_at(Path, Decoder, Dynamic) ->
    case Path of
        [] ->
            Decoder(Dynamic);

        [Head | Rest] ->
            case gleam@int:parse(Head) of
                {ok, Idx} ->
                    _pipe = Dynamic,
                    _pipe@1 = (index(Idx, fun gleam@dynamic:dynamic/1))(_pipe),
                    gleam@result:then(
                        _pipe@1,
                        fun(_capture) -> do_at(Rest, Decoder, _capture) end
                    );

                {error, _} ->
                    _pipe@2 = Dynamic,
                    _pipe@3 = (gleam@dynamic:field(
                        Head,
                        fun gleam@dynamic:dynamic/1
                    ))(_pipe@2),
                    gleam@result:then(
                        _pipe@3,
                        fun(_capture@1) -> do_at(Rest, Decoder, _capture@1) end
                    )
            end
    end.

-file("/Users/hayleigh/dev/decipher/src/decipher.gleam", 835).
-spec at(
    list(binary()),
    fun((gleam@dynamic:dynamic_()) -> {ok, HXQ} |
        {error, list(gleam@dynamic:decode_error())})
) -> fun((gleam@dynamic:dynamic_()) -> {ok, HXQ} |
    {error, list(gleam@dynamic:decode_error())}).
at(Path, Decoder) ->
    fun(Dynamic) -> do_at(Path, Decoder, Dynamic) end.
