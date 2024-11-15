-module(learning_mist).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).
-export_type([my_message/0, move/0, square/0]).

-type my_message() :: {broadcast, binary()}.

-type move() :: {move, binary(), square(), square()}.

-type square() :: {square, integer(), integer()}.

-file("/home/kogul/projects/gleam/chess/server/src/learning_mist.gleam", 85).
-spec echo_body(gleam@http@request:request(mist@internal@http:connection())) -> gleam@http@response:response(mist:response_data()).
echo_body(Request) ->
    Content_type = begin
        _pipe = Request,
        _pipe@1 = gleam@http@request:get_header(_pipe, <<"content-type"/utf8>>),
        gleam@result:unwrap(_pipe@1, <<"text/plain"/utf8>>)
    end,
    _pipe@2 = mist:read_body(Request, (1024 * 1024) * 10),
    _pipe@5 = gleam@result:map(
        _pipe@2,
        fun(Req) -> _pipe@3 = gleam@http@response:new(200),
            _pipe@4 = gleam@http@response:set_body(
                _pipe@3,
                {bytes, gleam_stdlib:wrap_list(erlang:element(4, Req))}
            ),
            gleam@http@response:set_header(
                _pipe@4,
                <<"content-type"/utf8>>,
                Content_type
            ) end
    ),
    gleam@result:lazy_unwrap(
        _pipe@5,
        fun() -> _pipe@6 = gleam@http@response:new(400),
            gleam@http@response:set_body(
                _pipe@6,
                {bytes, gleam@bytes_builder:new()}
            ) end
    ).

-file("/home/kogul/projects/gleam/chess/server/src/learning_mist.gleam", 103).
-spec serve_chunk(gleam@http@request:request(mist@internal@http:connection())) -> gleam@http@response:response(mist:response_data()).
serve_chunk(_) ->
    Iter = begin
        _pipe = [<<"one"/utf8>>, <<"two"/utf8>>, <<"three"/utf8>>],
        _pipe@1 = gleam@iterator:from_list(_pipe),
        gleam@iterator:map(_pipe@1, fun gleam_stdlib:wrap_list/1)
    end,
    _pipe@2 = gleam@http@response:new(200),
    _pipe@3 = gleam@http@response:set_body(_pipe@2, {chunked, Iter}),
    gleam@http@response:set_header(
        _pipe@3,
        <<"content-type"/utf8>>,
        <<"text/plain"/utf8>>
    ).

-file("/home/kogul/projects/gleam/chess/server/src/learning_mist.gleam", 134).
-spec handle_form(gleam@http@request:request(mist@internal@http:connection())) -> gleam@http@response:response(mist:response_data()).
handle_form(Req) ->
    _ = mist:read_body(Req, (1024 * 1024) * 30),
    _pipe = gleam@http@response:new(200),
    gleam@http@response:set_body(_pipe, {bytes, gleam@bytes_builder:new()}).

-file("/home/kogul/projects/gleam/chess/server/src/learning_mist.gleam", 140).
-spec guess_content_type(binary()) -> binary().
guess_content_type(_) ->
    <<"application/octet-stream"/utf8>>.

-file("/home/kogul/projects/gleam/chess/server/src/learning_mist.gleam", 114).
-spec serve_file(
    gleam@http@request:request(mist@internal@http:connection()),
    list(binary())
) -> gleam@http@response:response(mist:response_data()).
serve_file(_, Path) ->
    File_path = gleam@string:join(Path, <<"/"/utf8>>),
    _pipe = mist:send_file(File_path, 0, none),
    _pipe@3 = gleam@result:map(
        _pipe,
        fun(File) ->
            Content_type = guess_content_type(File_path),
            _pipe@1 = gleam@http@response:new(200),
            _pipe@2 = gleam@http@response:prepend_header(
                _pipe@1,
                <<"content-type"/utf8>>,
                Content_type
            ),
            gleam@http@response:set_body(_pipe@2, File)
        end
    ),
    gleam@result:lazy_unwrap(
        _pipe@3,
        fun() -> _pipe@4 = gleam@http@response:new(404),
            gleam@http@response:set_body(
                _pipe@4,
                {bytes, gleam@bytes_builder:new()}
            ) end
    ).

-file("/home/kogul/projects/gleam/chess/server/src/learning_mist.gleam", 153).
-spec move_from_json(binary()) -> {ok, move()} |
    {error, gleam@json:decode_error()}.
move_from_json(Json_string) ->
    Move_decoder = gleam@dynamic:decode3(
        fun(Field@0, Field@1, Field@2) -> {move, Field@0, Field@1, Field@2} end,
        gleam@dynamic:field(<<"method"/utf8>>, fun gleam@dynamic:string/1),
        gleam@dynamic:field(
            <<"from"/utf8>>,
            gleam@dynamic:decode2(
                fun(Field@0, Field@1) -> {square, Field@0, Field@1} end,
                gleam@dynamic:field(<<"x"/utf8>>, fun gleam@dynamic:int/1),
                gleam@dynamic:field(<<"y"/utf8>>, fun gleam@dynamic:int/1)
            )
        ),
        gleam@dynamic:field(
            <<"to"/utf8>>,
            gleam@dynamic:decode2(
                fun(Field@0, Field@1) -> {square, Field@0, Field@1} end,
                gleam@dynamic:field(<<"x"/utf8>>, fun gleam@dynamic:int/1),
                gleam@dynamic:field(<<"y"/utf8>>, fun gleam@dynamic:int/1)
            )
        )
    ),
    gleam@json:decode(Json_string, Move_decoder).

-file("/home/kogul/projects/gleam/chess/server/src/learning_mist.gleam", 53).
-spec handle_ws_message(
    BCM,
    mist@internal@websocket:websocket_connection(),
    mist:websocket_message(my_message())
) -> gleam@otp@actor:next(any(), BCM).
handle_ws_message(State, Conn, Message) ->
    case Message of
        {text, <<"ping"/utf8>>} ->
            gleam@io:println(<<"ping received"/utf8>>),
            _assert_subject = mist:send_text_frame(Conn, <<"pong"/utf8>>),
            {ok, _} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"learning_mist"/utf8>>,
                                function => <<"handle_ws_message"/utf8>>,
                                line => 57})
            end,
            gleam@otp@actor:continue(State);

        {text, Message@1} ->
            case move_from_json(Message@1) of
                {ok, Move} ->
                    gleam@io:println(
                        <<"move received:"/utf8,
                            (begin
                                _pipe = Move,
                                gleam@string:inspect(_pipe)
                            end)/binary>>
                    );

                _ ->
                    gleam@io:println(
                        <<"invalid move received:"/utf8, Message@1/binary>>
                    )
            end,
            _assert_subject@1 = mist:send_text_frame(
                Conn,
                <<"invalid move"/utf8>>
            ),
            {ok, _} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@1,
                                module => <<"learning_mist"/utf8>>,
                                function => <<"handle_ws_message"/utf8>>,
                                line => 71})
            end,
            gleam@otp@actor:continue(State);

        {text, _} ->
            gleam@otp@actor:continue(State);

        {binary, _} ->
            gleam@otp@actor:continue(State);

        {custom, {broadcast, Text}} ->
            _assert_subject@2 = mist:send_text_frame(Conn, Text),
            {ok, _} = case _assert_subject@2 of
                {ok, _} -> _assert_subject@2;
                _assert_fail@2 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@2,
                                module => <<"learning_mist"/utf8>>,
                                function => <<"handle_ws_message"/utf8>>,
                                line => 78})
            end,
            gleam@otp@actor:continue(State);

        closed ->
            {stop, normal};

        shutdown ->
            {stop, normal}
    end.

-file("/home/kogul/projects/gleam/chess/server/src/learning_mist.gleam", 15).
-spec main() -> nil.
main() ->
    Selector = gleam_erlang_ffi:new_selector(),
    State = nil,
    Not_found = begin
        _pipe = gleam@http@response:new(404),
        gleam@http@response:set_body(_pipe, {bytes, gleam@bytes_builder:new()})
    end,
    _assert_subject = begin
        _pipe@1 = fun(Req) -> case gleam@http@request:path_segments(Req) of
                [<<"ws"/utf8>>] ->
                    mist:websocket(
                        Req,
                        fun handle_ws_message/3,
                        fun(_) -> {State, {some, Selector}} end,
                        fun(_) -> gleam@io:println(<<"goodbye!"/utf8>>) end
                    );

                [<<"echo"/utf8>>] ->
                    echo_body(Req);

                [<<"chunk"/utf8>>] ->
                    serve_chunk(Req);

                [<<"file"/utf8>> | Rest] ->
                    serve_file(Req, Rest);

                [<<"form"/utf8>>] ->
                    handle_form(Req);

                _ ->
                    Not_found
            end end,
        _pipe@2 = mist:new(_pipe@1),
        _pipe@3 = mist:port(_pipe@2, 6969),
        mist:start_http(_pipe@3)
    end,
    {ok, _} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"learning_mist"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 24})
    end,
    gleam_erlang_ffi:sleep_forever().
