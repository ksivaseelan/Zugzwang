-module(learning_mist).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).
-export_type([state/0, my_message/0, payload/0, client_message/0]).

-type state() :: {game_state, gleam@erlang@process:pid_()} | no_game.

-type my_message() :: {broadcast, binary()}.

-type payload() :: {move, binary()} | surrender.

-type client_message() :: {client_message, binary(), payload()}.

-file("/home/kogul/projects/gleam/chess/server/src/learning_mist.gleam", 152).
-spec client_message_from_json(binary()) -> {ok, client_message()} |
    {error, gleam@json:decode_error()}.
client_message_from_json(Json_string) ->
    Payload_decoder = gleam@dynamic:decode1(
        fun(Field@0) -> {move, Field@0} end,
        gleam@dynamic:field(<<"move"/utf8>>, fun gleam@dynamic:string/1)
    ),
    Client_message_decoder = gleam@dynamic:decode2(
        fun(Field@0, Field@1) -> {client_message, Field@0, Field@1} end,
        gleam@dynamic:field(<<"method"/utf8>>, fun gleam@dynamic:string/1),
        gleam@dynamic:field(<<"payload"/utf8>>, Payload_decoder)
    ),
    gleam@json:decode(Json_string, Client_message_decoder).

-file("/home/kogul/projects/gleam/chess/server/src/learning_mist.gleam", 65).
-spec handle_ws_message(
    state(),
    mist@internal@websocket:websocket_connection(),
    mist:websocket_message(my_message())
) -> gleam@otp@actor:next(any(), state()).
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
                                line => 69})
            end,
            gleam@otp@actor:continue(State);

        {text, <<"Create"/utf8>>} ->
            gleam@io:println(<<"Creating game"/utf8>>),
            _assert_subject@1 = gleam_binbo:play(),
            {ok, Pid} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@1,
                                module => <<"learning_mist"/utf8>>,
                                function => <<"handle_ws_message"/utf8>>,
                                line => 75})
            end,
            binbo:print_board(Pid),
            _assert_subject@2 = mist:send_text_frame(
                Conn,
                begin
                    _pipe = Pid,
                    gleam@string:inspect(_pipe)
                end
            ),
            {ok, _} = case _assert_subject@2 of
                {ok, _} -> _assert_subject@2;
                _assert_fail@2 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@2,
                                module => <<"learning_mist"/utf8>>,
                                function => <<"handle_ws_message"/utf8>>,
                                line => 77})
            end,
            gleam@otp@actor:continue({game_state, Pid});

        {text, <<"Make Move: "/utf8, Move/binary>>} ->
            _ = case State of
                {game_state, Pid@1} ->
                    gleam@io:println(<<"Making move"/utf8, Move/binary>>),
                    case binbo:move(Pid@1, Move) of
                        {ok, _} ->
                            binbo:print_board(Pid@1),
                            _assert_subject@3 = mist:send_text_frame(
                                Conn,
                                <<"Move made: "/utf8, Move/binary>>
                            ),
                            {ok, _} = case _assert_subject@3 of
                                {ok, _} -> _assert_subject@3;
                                _assert_fail@3 ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                value => _assert_fail@3,
                                                module => <<"learning_mist"/utf8>>,
                                                function => <<"handle_ws_message"/utf8>>,
                                                line => 88})
                            end;

                        _ ->
                            _assert_subject@4 = mist:send_text_frame(
                                Conn,
                                <<"Invalid move "/utf8>>
                            ),
                            {ok, _} = case _assert_subject@4 of
                                {ok, _} -> _assert_subject@4;
                                _assert_fail@4 ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                value => _assert_fail@4,
                                                module => <<"learning_mist"/utf8>>,
                                                function => <<"handle_ws_message"/utf8>>,
                                                line => 92})
                            end
                    end;

                _ ->
                    _assert_subject@5 = mist:send_text_frame(
                        Conn,
                        <<"Invalid: Game must be created first"/utf8>>
                    ),
                    {ok, _} = case _assert_subject@5 of
                        {ok, _} -> _assert_subject@5;
                        _assert_fail@5 ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        value => _assert_fail@5,
                                        module => <<"learning_mist"/utf8>>,
                                        function => <<"handle_ws_message"/utf8>>,
                                        line => 97})
                    end
            end,
            gleam@otp@actor:continue(State);

        {text, Message@1} ->
            case client_message_from_json(Message@1) of
                {ok, {client_message, Method, Payload}} ->
                    gleam@io:println(
                        <<"Message received with method:"/utf8,
                            (begin
                                _pipe@1 = Method,
                                gleam@string:inspect(_pipe@1)
                            end)/binary>>
                    ),
                    case Method of
                        <<"move"/utf8>> ->
                            case Payload of
                                {move, Move@1} ->
                                    gleam@io:println(
                                        <<"Move received:"/utf8,
                                            (begin
                                                _pipe@2 = Move@1,
                                                gleam@string:inspect(_pipe@2)
                                            end)/binary>>
                                    );

                                _ ->
                                    gleam@io:println(
                                        <<"Unknown payload:"/utf8,
                                            (begin
                                                _pipe@3 = Payload,
                                                gleam@string:inspect(_pipe@3)
                                            end)/binary>>
                                    )
                            end;

                        _ ->
                            gleam@io:println(
                                <<"Unknown method:"/utf8,
                                    (begin
                                        _pipe@4 = Method,
                                        gleam@string:inspect(_pipe@4)
                                    end)/binary>>
                            )
                    end;

                _ ->
                    gleam@io:println(
                        <<"invalid message Format:"/utf8, Message@1/binary>>
                    )
            end,
            gleam@otp@actor:continue(State);

        {text, _} ->
            gleam@otp@actor:continue(State);

        {binary, _} ->
            gleam@otp@actor:continue(State);

        {custom, {broadcast, Text}} ->
            _assert_subject@6 = mist:send_text_frame(Conn, Text),
            {ok, _} = case _assert_subject@6 of
                {ok, _} -> _assert_subject@6;
                _assert_fail@6 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@6,
                                module => <<"learning_mist"/utf8>>,
                                function => <<"handle_ws_message"/utf8>>,
                                line => 138})
            end,
            gleam@otp@actor:continue(State);

        closed ->
            {stop, normal};

        shutdown ->
            {stop, normal}
    end.

-file("/home/kogul/projects/gleam/chess/server/src/learning_mist.gleam", 22).
-spec main() -> nil.
main() ->
    Selector = gleam_erlang_ffi:new_selector(),
    State = no_game,
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
                        line => 31})
    end,
    gleam_erlang_ffi:sleep_forever().
