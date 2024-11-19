-module(learning_mist).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).
-export_type([state/0, my_message/0]).

-type state() :: {game_state, gleam@erlang@process:pid_()} | no_game.

-type my_message() :: {broadcast, binary()}.

-file("/home/kogul/projects/gleam/chess/server/src/learning_mist.gleam", 54).
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
                                line => 58})
            end,
            gleam@otp@actor:continue(State);

        {text, <<"Create"/utf8>>} ->
            _assert_subject@1 = gleam_binbo:play(),
            {ok, Pid} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@1,
                                module => <<"learning_mist"/utf8>>,
                                function => <<"handle_ws_message"/utf8>>,
                                line => 63})
            end,
            gleam@io:debug(
                <<"Created game with pid:"/utf8,
                    (begin
                        _pipe = Pid,
                        gleam@string:inspect(_pipe)
                    end)/binary>>
            ),
            binbo:print_board(Pid),
            _assert_subject@2 = mist:send_text_frame(
                Conn,
                <<"Game Success: Game created with "/utf8,
                    (begin
                        _pipe@1 = Pid,
                        gleam@string:inspect(_pipe@1)
                    end)/binary>>
            ),
            {ok, _} = case _assert_subject@2 of
                {ok, _} -> _assert_subject@2;
                _assert_fail@2 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@2,
                                module => <<"learning_mist"/utf8>>,
                                function => <<"handle_ws_message"/utf8>>,
                                line => 66})
            end,
            gleam@otp@actor:continue({game_state, Pid});

        {text, <<"Move: "/utf8, Move/binary>>} ->
            _ = case State of
                {game_state, Pid@1} ->
                    case binbo:move(Pid@1, Move) of
                        {ok, _} ->
                            binbo:print_board(Pid@1),
                            _assert_subject@3 = mist:send_text_frame(
                                Conn,
                                <<<<"Move Success: "/utf8, Move/binary>>/binary,
                                    " made"/utf8>>
                            ),
                            {ok, _} = case _assert_subject@3 of
                                {ok, _} -> _assert_subject@3;
                                _assert_fail@3 ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                value => _assert_fail@3,
                                                module => <<"learning_mist"/utf8>>,
                                                function => <<"handle_ws_message"/utf8>>,
                                                line => 80})
                            end;

                        _ ->
                            _assert_subject@4 = mist:send_text_frame(
                                Conn,
                                <<"Move Error: Not a legal move"/utf8>>
                            ),
                            {ok, _} = case _assert_subject@4 of
                                {ok, _} -> _assert_subject@4;
                                _assert_fail@4 ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                value => _assert_fail@4,
                                                module => <<"learning_mist"/utf8>>,
                                                function => <<"handle_ws_message"/utf8>>,
                                                line => 84})
                            end
                    end;

                _ ->
                    _assert_subject@5 = mist:send_text_frame(
                        Conn,
                        <<"Game Error: Game must be created first"/utf8>>
                    ),
                    {ok, _} = case _assert_subject@5 of
                        {ok, _} -> _assert_subject@5;
                        _assert_fail@5 ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        value => _assert_fail@5,
                                        module => <<"learning_mist"/utf8>>,
                                        function => <<"handle_ws_message"/utf8>>,
                                        line => 90})
                    end
            end,
            gleam@otp@actor:continue(State);

        {text, _} ->
            _assert_subject@6 = mist:send_text_frame(
                Conn,
                <<"Unknown Message type"/utf8>>
            ),
            {ok, _} = case _assert_subject@6 of
                {ok, _} -> _assert_subject@6;
                _assert_fail@6 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@6,
                                module => <<"learning_mist"/utf8>>,
                                function => <<"handle_ws_message"/utf8>>,
                                line => 97})
            end,
            gleam@otp@actor:continue(State);

        {binary, _} ->
            _assert_subject@6 = mist:send_text_frame(
                Conn,
                <<"Unknown Message type"/utf8>>
            ),
            {ok, _} = case _assert_subject@6 of
                {ok, _} -> _assert_subject@6;
                _assert_fail@6 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@6,
                                module => <<"learning_mist"/utf8>>,
                                function => <<"handle_ws_message"/utf8>>,
                                line => 97})
            end,
            gleam@otp@actor:continue(State);

        {custom, {broadcast, Text}} ->
            _assert_subject@7 = mist:send_text_frame(Conn, Text),
            {ok, _} = case _assert_subject@7 of
                {ok, _} -> _assert_subject@7;
                _assert_fail@7 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@7,
                                module => <<"learning_mist"/utf8>>,
                                function => <<"handle_ws_message"/utf8>>,
                                line => 101})
            end,
            gleam@otp@actor:continue(State);

        closed ->
            {stop, normal};

        shutdown ->
            {stop, normal}
    end.

-file("/home/kogul/projects/gleam/chess/server/src/learning_mist.gleam", 21).
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
                        line => 30})
    end,
    gleam_erlang_ffi:sleep_forever().
