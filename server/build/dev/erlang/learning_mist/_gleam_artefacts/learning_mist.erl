-module(learning_mist).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).
-export_type([my_message/0, payload/0, client_message/0]).

-type my_message() :: {broadcast, binary()}.

-type payload() :: {move, binary()} | surrender.

-type client_message() :: {client_message, binary(), payload()}.

-file("/home/kogul/projects/gleam/chess/server/src/learning_mist.gleam", 114).
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

-file("/home/kogul/projects/gleam/chess/server/src/learning_mist.gleam", 58).
-spec handle_ws_message(
    AYV,
    mist@internal@websocket:websocket_connection(),
    mist:websocket_message(my_message())
) -> gleam@otp@actor:next(any(), AYV).
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
                                line => 62})
            end,
            gleam@otp@actor:continue(State);

        {text, Message@1} ->
            case client_message_from_json(Message@1) of
                {ok, {client_message, Method, Payload}} ->
                    gleam@io:println(
                        <<"Message received with method:"/utf8,
                            (begin
                                _pipe = Method,
                                gleam@string:inspect(_pipe)
                            end)/binary>>
                    ),
                    case Method of
                        <<"move"/utf8>> ->
                            case Payload of
                                {move, Move} ->
                                    gleam@io:println(
                                        <<"Move received:"/utf8,
                                            (begin
                                                _pipe@1 = Move,
                                                gleam@string:inspect(_pipe@1)
                                            end)/binary>>
                                    );

                                _ ->
                                    gleam@io:println(
                                        <<"Unknown payload:"/utf8,
                                            (begin
                                                _pipe@2 = Payload,
                                                gleam@string:inspect(_pipe@2)
                                            end)/binary>>
                                    )
                            end;

                        _ ->
                            gleam@io:println(
                                <<"Unknown method:"/utf8,
                                    (begin
                                        _pipe@3 = Method,
                                        gleam@string:inspect(_pipe@3)
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
            _assert_subject@1 = mist:send_text_frame(Conn, Text),
            {ok, _} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@1,
                                module => <<"learning_mist"/utf8>>,
                                function => <<"handle_ws_message"/utf8>>,
                                line => 100})
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
