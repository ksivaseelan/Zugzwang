-module(on_terminate).

-export([run/0]).

run() ->
    binbo:start(),
    {ok, Pid} = binbo:new_server(),
    io:format(Pid),
    binbo:new_game(Pid),
    binbo:set_server_options(Pid, #{
        idle_timeout => 1000,
        onterminate => {fun onterminate_callback/4, "my argument"}
    }),
    % 'onterminate_callback/4' will be called after 1000 ms
    ok.

onterminate_callback(GamePid, Reason, Game, Arg) ->
    io:format("GamePid: ~p~n", [GamePid]),
    io:format("Reason: ~p~n", [Reason]),
    io:format("Game: ~p~n", [Game]),
    io:format("Arg: ~p~n", [Arg]),
    ok.