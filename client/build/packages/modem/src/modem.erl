-module(modem).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([initial_uri/0, init/1, advanced/2, load/1, forward/1, back/1, push/3, replace/3]).
-export_type([options/0]).

-type options() :: {options, boolean(), boolean()}.

-spec initial_uri() -> {ok, gleam@uri:uri()} | {error, nil}.
initial_uri() ->
    {error, nil}.

-spec do_init(fun((gleam@uri:uri()) -> nil)) -> nil.
do_init(_) ->
    nil.

-spec init(fun((gleam@uri:uri()) -> MGS)) -> lustre@effect:effect(MGS).
init(Handler) ->
    lustre@effect:from(
        fun(Dispatch) ->
            gleam@bool:guard(
                not lustre:is_browser(),
                nil,
                fun() -> do_init(fun(Uri) -> _pipe = Uri,
                            _pipe@1 = Handler(_pipe),
                            Dispatch(_pipe@1) end) end
            )
        end
    ).

-spec do_advanced(fun((gleam@uri:uri()) -> nil), options()) -> nil.
do_advanced(_, _) ->
    nil.

-spec advanced(options(), fun((gleam@uri:uri()) -> MGU)) -> lustre@effect:effect(MGU).
advanced(Options, Handler) ->
    lustre@effect:from(
        fun(Dispatch) ->
            gleam@bool:guard(
                not lustre:is_browser(),
                nil,
                fun() -> do_advanced(fun(Uri) -> _pipe = Uri,
                            _pipe@1 = Handler(_pipe),
                            Dispatch(_pipe@1) end, Options) end
            )
        end
    ).

-spec do_push(gleam@uri:uri()) -> nil.
do_push(_) ->
    nil.

-spec do_replace(gleam@uri:uri()) -> nil.
do_replace(_) ->
    nil.

-spec do_load(gleam@uri:uri()) -> nil.
do_load(_) ->
    nil.

-spec load(gleam@uri:uri()) -> lustre@effect:effect(any()).
load(Uri) ->
    lustre@effect:from(
        fun(_) ->
            gleam@bool:guard(
                not lustre:is_browser(),
                nil,
                fun() -> do_load(Uri) end
            )
        end
    ).

-spec do_forward(integer()) -> nil.
do_forward(_) ->
    nil.

-spec forward(integer()) -> lustre@effect:effect(any()).
forward(Steps) ->
    lustre@effect:from(
        fun(_) ->
            gleam@bool:guard(
                not lustre:is_browser(),
                nil,
                fun() -> do_forward(Steps) end
            )
        end
    ).

-spec do_back(integer()) -> nil.
do_back(_) ->
    nil.

-spec back(integer()) -> lustre@effect:effect(any()).
back(Steps) ->
    lustre@effect:from(
        fun(_) ->
            gleam@bool:guard(
                not lustre:is_browser(),
                nil,
                fun() -> do_back(Steps) end
            )
        end
    ).

-spec push(
    binary(),
    gleam@option:option(binary()),
    gleam@option:option(binary())
) -> lustre@effect:effect(any()).
push(Path, Query, Fragment) ->
    lustre@effect:from(
        fun(_) ->
            gleam@bool:guard(
                not lustre:is_browser(),
                nil,
                fun() ->
                    do_push(
                        erlang:setelement(
                            8,
                            erlang:setelement(
                                7,
                                erlang:setelement(
                                    6,
                                    {uri,
                                        none,
                                        none,
                                        none,
                                        none,
                                        <<""/utf8>>,
                                        none,
                                        none},
                                    Path
                                ),
                                Query
                            ),
                            Fragment
                        )
                    )
                end
            )
        end
    ).

-spec replace(
    binary(),
    gleam@option:option(binary()),
    gleam@option:option(binary())
) -> lustre@effect:effect(any()).
replace(Path, Query, Fragment) ->
    lustre@effect:from(
        fun(_) ->
            gleam@bool:guard(
                not lustre:is_browser(),
                nil,
                fun() ->
                    do_replace(
                        erlang:setelement(
                            8,
                            erlang:setelement(
                                7,
                                erlang:setelement(
                                    6,
                                    {uri,
                                        none,
                                        none,
                                        none,
                                        none,
                                        <<""/utf8>>,
                                        none,
                                        none},
                                    Path
                                ),
                                Query
                            ),
                            Fragment
                        )
                    )
                end
            )
        end
    ).
