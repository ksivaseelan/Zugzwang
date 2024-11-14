-record(version, {
    major :: integer(),
    minor :: integer(),
    patch :: integer(),
    prerelease :: gleam@option:option(binary()),
    build_metadata :: gleam@option:option(binary())
}).
