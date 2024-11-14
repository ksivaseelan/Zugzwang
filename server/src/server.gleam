import app/router
import app/web
import gleam/erlang/process
import gleam/option
import gleam/result
import mist
import wisp
import wisp/wisp_mist

import wisp_kv_sessions
import wisp_kv_sessions/actor_store
import wisp_kv_sessions/session
import wisp_kv_sessions/session_config

pub fn main() {
  // This sets the logger to print INFO level logs, and other sensible defaults
  // for a web application.
  wisp.configure_logger()

  // Here we generate a secret key, but in a real application you would want to
  // load this from somewhere so that it is not regenerated on every restart.
  let secret_key_base = wisp.random_string(64)

  // Setup session_store
  use actor_store <- result.map(actor_store.try_create_session_store())
  use cache_store <- result.map(actor_store.try_create_session_store())
  // Create the session configuration.
  let session_config =
    session_config.Config(
      default_expiry: session.ExpireIn(60 * 60),
      cookie_name: "SESSION_COOKIE",
      store: actor_store,
      cache: option.Some(cache_store),
    )

  // The handle_request function is partially applied with the context to make
  // the request handler function that only takes a request.
  let handler = router.handle_request(_, session_config)

  // Start the Mist web server.
  let assert Ok(_) =
    handler
    |> wisp_mist.handler(secret_key_base)
    |> mist.new
    |> mist.port(6969)
    |> mist.start_http

  // The web server runs in new Erlang process, so put this one to sleep while
  // it works concurrently.
  process.sleep_forever()
}
