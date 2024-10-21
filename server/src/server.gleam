import app/router
import app/web
import gleam/erlang/process
import mist
import wisp
import wisp/wisp_mist
import bravo
import bravo/uset

pub fn main() {
  // This sets the logger to print INFO level logs, and other sensible defaults
  // for a web application.
  wisp.configure_logger()

  // Here we generate a secret key, but in a real application you would want to
  // load this from somewhere so that it is not regenerated on every restart.
  let secret_key_base = wisp.random_string(64)

  let assert Ok(room_table) = uset.new("rooms", 1, bravo.Public)

  //context will hold the room ets table
  let context = web.Context(room_table)

  // The handle_request function is partially applied with the context to make
  // the request handler function that only takes a request.
  let handler = router.handle_request(_, context)

  // Start the Mist web server.
  let assert Ok(_) =
    handler
    |> wisp_mist.handler( secret_key_base)
    |> mist.new
    |> mist.port(6969)
    |> mist.start_http

  // The web server runs in new Erlang process, so put this one to sleep while
  // it works concurrently.
  process.sleep_forever()
}