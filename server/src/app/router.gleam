import app/web
import game
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/http.{Get, Post}
import gleam/io
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string
import gleam/string_builder
import wisp.{type Request, type Response}
import wisp_kv_sessions
import wisp_kv_sessions/actor_store
import wisp_kv_sessions/session
import wisp_kv_sessions/session_config
import youid/uuid





fn encode(str: String) {
  json.string(str) |> json.to_string
}



// HTTP request handler
pub fn handle_request(req: Request, session_config) -> Response {
  // Apply the middleware stack for this request/response.
  use req <- web.middleware(req)
  use req <- wisp_kv_sessions.middleware(session_config, req)

  case wisp.path_segments(req), req.method {
    [], Get -> home_page(req)
    ["create", color], Post -> create_game_route(req, color, session_config)
    ["join", id], Post -> join_game(req, id, session_config)
    _, _ -> wisp.not_found()
  }
}

fn home_page(req: Request) -> Response {
  // The home page can only be accessed via GET requests, so this middleware is
  // used to return a 405: Method Not Allowed response for all other methods.
  use <- wisp.require_method(req, Get)

  let html = string_builder.from_string("Welcome to Zugzwang")
  wisp.ok()
  |> wisp.html_body(html)
}

fn create_game_route(req: Request, color: String, session_config) -> Response {
  let player_id = uuid.v4_string()

  let chosen_color = {
    case color {
      "white" -> White
      _ -> Black
    }
  }

  let initial_state =
    GameState(
      player1: None,
      player2: None,
      id: "",
      pid: process.self(),
      status: Initializing,
      turn: Turn(White),
    )

  let assert Ok(state) = actor.start(initial_state, game_actor_loop)
  io.debug("Initial state")
  io.debug(state)

  let assert Ok(game) =
    actor.call(state, CreateGame(_, chosen_color, player_id), 100)
  io.debug("Game created")
  io.debug(game)

  let _ =
    wisp_kv_sessions.set(
      session_config,
      req,
      player_id |> string.inspect,
      game |> string.inspect,
      encode,
    )

  let game_id = game.id

  let link = "http://localhost:6969/join/" <> game.id

  let object =
    json.object([
      #("game_id", game_id |> json.string),
      #("player1", player_id |> json.string),
      #("link", link |> json.string),
    ])

  let resp = json.to_string_builder(object)
  wisp.json_response(resp, 201)
}

fn join_game(_req: Request, _game_id: String, _session_config) -> Response {
  wisp.ok()
}
