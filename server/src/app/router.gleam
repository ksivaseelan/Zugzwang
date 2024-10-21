import app/web.{type Context}
import bravo/uset
import game
import gleam/dynamic.{type Dynamic}
import gleam/http.{Get, Post}
import gleam/io
import gleam/json
import gleam/result
import gleam/string
import gleam/string_builder
import mist
import wisp.{type Request, type Response}
import youid/uuid

/// The HTTP request handler- your application!
/// 
pub fn handle_request(req: Request, ctx: Context) -> Response {
  // Apply the middleware stack for this request/response.
  use _req <- web.middleware(req)

  case wisp.path_segments(req) {
    [] -> home_page(req)
    ["create"] -> create_game(req, ctx)
    ["game", id] -> join_game(req, id, ctx)
    _ -> wisp.not_found()
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

fn create_game(req: Request, ctx: Context) -> Response {
  case req.method {
    Post -> create_game_post(req, ctx)
    _ -> wisp.method_not_allowed([Post])
  }
}

pub type GameCreateRequest {
  GameCreateRequest(color: String)
}

fn decode_game_create_request(
  json: Dynamic,
) -> Result(GameCreateRequest, dynamic.DecodeErrors) {
  let decoder =
    dynamic.decode1(GameCreateRequest, dynamic.field("color", dynamic.string))
  decoder(json)
}

fn create_game_post(req: Request, ctx: Context) -> Response {
  use json <- wisp.require_json(req)

  let result = {
    use pid <- result.try(game.play())
    use game_create_req <- result.try(decode_game_create_request(json))

    let table = ctx.room_table
    let room = uuid.v4_string() |> string.replace("-", "")

    uset.insert(table, [
      #(room, pid |> string.inspect, game_create_req.color, "0"),
    ])

    let object =
      json.object([
        #("pid", pid |> string.inspect |> json.string),
        #("room", room |> json.string),
        #("color", game_create_req.color |> json.string),
      ])
    Ok(json.to_string_builder(object))
  }

  case result {
    Ok(json) -> wisp.json_response(json, 201)
    Error(_) -> wisp.internal_server_error()
  }
}

fn join_game(req: Request, room: String, ctx: Context) -> Response {
  let table = ctx.room_table
  let assert Ok(existing_players) = {
    use room <- result.try(uset.lookup(table, room))
    Ok(room.3)
  }

  case existing_players {
    "0" -> join_game_first(req, room, ctx)
    "1" -> join_game_second(req, room, ctx)
    _ -> wisp.internal_server_error()
  }
}

fn join_game_first(_req: Request, room: String, ctx: Context) -> Response {
  let table = ctx.room_table
  let search = case uset.lookup(table, room) {
    Ok(object) ->
      json.object([
        #("room", room |> json.string),
        #("pid", object.1 |> json.string),
        #("color", object.2 |> json.string),
      ])
    Error(_) -> json.object([#("room", "not found" |> json.string)])
  }

  let result = {
    // And then a JSON response can be created from the person.
    let object = json.object([#("search", search)])
    Ok(json.to_string_builder(object))
  }

  // An appropriate response is returned depending on whether the JSON could be
  // successfully handled or not.
  case result {
    Ok(json) -> wisp.json_response(json, 200)

    // In a real application we would probably want to return some JSON error
    // object, but for this example we'll just return an empty response.
    Error(_) -> wisp.unprocessable_entity()
  }
}

fn join_game_second(_req: Request, _room: String, _ctx: Context) -> Response {
  let result = {
    let object =
      json.object([
        #("user", "You have joined the game as a second player" |> json.string),
      ])
    Ok(json.to_string_builder(object))
  }
  case result {
    Ok(json) -> wisp.json_response(json, 201)
    Error(_) -> wisp.unprocessable_entity()
  }
}
