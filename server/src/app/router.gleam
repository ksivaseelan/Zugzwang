import app/web.{type Context}
import bravo/uset
import game
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Subject, type Pid}
import gleam/http.{Get, Post}
import gleam/io
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string
import gleam/string_builder
import wisp.{type Request, type Response}
import youid/uuid

pub type Color {
  Black
  White
}

pub type Player {
  Player(color: Color, id: String)
}

pub type Status {
  Initializing
  WaitingForPlayer
  InProgress
  Completed
}

pub type Turn {
  Turn(color: Color)
}

pub type GameActorMessage {
  CreateGame(
    repy_with: Subject(Result(GameState, GameError)),
    chosen_color: Color,
    player_id: String,
  )
  JoinGame(
    reply_wiht: Subject(Result(GameState, GameError)),
    player_id: String,
    game_id: String,
  )
  // MakeMove(player_id: String, move_details: String)
  // GetGameState(
  //   reply_with: Subject(Result(GameState, GameError)),
  //   player_id: String,
  // )
  // PlayerDisconnect(player_id: String)
  // EndGame(game_id: String, reason: String)
}

pub type GameState {
  GameState(
    player1: Option(Player),
    player2: Option(Player),
    id: String,
    pid: Pid,
    status: Status,
    turn: Turn,
  )
}

pub type GameError {
  GameFull
  GameNotFound
  InvalidColor
}

fn game_actor_loop(message: GameActorMessage, state: GameState) {
  case message, state {
    CreateGame(client, chosen_color, player_id), state ->
      case state {
        _ -> {
          //create game with first player

          let new_player = Player(chosen_color, player_id)
          io.debug("New player:")
          io.debug(new_player)
          let assert Ok(pid) = game.play()
          
          let new_state =
            GameState(
              player1: Some(new_player),
              player2: None,
              id: uuid.v4_string(),
              pid: pid,
              status: WaitingForPlayer,
              turn: Turn(White),
            )
          process.send(client, Ok(new_state))
          actor.continue(new_state)
        }
      }
    JoinGame(client, player_id, new_game_id), state -> {
      case state, new_game_id {
        GameState(
          status: WaitingForPlayer,
          player1: Some(existing_player),
          id: game_id,
          ..,
        ),
          new_game_id
        -> {
          case new_game_id == game_id {
            True -> {
              let new_color = case existing_player.color {
                White -> Black
                Black -> White
              }

              let new_player = Player(new_color, player_id)
              let new_state =
                GameState(
                  status: InProgress,
                  id: game_id,
                  pid: pid,
                  player1: Some(existing_player),
                  player2: Some(new_player),
                  turn: Turn(White),
                )
              process.send(client, Ok(new_state))
              actor.continue(new_state)
            }
            False -> {
              //game doesn't exist
              io.println("Error: Game doesn't exist")
              process.send(client, Error(GameNotFound))
              actor.continue(state)
            }
          }
        }
        _, _ -> {
          //error unjoinable game
          io.println("Error: Game is unjoinable")
          actor.continue(state)
        }
      }
    }
    // GetGameState(client, player_id), state -> {
    //   case state, player_id {
    //     NotStarted, _player_id -> {
    //       //error Game does not exist
    //       io.println("Error: Game does not exist")
    //       actor.continue(state)
    //     }
    //     GameState(player1: player1, player2: player2, ..), player_id -> {
    //       //verify requested player is in the game
    //       case player1, player2 {
    //         Some(player1), Some(player2) -> {
    //           case player1.id == player_id || player2.id == player_id {
    //             True -> {
    //               io.debug("Returning game state for player: " <> player_id)
    //               io.debug(state)
    //               actor.continue(state)
    //             }
    //             False -> {
    //               //error player not in game
    //               io.println("Error: Player not found in game")
    //               actor.continue(state)
    //             }
    //           }
    //         }
    //         Some(player1), None -> {
    //           case player1.id == player_id {
    //             True -> {
    //               io.debug("Returning game state for player: " <> player_id)
    //               io.debug(state)
    //               process.send(client, Ok(state))
    //               actor.continue(state)
    //             }
    //             False -> {
    //               //error player not in game
    //               io.println("Error: Player not found in game")
    //               actor.continue(state)
    //             }
    //           }
    //         }
    //         None, Some(player2) -> {
    //           case player2.id == player_id {
    //             True -> {
    //               io.debug("Returning game state for player: " <> player_id)
    //               io.debug(state)
    //               process.send(client, Ok(state))
    //               actor.continue(state)
    //             }
    //             False -> {
    //               //error player not in game
    //               io.println("Error: Player not found in game")
    //               actor.continue(state)
    //             }
    //           }
    //         }
    //         _, _ -> {
    //           //error player not in game
    //           io.println("Error: Player not found in game")
    //           actor.continue(state)
    //         }
    //       }
    //     }
    //   }
    // }
  }
}

/// The HTTP request handler- your application!
/// 
pub fn handle_request(req: Request, ctx: Context) -> Response {
  // Apply the middleware stack for this request/response.
  use _req <- web.middleware(req)

  case wisp.path_segments(req), req.method {
    [], Get -> home_page(req)
    ["create", color], Post -> create_game_route(req, ctx, color)
    ["join", id], Post -> join_game(req, id, ctx)
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

fn create_game_route(_req: Request, _ctx: Context, color: String) -> Response {
  let result = {
    let assert Ok(state) =
      actor.start(
        GameState(
          player1: None,
          player2: None,
          id: "",
          status: Initializing,
          turn: Turn(White),
        ),
        game_actor_loop,
      )
    let player_id = uuid.v4_string()

    let c = {
      case color {
        "white" -> White
        _ -> Black
      }
    }

    let assert Ok(game) = actor.call(state, CreateGame(_, c, player_id), 100)
    process.sleep(100)

    let player_id = case game.player1 {
      Some(player) -> player.id
      None -> ""
    }

    let link = "http://localhost:6969/join/" <> game.id

    // let assert Ok(state) = actor.call(state, GetGameState(_, player_id), 100)
    // io.debug("Game state from route function")
    // io.debug(state)
    // process.sleep(100)

    let object =
      json.object([
        #("game_id", game.id |> string.inspect |> json.string),
        #("player1", player_id |> json.string),
        #("link", link |> json.string),
      ])

    Ok(json.to_string_builder(object))
  }

  case result {
    Ok(json) -> wisp.json_response(json, 201)
    Error(_) -> wisp.internal_server_error()
  }
}

fn join_game(_req: Request, game_id: String, _ctx: Context) -> Response {
 
  wisp.ok()
}
// fn create_game_p(req: Request, ctx: Context) -> Response {
//   use json <- wisp.require_json(req)

//   let result = {
//     use pid <- result.try(game.play())
//     use game_create_req <- result.try(decode_game_create_request(json))

//     let table = ctx.room_table
//     let room = uuid.v4_string() |> string.replace("-", "")

//     uset.insert(table, [
//       #(room, pid |> string.inspect, game_create_req.color, "0"),
//     ])

//     let object =
//       json.object([
//         #("pid", pid |> string.inspect |> json.string),
//         #("room", room |> json.string),
//         #("color", game_create_req.color |> json.string),
//       ])
//     Ok(json.to_string_builder(object))
//   }

//   case result {
//     Ok(json) -> wisp.json_response(json, 201)
//     Error(_) -> wisp.internal_server_error()
//   }
// }
// fn join_game(req: Request, room: String, ctx: Context) -> Response {
//   let table = ctx.room_table
//   let assert Ok(existing_players) = {
//     use room <- result.try(uset.lookup(table, room))
//     Ok(room.3)
//   }

//   case existing_players {
//     "0" -> join_game_first(req, room, ctx)
//     "1" -> join_game_second(req, room, ctx)
//     _ -> wisp.internal_server_error()
//   }
// }

// fn join_game_first(_req: Request, room: String, ctx: Context) -> Response {
//   let table = ctx.room_table
//   let search = case uset.lookup(table, room) {
//     Ok(object) ->
//       json.object([
//         #("room", room |> json.string),
//         #("pid", object.1 |> json.string),
//         #("color", object.2 |> json.string),
//       ])
//     Error(_) -> json.object([#("room", "not found" |> json.string)])
//   }

//   let result = {
//     // And then a JSON response can be created from the person.
//     let object = json.object([#("search", search)])
//     Ok(json.to_string_builder(object))
//   }

//   // An appropriate response is returned depending on whether the JSON could be
//   // successfully handled or not.
//   case result {
//     Ok(json) -> wisp.json_response(json, 200)

//     // In a real application we would probably want to return some JSON error
//     // object, but for this example we'll just return an empty response.
//     Error(_) -> wisp.unprocessable_entity()
//   }
// }

// fn join_game_second(_req: Request, _room: String, _ctx: Context) -> Response {
//   let result = {
//     let object =
//       json.object([
//         #("user", "You have joined the game as a second player" |> json.string),
//       ])
//     Ok(json.to_string_builder(object))
//   }
//   case result {
//     Ok(json) -> wisp.json_response(json, 201)
//     Error(_) -> wisp.unprocessable_entity()
//   }
// }
