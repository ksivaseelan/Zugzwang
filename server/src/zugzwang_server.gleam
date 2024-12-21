import gleam/bytes_builder
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic, dict, dynamic, field, int, string}
import gleam/erlang/process.{type Pid}
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/io
import gleam/iterator
import gleam/option.{None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string
import gleam_binbo
import mist.{type Connection, type ResponseData}
import youid/uuid

pub type State {
  GameState(pids: Dict(String, Pid))
}

pub fn main() {
  // These values are for the Websocket process initialized below
  let selector = process.new_selector()
  let state = dict.new()

  let not_found =
    response.new(404)
    |> response.set_body(mist.Bytes(bytes_builder.new()))

  let assert Ok(_) =
    fn(req: Request(Connection)) -> Response(ResponseData) {
      case request.path_segments(req) {
        ["ws"] ->
          mist.websocket(
            request: req,
            on_init: fn(_conn) { #(state, Some(selector)) },
            on_close: fn(_state) { io.println("goodbye!") },
            handler: handle_ws_message,
          )
        _ -> not_found
      }
    }
    |> mist.new
    |> mist.port(6969)
    |> mist.start_http

  process.sleep_forever()
}

pub type MyMessage {
  Broadcast(String)
}

fn handle_ws_message(state, conn, message) {
  case message {
    mist.Text("ping") -> {
      io.println("ping received")
      let assert Ok(_) = mist.send_text_frame(conn, "pong")
      io.debug("state is " <> state|> string.inspect)
      actor.continue(state)
    }
    mist.Text("Create as " <> _color) -> {
      io.debug("Create received")
      let assert Ok(pid) = gleam_binbo.play()
      io.debug("New pid:" <> pid |> string.inspect)
      gleam_binbo.print_board(pid)

      let game_id = uuid.v4_string()
      io.debug("New game_id:" <> game_id)
      let new_state = dict.insert(state, game_id, pid)
      let assert Ok(_) =
        mist.send_text_frame(conn, "Create Success: Game id " <> game_id)
      actor.continue(new_state)
    }
    // Move message is "Move: <game_id>:<move>"
    mist.Text("Move: " <> game_and_move) -> {
      io.debug(string.split(game_and_move, ":"))
      let _ = case string.split(game_and_move, ":") {
        [game_id, move] -> {
          case dict.get(state, game_id) {
            Ok(pid) -> {
              case gleam_binbo.move(pid, move) {
                Ok(_) -> {
                  gleam_binbo.print_board(pid)
                  let assert Ok(_) =
                    mist.send_text_frame(conn, "Move Success: " <> move)
                }
                _ -> {
                  let assert Ok(_) =
                    mist.send_text_frame(conn, "Move Error: Not a legal move")
                }
              }
            }
            _ -> {
              let assert Ok(_) =
                mist.send_text_frame(
                  conn,
                  "Game Error: Game with id " <> game_id <> " not found",
                )
            }
          }
        }
        _ -> {
          let assert Ok(_) =
            mist.send_text_frame(conn, "Game Error: Invalid move message")
        }
      }
      actor.continue(state)
    }

    // mist.Text("Join: " <> game_id) -> {
    //   let _ = case dict.get(state, game_id) {
    //     Ok(pid) -> {
    //       let assert Ok(_) =
    //         mist.send_text_frame(
    //           conn,
    //           "Game Success: Joined game with id " <> game_id,
    //         )
    //     }
    //     _ -> {
    //       let assert Ok(_) =
    //         mist.send_text_frame(
    //           conn,
    //           "Game Error: Game with id " <> game_id <> " not found",
    //         )
    //     }
    //   }
    //   actor.continue(state)
    // }
    mist.Text(_) | mist.Binary(_) -> {
      let assert Ok(_) = mist.send_text_frame(conn, "Unknown Message type")
      actor.continue(state)
    }
    mist.Custom(Broadcast(text)) -> {
      let assert Ok(_) = mist.send_text_frame(conn, text)
      actor.continue(state)
    }
    mist.Closed | mist.Shutdown -> actor.Stop(process.Normal)
  }
}
