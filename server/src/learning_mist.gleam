import gleam/bytes_builder
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

pub type State {
  GameState(pid: Pid)
  // Store pid as String for easier serialization if needed
  NoGame
}

pub fn main() {
  // These values are for the Websocket process initialized below
  let selector = process.new_selector()
  let state = NoGame

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

      actor.continue(state)
    }
    mist.Text("Create") -> {
      let assert Ok(pid) = gleam_binbo.play()
      io.debug("Created game with pid:" <> pid |> string.inspect)
      gleam_binbo.print_board(pid)
      let assert Ok(_) =
        mist.send_text_frame(
          conn,
          "Game Success: Game created with " <> pid |> string.inspect,
        )

      actor.continue(GameState(pid))
    }
    mist.Text("Move: " <> move) -> {
      let _ = case state {
        GameState(pid) -> {
          case gleam_binbo.move(pid, move) {
            Ok(_) -> {
              gleam_binbo.print_board(pid)
              let assert Ok(_) =
                mist.send_text_frame(conn, "Move Success: " <> move <> " made")
            }
            _ -> {
              let assert Ok(_) =
                mist.send_text_frame(conn, "Move Error: Not a legal move")
            }
          }
        }
        _ -> {
          let assert Ok(_) =
            mist.send_text_frame(conn, "Game Error: Game must be created first")
        }
      }
      actor.continue(state)
    }
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
