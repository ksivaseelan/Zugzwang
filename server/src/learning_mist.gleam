import gleam/bytes_builder
import gleam/dynamic.{type Dynamic, dict, dynamic, field, int, string}
import gleam/erlang/process.{type Pid}
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/io
import gleam/iterator
import gleam/json
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

//move json type
pub type Payload {
  Move(move: String)
  Surrender
}

pub type ClientMessage {
  ClientMessage(method: String, payload: Payload)
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
      gleam_binbo.print_board(pid)
      let assert Ok(_) = mist.send_text_frame(conn, pid |> string.inspect)

      actor.continue(GameState(pid))
    }
    mist.Text("Make Move: " <> move) -> {
      let _ = case state {
        GameState(pid) -> {
          case gleam_binbo.move(pid, move) {
            Ok(_) -> {
              gleam_binbo.print_board(pid)
              let assert Ok(_) =
                mist.send_text_frame(conn, "Move made: " <> move)
            }
            _ -> {
              let assert Ok(_) = mist.send_text_frame(conn, "Invalid move ")
            }
          }
        }
        _ -> {
          let assert Ok(_) =
            mist.send_text_frame(conn, "Invalid: Game must be created first")
        }
      }
      actor.continue(state)
    }
    mist.Text(message) -> {
      case client_message_from_json(message) {
        Ok(ClientMessage(method, payload)) -> {
          io.println(
            "Message received with method:" <> method |> string.inspect,
          )
          case method {
            "move" -> {
              case payload {
                Move(move) -> {
                  io.println("Move received:" <> move |> string.inspect)
                }
                _ -> {
                  io.println("Unknown payload:" <> payload |> string.inspect)
                }
              }
            }
            _ -> {
              io.println("Unknown method:" <> method |> string.inspect)
            }
          }
        }
        _ -> {
          io.println("invalid message Format:" <> message)
        }
      }

      // let assert Ok(_) = mist.send_text_frame(conn, "invalid move")
      actor.continue(state)
    }
    mist.Text(_) | mist.Binary(_) -> {
      actor.continue(state)
    }
    mist.Custom(Broadcast(text)) -> {
      let assert Ok(_) = mist.send_text_frame(conn, text)
      actor.continue(state)
    }
    mist.Closed | mist.Shutdown -> actor.Stop(process.Normal)
  }
}

// function to decode 
// fn move_from_payload(payload: Dynamic) -> Result(Payload, json.DecodeError) {
//   let payload_decoder = dynamic.decode1(Payload, field("payload", of: dynamic))
//   let move_decoder = dynamic.decode1(Move, field("move", of: string))
//   json.decode(from: payload, using: move_decoder)
// }

fn client_message_from_json(
  json_string: String,
) -> Result(ClientMessage, json.DecodeError) {
  let payload_decoder = dynamic.decode1(Move, field("move", of: string))
  let client_message_decoder =
    dynamic.decode2(
      ClientMessage,
      field("method", of: string),
      field("payload", of: payload_decoder),
    )
  json.decode(from: json_string, using: client_message_decoder)
}
