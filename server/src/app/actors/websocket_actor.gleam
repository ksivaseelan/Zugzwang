import birl
import gleam/erlang/process.{type Subject}
import gleam/function
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/option.{type Option, None, Some}
import gleam/otp/actor.{type Next, Stop}
import mist.{
  type Connection, type ResponseData, type WebsocketConnection,
  type WebsocketMessage, Custom, Text,
}

import gleam/int

import gleam/string
import logging

pub type WebsocketActorState {
  WebsocketActorState(
    name: Option(String),
    ws_subject: Subject(CustomWebsocketMessage),
  )
}

pub type CustomWebsocketMessage {
  SendToClient(message: Message)
}

pub type Message {
  ChatMessage(text: String, author: String, created_at: String)
}

pub fn start(req: Request(Connection)) -> Response(ResponseData) {
  mist.websocket(
    request: req,
    on_init: fn(_) {
      let ws_subject = process.new_subject()
      let new_selector =
        process.new_selector()
        |> process.selecting(ws_subject, function.identity)
      let state = WebsocketActorState(name: None, ws_subject: ws_subject)
      #(state, Some(new_selector))
    },
    on_close: fn(_state) {
      logging.log(logging.Info, "A connection was closed")
      Nil
    },
    handler: handle_message,
  )
}

fn handle_message(
  state: WebsocketActorState,
  connection: WebsocketConnection,
  message: WebsocketMessage(CustomWebsocketMessage),
) -> Next(CustomWebsocketMessage, WebsocketActorState) {
  case message {
    Text(text) -> {
      actor.continue(state)
    }
    _ -> actor.continue(state)
  }
}
