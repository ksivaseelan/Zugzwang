import gleam/dynamic
import gleam/int
import gleam/io
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri.{type Uri}
import lustre
import lustre/attribute.{checked, class, href, name, type_, value}
import lustre/effect.{type Effect}
import lustre/element.{type Element, text}
import lustre/element/html
import lustre/event.{on_click}
import lustre_http
import lustre_websocket.{
  InvalidUrl, OnBinaryMessage, OnClose, OnOpen, OnTextMessage,
} as ws
import modem

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

pub type Route {
  Home
  Game(game_id: String)
  NotFound
  ErrorPage(lustre_http.HttpError)
}

@external(javascript, "./ffi.ts", "get_route")
fn do_get_route() -> String

fn get_route() -> Route {
  let uri = case do_get_route() |> uri.parse {
    Ok(uri) -> uri
    _ -> panic as "Invalid URI"
  }
  uri.path |> string.inspect |> io.debug()

  case uri.path |> uri.path_segments {
    [] -> Home
    ["game", game_id] -> Game(game_id)
    _ -> NotFound
  }
}

fn on_url_change(_uri: Uri) -> Msg {
  OnRouteChange(get_route())
}

pub type Model {
  Model(game: String, color: String, route: Route, ws: Option(ws.WebSocket))
}

type Color {
  Black
  White
}

type Msg {
  WsWrapper(ws.WebSocketEvent)
  UserSelectedPieces(Color)
  UserStartedGame
  ApiReturnedGame(Result(GameCreateResponse, lustre_http.HttpError))
  OnRouteChange(Route)
}

fn init(_) -> #(Model, Effect(Msg)) {
  #(
    Model("", "", route: get_route(), ws: None),
    effect.batch([ws.init("/path", WsWrapper), modem.init(on_url_change)]),
  )
}

type GameCreateResponse {
  GameCreateResponse(pid: String, room: String, color: String)
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    WsWrapper(InvalidUrl) -> panic
    WsWrapper(OnOpen(socket)) -> #(
      Model(..model, ws: Some(socket)),
      ws.send(socket, "client-init"),
    )
    WsWrapper(OnTextMessage(msg)) -> todo
    WsWrapper(OnBinaryMessage(msg)) -> todo as "either-or"
    WsWrapper(OnClose(reason)) -> #(Model(..model, ws: None), effect.none())
    UserSelectedPieces(Black) -> #(
      Model(..model, color: "black"),
      effect.none(),
    )
    UserSelectedPieces(White) -> #(
      Model(..model, color: "white"),
      effect.none(),
    )
    UserStartedGame -> #(model, start_game(model))
    ApiReturnedGame(Ok(game)) -> #(
      Model(..model, game: game.room, route: Game(game.room)),
      effect.none(),
    )
    ApiReturnedGame(Error(error)) -> #(
      Model(..model, route: ErrorPage(error)),
      effect.none(),
    )
    OnRouteChange(route) -> #(Model(..model, route: route), effect.none())
  }
}

fn start_game(model: Model) -> Effect(Msg) {
  let decoder =
    dynamic.decode3(
      GameCreateResponse,
      dynamic.field("pid", dynamic.string),
      dynamic.field("room", dynamic.string),
      dynamic.field("color", dynamic.string),
    )
  let expect = lustre_http.expect_json(decoder, ApiReturnedGame)
  let object = json.object([#("color", model.color |> json.string)])
  lustre_http.post("http://localhost:6969/create", object, expect)
}

fn view(model: Model) -> Element(Msg) {
  case model.route {
    Home ->
      html.body(
        [
          class(
            "bg-gradient-to-br from-purple-100 to-blue-200 min-h-screen flex items-center justify-center p-4",
          ),
        ],
        [
          html.div(
            [class("bg-white rounded-lg shadow-xl p-6 w-full max-w-md")],
            [
              html.div([class("p-6 space-y-6 text-center")], [
                html.h1([class("text-3xl font-bold text-purple-700")], [
                  text("Create a chess game"),
                ]),
                html.p([class("text-purple-500 mt-2")], [
                  text("Choose your colour and invite a friend to play!"),
                ]),
                color_choice_form(),
                html.button(
                  [
                    class(
                      "w-full bg-purple-600 hover:bg-purple-700 text-white font-bold py-3 px-4 rounded-md transition duration-200 ease-in-out flex items-center justify-center",
                    ),
                  ],
                  [text("Create New Game")],
                ),
              ]),
            ],
          ),
        ],
      )
    Game(_game_id) ->
      html.body(
        [class("bg-gray-100 min-h-screen flex items-center justify-center")],
        [
          html.h1([class("text-3xl font-bold mb-6 text-center text-gray-800")], [
            text(model.game <> " Game"),
          ]),
          html.div(
            [class("grid grid-cols-8 w-96 h-96 border-4 border-gray-800")],
            [],
          ),
        ],
      )
    NotFound ->
      html.body(
        [
          class(
            "bg-gray-100 min-h-screen flex items-center justify-center px-4",
          ),
        ],
        [
          html.div(
            [
              class(
                "max-w-md w-full bg-white rounded-lg shadow-md p-8 text-center",
              ),
            ],
            [
              html.h1([class("text-6xl font-bold text-gray-800 mb-4")], [
                text("404"),
              ]),
              html.p([class("text-2xl font-semibold text-gray-600 mb-4")], [
                text("Page Not Found"),
              ]),
              html.p([class("text-gray-500 mb-8")], [
                text("Oops! The page you are looking for doesn't exist."),
              ]),
              html.a(
                [
                  href("/"),
                  class(
                    "inline-block bg-blue-500 hover:bg-blue-600 text-white font-bold py-2 px-4 rounded transition duration-300",
                  ),
                ],
                [text("Go to Home")],
              ),
            ],
          ),
        ],
      )
    ErrorPage(error) ->
      html.body(
        [
          class(
            "bg-gray-100 min-h-screen flex items-center justify-center px-4",
          ),
        ],
        [
          html.div(
            [
              class(
                "max-w-md w-full bg-white rounded-lg shadow-md p-8 text-center",
              ),
            ],
            [
              html.h1([class("text-6xl font-bold text-gray-800 mb-4")], [
                text("Oh No!"),
              ]),
              html.p([class("text-2xl font-semibold text-gray-600 mb-4")], [
                text("Error"),
              ]),
              html.p([class("text-gray-500 mb-8")], [
                text(
                  "Looks like you got there was a " <> error |> string.inspect,
                ),
              ]),
              html.a(
                [
                  href("/"),
                  class(
                    "inline-block bg-blue-500 hover:bg-blue-600 text-white font-bold py-2 px-4 rounded transition duration-300",
                  ),
                ],
                [text("Go to Home")],
              ),
            ],
          ),
        ],
      )
  }
}

fn color_choice_form() -> Element(Msg) {
  html.form([class("space-y-6")], [
    html.div([class("space-y-2")], [
      html.label(
        [
          class("block text-xl font-medium text-purple-700 text-left"),
          type_("color-choice"),
        ],
        // for is deprecated in favour of type_ for labels
        [text("Choose your color")],
      ),
      html.div([class("flex justify-center space-x-4")], [
        color_radio_button("white", "White", True),
        color_radio_button("black", "Black", False),
        color_radio_button("random", "Random", False),
      ]),
    ]),
  ])
}

fn color_radio_button(
  color_value: String,
  label_text: String,
  is_checked: Bool,
) -> Element(Msg) {
  html.label([class("flex items-center space-x-2 cursor-pointer")], [
    html.input([
      type_("radio"),
      name("color"),
      value(color_value),
      class("sr-only peer"),
      case is_checked {
        True -> checked(True)
        False -> checked(False)
      },
    ]),
    html.span(
      [
        class(
          "w-8 h-8 rounded-full border-2 border-gray-300 peer-checked:border-purple-500 peer-checked:ring-2 peer-checked:ring-purple-500 transition-all duration-200 ease-in-out",
        ),
        case color_value {
          "white" -> class("bg-white")
          "black" -> class("bg-gray-800")
          "random" -> class("bg-gradient-to-r from-white to-gray-800")
          _ -> class("")
        },
      ],
      [],
    ),
    html.span([class("text-gray-700 peer-checked:text-purple-700")], [
      text(label_text),
    ]),
  ])
}
