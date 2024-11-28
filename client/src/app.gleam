import gleam/dynamic
import gleam/int
import gleam/io
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri.{type Uri}
import lustre
import lustre/attribute.{checked, class, href, name, placeholder, type_, value}
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
  Game
  NotFound
  ErrorPage(lustre_http.HttpError)
}

@external(javascript, "./ffi.ts", "get_route")
fn do_get_route() -> String

@external(javascript, "./ffi.ts", "copy_game_id")
fn copy_game_id(game_id: String) -> Nil

fn get_route() -> Route {
  let uri = case do_get_route() |> uri.parse {
    Ok(uri) -> uri
    _ -> panic as "Invalid URI"
  }
  uri.path |> string.inspect |> io.debug()

  case uri.path |> uri.path_segments {
    [] -> Home
    _ -> NotFound
  }
}

fn on_url_change(_uri: Uri) -> Msg {
  OnRouteChange(get_route())
}

pub type Model {
  Model(
    game: String,
    color: String,
    route: Route,
    ws: Option(ws.WebSocket),
    loading: Bool,
    mode: String,
  )
}

type Msg {
  OnRouteChange(Route)
  WsWrapper(ws.WebSocketEvent)
  ToggleMode
  UserSelectedColor(value: String)
  UserStartedGame
  UserJoinedGame(game_id: String)
  CopyGameId
  // ApiReturnedGame(Result(GameCreateResponse, lustre_http.HttpError))
}

fn init(_) -> #(Model, Effect(Msg)) {
  #(
    Model(
      "",
      "white",
      route: get_route(),
      ws: None,
      loading: False,
      mode: "create",
    ),
    effect.batch([
      ws.init("ws://localhost:6969/ws", WsWrapper),
      modem.init(on_url_change),
    ]),
  )
}

// type GameCreateResponse {
//   GameCreateResponse(pid: String, room: String, color: String)
// }

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    OnRouteChange(route) -> #(Model(..model, route: route), effect.none())
    WsWrapper(InvalidUrl) -> panic
    WsWrapper(OnOpen(socket)) -> #(
      Model(..model, ws: Some(socket)),
      ws.send(socket, "ping"),
    )
    WsWrapper(OnTextMessage(msg)) -> {
      case msg {
        "Create Success: Game id " <> game_id -> {
          io.debug("Received create success" <> game_id)
          #(Model(..model, game: game_id, loading: False), effect.none())
        }

        _ -> {
          io.debug("Received message: " <> msg)
          #(model, effect.none())
        }
      }
    }
    WsWrapper(OnBinaryMessage(_msg)) -> #(model, effect.none())
    WsWrapper(OnClose(_reason)) -> #(Model(..model, ws: None), effect.none())
    ToggleMode -> {
      let new_mode = case model.mode {
        "create" -> "join"
        "join" -> "create"
        _ -> "create"
      }
      #(Model(..model, mode: new_mode), effect.none())
    }
    UserSelectedColor(value) -> #(Model(..model, color: value), effect.none())
    UserStartedGame -> {
      let color = case model.color {
        "" -> {
          io.debug("No color selected")
        }
        _ -> {
          io.debug("Starting game")
        }
      }

      #(Model(..model, route: Game, loading: True), case model.ws {
        None -> effect.none()
        Some(socket) -> ws.send(socket, "Create as " <> color)
      })
    }
    UserJoinedGame(_game_id) -> {
      #(model, effect.none())
    }
    CopyGameId -> {
      case model.game {
        "" -> {
          io.debug("No game id to copy")
          Nil
        }
        _ -> {
          io.debug("Copying game id")
          copy_game_id(model.game)
        }
      }
      #(model, effect.none())
    }
  }
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
                  text("Play chess with friends"),
                ]),
                html.p([class("text-purple-500 mt-2")], [
                  text("Choose your colour and invite a friend to play!"),
                ]),
                html.div([class("flex justify-center space-x-4 mb-6")], []),
                toggle_mode_button(model.mode),
                case model.mode {
                  "create" -> create_game_view(model)
                  "join" -> join_game_view(model)
                  _ -> create_game_view(model)
                },
              ]),
            ],
          ),
        ],
      )
    Game ->
      html.body(
        [
          class(
            "bg-gradient-to-br from-purple-100 to-blue-200 min-h-screen flex items-center justify-center p-4",
          ),
        ],
        [
          html.div(
            [
              class(
                "bg-white rounded-2xl shadow-2xl p-8 w-full max-w-md text-center",
              ),
            ],
            [
              html.div([class("mb-6")], [
                html.h1([class("text-4xl font-bold text-purple-700")], [
                  text("Your Game Code"),
                ]),
                html.h2([class("text-2xl font-semibold text-purple-600 mt-2")], [
                  text(model.game),
                ]),
              ]),
              html.p([class("text-purple-500 mb-6")], [
                text("Share this code with your friend to join the game!"),
              ]),
              html.p([class("text-purple-500 mb-6")], [
                text("Waiting for your friend to join..."),
              ]),
              html.div([class("flex justify-center")], [
                html.button(
                  [
                    on_click(CopyGameId),
                    class(
                      "bg-purple-600 hover:bg-purple-700 text-white font-bold py-3 px-6 rounded-full transition duration-300 ease-in-out",
                    ),
                  ],
                  [text("Copy Game Code")],
                ),
              ]),
            ],
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

fn toggle_mode_button(mode: String) -> Element(Msg) {
  let button_class = case mode {
    "join" -> "bg-indigo-600 hover:bg-indigo-700"
    _ -> "bg-pink-600 hover:bg-pink-700"
  }

  html.button(
    [
      on_click(ToggleMode),
      class(
        button_class
        <> " text-white font-bold py-2 px-4 rounded-md transition duration-200 ease-in-out",
      ),
    ],
    [
      text(case mode {
        "create" -> "Switch to Join Game"
        "join" -> "Switch to Create Game"
        _ -> "Create Game"
      }),
    ],
  )
}

fn create_game_view(model: Model) -> Element(Msg) {
  html.div([class("space-y-6")], [
    color_choice_form(model),
    html.button(
      [
        on_click(UserStartedGame),
        class(
          "w-full bg-purple-600 hover:bg-purple-700 text-white font-bold py-3 px-4 rounded-md transition duration-200 ease-in-out flex items-center justify-center",
        ),
      ],
      [text("Create New Game")],
    ),
  ])
}

fn join_game_view(_model: Model) -> Element(Msg) {
  html.div([class("space-y-6")], [
    html.input([
      type_("text"),
      class("w-full p-2 border border-gray-300 rounded-md"),
      placeholder("Enter Game ID"),
    ]),
    html.button(
      [
        // on_click(UserJoinedGame),
        class(
          "w-full mt-2 bg-purple-600 hover:bg-purple-700 text-white font-bold py-3 px-4 rounded-md transition duration-200 ease-in-out flex items-center justify-center",
        ),
      ],
      [text("Join Game")],
    ),
  ])
}

fn color_choice_form(model: Model) -> Element(Msg) {
  html.form([class("space-y-4")], [
    html.div([class("space-y-2")], [
      html.label(
        [
          class("block text-xl font-medium text-purple-700 text-center"),
          type_("color-choice"),
        ],
        // for is deprecated in favour of type_ for labels
        [text("Choose your color")],
      ),
      html.div([class("flex justify-center space-x-4")], [
        color_radio_button("white", "White", model.color == "white"),
        color_radio_button("black", "Black", model.color == "black"),
        color_radio_button("random", "Random", model.color == "random"),
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
      on_click(UserSelectedColor(color_value)),
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
