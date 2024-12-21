import gleam/dynamic
import gleam/int
import gleam/io
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri.{type Uri}
import lustre
import lustre/attribute.{
  checked, class, href, id, name, placeholder, type_, value,
}
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
  Code
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
  UserCreatedGame
  UserJoinedGame(game_id: String)
  CopyGameId
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
      io.debug(new_mode)
      #(Model(..model, mode: new_mode), effect.none())
    }
    UserSelectedColor(value) -> #(Model(..model, color: value), effect.none())
    UserCreatedGame -> {
      let color = case model.color {
        "white" -> {
          "white"
        }
        "black" -> {
          "black"
        }
        "random" -> {
          case int.random(2) {
            0 -> "white"
            1 -> "black"
            _ -> "white"
          }
        }
        _ -> {
          io.debug("Starting game")
        }
      }

      #(Model(..model, route: Code, loading: True), case model.ws {
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
  html.div(
    [
      class(
        "bg-gradient-to-br from-base-300 to-base-200 min-h-screen flex items-center justify-center p-4",
      ),
    ],
    [
      html.div(
        [class("card bg-base-100 rounded-lg shadow-xl p-6 w-full max-w-md")],
        [
          case model.route {
            Home ->
              html.div([class("p-6 space-y-6 text-center ")], [
                html.h1([class("text-3xl font-bold text-primary")], [
                  text("Play chess with friends"),
                ]),
                case model.mode {
                  "create" -> create_game_view(model)
                  "join" -> join_game_view(model)
                  _ -> create_game_view(model)
                },
                toggle_mode_button(),
              ])

            Code ->
              html.div([class("p-6 space-y-6 text-center ")], [
                html.h1([class("text-3xl font-bold text-primary")], [
                  text("Your Game Code"),
                ]),
                html.h2([class("text-2xl font-semibold text-primary mt-2")], [
                  text(model.game),
                ]),
                html.p([class("text-primary mb-6")], [
                  text("Share this code with your friend to join the game!"),
                ]),
                html.p([class("text-primary mb-6")], [
                  text("Waiting for your friend to join..."),
                ]),
                html.button(
                  [
                    on_click(CopyGameId),
                    class("btn btn-primary text-primary-content"),
                  ],
                  [text("Copy Game Code")],
                ),
              ])

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
                      html.p(
                        [class("text-2xl font-semibold text-gray-600 mb-4")],
                        [text("Page Not Found")],
                      ),
                      html.p([class("text-gray-500 mb-8")], [
                        text(
                          "Oops! The page you are looking for doesn't exist.",
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
                      html.p(
                        [class("text-2xl font-semibold text-gray-600 mb-4")],
                        [text("Error")],
                      ),
                      html.p([class("text-gray-500 mb-8")], [
                        text(
                          "Looks like you got there was a "
                          <> error |> string.inspect,
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
          },
        ],
      ),
    ],
  )
}

fn toggle_mode_button() -> Element(Msg) {
  html.label(
    [class("swap swap-flip text-xl text-secondary"), type_("checkbox")],
    [
      html.input([type_("checkbox"), on_click(ToggleMode)]),
      html.div([class("swap-on")], [text("Or Create a Game")]),
      html.div([class("swap-off")], [text("Or Join a Game")]),
    ],
  )
}

fn create_game_view(model: Model) -> Element(Msg) {
  html.div([], [
    html.div([class("space-y-6")], [
      color_choice_form(model),
      html.button(
        [
          on_click(UserCreatedGame),
          class("btn btn-primary text-primary-content"),
        ],
        [text("Create New Game")],
      ),
    ]),
  ])
}

fn join_game_view(_model: Model) -> Element(Msg) {
  html.form([class("space-y-4")], [
    html.div([class("space-y-2")], [
      html.label([class("block text-xl font-medium text-primary")], [
        text("Enter Game Code"),
      ]),
      html.input([
        type_("text"),
        class("input input-bordered input-primary w-full max-w-xs"),
        placeholder("Enter Game Code"),
      ]),
      html.button(
        [
          // on_click(UserJoinedGame),
          class("btn btn-primary text-primary-content"),
        ],
        [text("Join Game")],
      ),
    ]),
  ])
}

fn color_choice_form(model: Model) -> Element(Msg) {
  html.form([class("space-y-4")], [
    html.div([class("space-y-2")], [
      html.label(
        [class("block text-xl font-medium text-primary"), type_("color-choice")],
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
          "w-8 h-8 rounded-full border-2 border-neutral peer-checked:border-primary peer-checked:ring-2 peer-checked:ring-primary transition-all duration-200 ease-in-out",
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
    html.span([class("text-neutral-content peer-checked:text-primary")], [
      text(label_text),
    ]),
  ])
}
