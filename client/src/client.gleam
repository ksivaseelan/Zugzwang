import gleam/dynamic
import gleam/int
import gleam/io
import gleam/json
import gleam/result
import gleam/string
import gleam/uri.{type Uri}
import lustre
import lustre/attribute.{class, href, type_}
import lustre/effect.{type Effect}
import lustre/element.{type Element, text}
import lustre/element/html
import lustre/event.{on_click}
import lustre_http
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

@external(javascript, "./ffi.mjs", "get_route")
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
  Model(game: String, color: String, route: Route)
}

type Color {
  Black
  White
}

type Msg {
  UserSelectedPieces(Color)
  UserStartedGame
  ApiReturnedGame(Result(GameCreateResponse, lustre_http.HttpError))
  OnRouteChange(Route)
}

fn init(_) -> #(Model, Effect(Msg)) {
  #(Model("", "", route: get_route()), modem.init(on_url_change))
}

type GameCreateResponse {
  GameCreateResponse(pid: String, room: String, color: String)
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
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
    Home -> html.body([], [html.h1([], [text("Welcome to Zugzwang")])])

    // html.body(
    //   [class("bg-gray-100 min-h-screen flex items-center justify-center p-4")],
    //   [
    //     html.div(
    //       [class("bg-white rounded-lg shadow-md p-6 w-full max-w-md")],
    //       [
    //         html.h1([class("text-2xl font-bold text-center mb-6")], [
    //           text("Choose your pieces"),
    //         ]),
    //         html.div(
    //           [class("flex flex-col sm:flex-row justify-center gap-4 mb-6")],
    //           [
    //             html.button(
    //               [
    //                 on_click(UserSelectedPieces(White)),
    //                 class(
    //                   "flex-1 py-3 px-6 bg-gray-200 hover:bg-gray-300 hover:-translate-y-1 rounded-lg font-semibold text-gray-800 transition duration-300 ease-in-out",
    //                 ),
    //                 case model.color {
    //                   "white" -> class("ring-2 ring-blue-500")
    //                   _ -> class("")
    //                 },
    //               ],
    //               [text("White Pieces")],
    //             ),
    //             html.button(
    //               [
    //                 on_click(UserSelectedPieces(Black)),
    //                 class(
    //                   "flex-1 py-3 px-6 bg-gray-800 hover:bg-gray-700 hover:-translate-y-1 rounded-lg font-semibold text-white transition duration-300 ease-in-out",
    //                 ),
    //                 case model.color {
    //                   "black" -> class("ring-2 ring-blue-500")
    //                   _ -> class("")
    //                 },
    //               ],
    //               [text("Black Pieces")],
    //             ),
    //           ],
    //         ),
    //         html.button(
    //           [
    //             on_click(UserStartedGame),
    //             class(
    //               "w-full py-3 px-6 bg-green-500 hover:bg-green-600 hover:-translate-y-1 rounded-lg font-semibold text-white transition duration-300 ease-in-out focus:outline-none focus:ring-2 focus:ring-green-400",
    //             ),
    //           ],
    //           [text("Start game")],
    //         ),
    //       ],
    //     ),
    //   ],
    // )
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
                text("Looks like you got the error" <> error |> string.inspect),
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
