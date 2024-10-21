import * as $json from "../gleam_json/gleam/json.mjs";
import * as $dynamic from "../gleam_stdlib/gleam/dynamic.mjs";
import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $io from "../gleam_stdlib/gleam/io.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $uri from "../gleam_stdlib/gleam/uri.mjs";
import * as $lustre from "../lustre/lustre.mjs";
import * as $attribute from "../lustre/lustre/attribute.mjs";
import { class$, href, type_ } from "../lustre/lustre/attribute.mjs";
import * as $effect from "../lustre/lustre/effect.mjs";
import * as $element from "../lustre/lustre/element.mjs";
import { text } from "../lustre/lustre/element.mjs";
import * as $html from "../lustre/lustre/element/html.mjs";
import * as $event from "../lustre/lustre/event.mjs";
import { on_click } from "../lustre/lustre/event.mjs";
import * as $lustre_http from "../lustre_http/lustre_http.mjs";
import * as $modem from "../modem/modem.mjs";
import { get_route as do_get_route } from "./ffi.ts";
import { toList, CustomType as $CustomType, makeError } from "./gleam.mjs";

export class Home extends $CustomType {}

export class Game extends $CustomType {
  constructor(game_id) {
    super();
    this.game_id = game_id;
  }
}

export class NotFound extends $CustomType {}

export class ErrorPage extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Model extends $CustomType {
  constructor(game, color, route) {
    super();
    this.game = game;
    this.color = color;
    this.route = route;
  }
}

class Black extends $CustomType {}

class White extends $CustomType {}

class UserSelectedPieces extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class UserStartedGame extends $CustomType {}

class ApiReturnedGame extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class OnRouteChange extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class GameCreateResponse extends $CustomType {
  constructor(pid, room, color) {
    super();
    this.pid = pid;
    this.room = room;
    this.color = color;
  }
}

function get_route() {
  let uri = (() => {
    let $ = (() => {
      let _pipe = do_get_route();
      return $uri.parse(_pipe);
    })();
    if ($.isOk()) {
      let uri = $[0];
      return uri;
    } else {
      throw makeError("panic", "app", 37, "get_route", "Invalid URI", {})
    }
  })();
  let _pipe = uri.path;
  let _pipe$1 = $string.inspect(_pipe);
  $io.debug(_pipe$1)
  let $ = (() => {
    let _pipe$2 = uri.path;
    return $uri.path_segments(_pipe$2);
  })();
  if ($.hasLength(0)) {
    return new Home();
  } else if ($.hasLength(2) && $.head === "game") {
    let game_id = $.tail.head;
    return new Game(game_id);
  } else {
    return new NotFound();
  }
}

function on_url_change(_) {
  return new OnRouteChange(get_route());
}

function init(_) {
  return [new Model("", "", get_route()), $modem.init(on_url_change)];
}

function start_game(model) {
  let decoder = $dynamic.decode3(
    (var0, var1, var2) => { return new GameCreateResponse(var0, var1, var2); },
    $dynamic.field("pid", $dynamic.string),
    $dynamic.field("room", $dynamic.string),
    $dynamic.field("color", $dynamic.string),
  );
  let expect = $lustre_http.expect_json(
    decoder,
    (var0) => { return new ApiReturnedGame(var0); },
  );
  let object = $json.object(
    toList([
      [
        "color",
        (() => {
          let _pipe = model.color;
          return $json.string(_pipe);
        })(),
      ],
    ]),
  );
  return $lustre_http.post("http://localhost:6969/create", object, expect);
}

function update(model, msg) {
  if (msg instanceof UserSelectedPieces && msg[0] instanceof Black) {
    return [model.withFields({ color: "black" }), $effect.none()];
  } else if (msg instanceof UserSelectedPieces && msg[0] instanceof White) {
    return [model.withFields({ color: "white" }), $effect.none()];
  } else if (msg instanceof UserStartedGame) {
    return [model, start_game(model)];
  } else if (msg instanceof ApiReturnedGame && msg[0].isOk()) {
    let game = msg[0][0];
    return [
      model.withFields({ game: game.room, route: new Game(game.room) }),
      $effect.none(),
    ];
  } else if (msg instanceof ApiReturnedGame && !msg[0].isOk()) {
    let error = msg[0][0];
    return [model.withFields({ route: new ErrorPage(error) }), $effect.none()];
  } else {
    let route = msg[0];
    return [model.withFields({ route: route }), $effect.none()];
  }
}

function view(model) {
  let $ = model.route;
  if ($ instanceof Home) {
    return $html.body(
      toList([
        class$("bg-gray-100 min-h-screen flex items-center justify-center p-4"),
      ]),
      toList([
        $html.div(
          toList([class$("bg-white rounded-lg shadow-md p-6 w-full max-w-md")]),
          toList([
            $html.h1(
              toList([class$("text-2xl font-bold text-center mb-6")]),
              toList([text("Choose your pieces")]),
            ),
            $html.div(
              toList([
                class$("flex flex-col sm:flex-row justify-center gap-4 mb-6"),
              ]),
              toList([
                $html.button(
                  toList([
                    on_click(new UserSelectedPieces(new White())),
                    class$(
                      "flex-1 py-3 px-6 bg-gray-200 hover:bg-gray-300 hover:-translate-y-1 rounded-lg font-semibold text-gray-800 transition duration-300 ease-in-out",
                    ),
                    (() => {
                      let $1 = model.color;
                      if ($1 === "white") {
                        return class$("ring-2 ring-blue-500");
                      } else {
                        return class$("");
                      }
                    })(),
                  ]),
                  toList([text("White Pieces")]),
                ),
                $html.button(
                  toList([
                    on_click(new UserSelectedPieces(new Black())),
                    class$(
                      "flex-1 py-3 px-6 bg-gray-800 hover:bg-gray-700 hover:-translate-y-1 rounded-lg font-semibold text-white transition duration-300 ease-in-out",
                    ),
                    (() => {
                      let $1 = model.color;
                      if ($1 === "black") {
                        return class$("ring-2 ring-blue-500");
                      } else {
                        return class$("");
                      }
                    })(),
                  ]),
                  toList([text("Black Pieces")]),
                ),
              ]),
            ),
            $html.button(
              toList([
                on_click(new UserStartedGame()),
                class$(
                  "w-full py-3 px-6 bg-green-500 hover:bg-green-600 hover:-translate-y-1 rounded-lg font-semibold text-white transition duration-300 ease-in-out focus:outline-none focus:ring-2 focus:ring-green-400",
                ),
              ]),
              toList([text("Start game")]),
            ),
          ]),
        ),
      ]),
    );
  } else if ($ instanceof Game) {
    return $html.body(
      toList([
        class$("bg-gray-100 min-h-screen flex items-center justify-center"),
      ]),
      toList([
        $html.h1(
          toList([class$("text-3xl font-bold mb-6 text-center text-gray-800")]),
          toList([text(model.game + " Game")]),
        ),
        $html.div(
          toList([class$("grid grid-cols-8 w-96 h-96 border-4 border-gray-800")]),
          toList([]),
        ),
      ]),
    );
  } else if ($ instanceof NotFound) {
    return $html.body(
      toList([
        class$("bg-gray-100 min-h-screen flex items-center justify-center px-4"),
      ]),
      toList([
        $html.div(
          toList([
            class$(
              "max-w-md w-full bg-white rounded-lg shadow-md p-8 text-center",
            ),
          ]),
          toList([
            $html.h1(
              toList([class$("text-6xl font-bold text-gray-800 mb-4")]),
              toList([text("404")]),
            ),
            $html.p(
              toList([class$("text-2xl font-semibold text-gray-600 mb-4")]),
              toList([text("Page Not Found")]),
            ),
            $html.p(
              toList([class$("text-gray-500 mb-8")]),
              toList([text("Oops! The page you are looking for doesn't exist.")]),
            ),
            $html.a(
              toList([
                href("/"),
                class$(
                  "inline-block bg-blue-500 hover:bg-blue-600 text-white font-bold py-2 px-4 rounded transition duration-300",
                ),
              ]),
              toList([text("Go to Home")]),
            ),
          ]),
        ),
      ]),
    );
  } else {
    let error = $[0];
    return $html.body(
      toList([
        class$("bg-gray-100 min-h-screen flex items-center justify-center px-4"),
      ]),
      toList([
        $html.div(
          toList([
            class$(
              "max-w-md w-full bg-white rounded-lg shadow-md p-8 text-center",
            ),
          ]),
          toList([
            $html.h1(
              toList([class$("text-6xl font-bold text-gray-800 mb-4")]),
              toList([text("Oh No!")]),
            ),
            $html.p(
              toList([class$("text-2xl font-semibold text-gray-600 mb-4")]),
              toList([text("Error")]),
            ),
            $html.p(
              toList([class$("text-gray-500 mb-8")]),
              toList([
                text(
                  "Looks like you got there was a " + (() => {
                    let _pipe = error;
                    return $string.inspect(_pipe);
                  })(),
                ),
              ]),
            ),
            $html.a(
              toList([
                href("/"),
                class$(
                  "inline-block bg-blue-500 hover:bg-blue-600 text-white font-bold py-2 px-4 rounded transition duration-300",
                ),
              ]),
              toList([text("Go to Home")]),
            ),
          ]),
        ),
      ]),
    );
  }
}

export function main() {
  let app = $lustre.application(init, update, view);
  let $ = $lustre.start(app, "#app", undefined);
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "app",
      19,
      "main",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  return undefined;
}
