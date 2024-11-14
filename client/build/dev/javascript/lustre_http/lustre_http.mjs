import * as $fetch from "../gleam_fetch/gleam/fetch.mjs";
import * as $http from "../gleam_http/gleam/http.mjs";
import * as $request from "../gleam_http/gleam/http/request.mjs";
import * as $response from "../gleam_http/gleam/http/response.mjs";
import { Response } from "../gleam_http/gleam/http/response.mjs";
import * as $promise from "../gleam_javascript/gleam/javascript/promise.mjs";
import * as $json from "../gleam_json/gleam/json.mjs";
import * as $dynamic from "../gleam_stdlib/gleam/dynamic.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $effect from "../lustre/lustre/effect.mjs";
import { Ok, Error, CustomType as $CustomType } from "./gleam.mjs";

export class BadUrl extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class InternalServerError extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class JsonError extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class NetworkError extends $CustomType {}

export class NotFound extends $CustomType {}

export class OtherError extends $CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
}

export class Unauthorized extends $CustomType {}

class ExpectTextResponse extends $CustomType {
  constructor(run) {
    super();
    this.run = run;
  }
}

function do_send(req, expect, dispatch) {
  let _pipe = $fetch.send(req);
  let _pipe$1 = $promise.try_await(_pipe, $fetch.read_text_body);
  let _pipe$2 = $promise.map(
    _pipe$1,
    (response) => {
      if (response.isOk()) {
        let res = response[0];
        return expect.run(new Ok(res));
      } else {
        return expect.run(new Error(new NetworkError()));
      }
    },
  );
  let _pipe$3 = $promise.rescue(
    _pipe$2,
    (_) => { return expect.run(new Error(new NetworkError())); },
  );
  $promise.tap(_pipe$3, dispatch)
  return undefined;
}

export function get(url, expect) {
  return $effect.from(
    (dispatch) => {
      let $ = $request.to(url);
      if ($.isOk()) {
        let req = $[0];
        return do_send(req, expect, dispatch);
      } else {
        return dispatch(expect.run(new Error(new BadUrl(url))));
      }
    },
  );
}

export function post(url, body, expect) {
  return $effect.from(
    (dispatch) => {
      let $ = $request.to(url);
      if ($.isOk()) {
        let req = $[0];
        let _pipe = req;
        let _pipe$1 = $request.set_method(_pipe, new $http.Post());
        let _pipe$2 = $request.set_header(
          _pipe$1,
          "Content-Type",
          "application/json",
        );
        let _pipe$3 = $request.set_body(_pipe$2, $json.to_string(body));
        return do_send(_pipe$3, expect, dispatch);
      } else {
        return dispatch(expect.run(new Error(new BadUrl(url))));
      }
    },
  );
}

export function send(req, expect) {
  return $effect.from((_capture) => { return do_send(req, expect, _capture); });
}

function response_to_result(response) {
  if (response instanceof Response &&
  ((200 <= response.status) && (response.status <= 299))) {
    let status = response.status;
    let body = response.body;
    return new Ok(body);
  } else if (response instanceof Response && response.status === 401) {
    return new Error(new Unauthorized());
  } else if (response instanceof Response && response.status === 404) {
    return new Error(new NotFound());
  } else if (response instanceof Response && response.status === 500) {
    let body = response.body;
    return new Error(new InternalServerError(body));
  } else {
    let code = response.status;
    let body = response.body;
    return new Error(new OtherError(code, body));
  }
}

export function expect_anything(to_msg) {
  return new ExpectTextResponse(
    (response) => {
      let _pipe = response;
      let _pipe$1 = $result.then$(_pipe, response_to_result);
      let _pipe$2 = $result.replace(_pipe$1, undefined);
      return to_msg(_pipe$2);
    },
  );
}

export function expect_text(to_msg) {
  return new ExpectTextResponse(
    (response) => {
      let _pipe = response;
      let _pipe$1 = $result.then$(_pipe, response_to_result);
      return to_msg(_pipe$1);
    },
  );
}

export function expect_json(decoder, to_msg) {
  return new ExpectTextResponse(
    (response) => {
      let _pipe = response;
      let _pipe$1 = $result.then$(_pipe, response_to_result);
      let _pipe$2 = $result.then$(
        _pipe$1,
        (body) => {
          let $ = $json.decode(body, decoder);
          if ($.isOk()) {
            let json = $[0];
            return new Ok(json);
          } else {
            let json_error = $[0];
            return new Error(new JsonError(json_error));
          }
        },
      );
      return to_msg(_pipe$2);
    },
  );
}

export function expect_text_response(on_response, on_failure, to_msg) {
  return new ExpectTextResponse(
    (response) => {
      if (response.isOk()) {
        let response$1 = response[0];
        return to_msg(on_response(response$1));
      } else {
        let error = response[0];
        return to_msg(new Error(on_failure(error)));
      }
    },
  );
}
