import * as $bool from "../gleam_stdlib/gleam/bool.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { None } from "../gleam_stdlib/gleam/option.mjs";
import * as $uri from "../gleam_stdlib/gleam/uri.mjs";
import { Uri } from "../gleam_stdlib/gleam/uri.mjs";
import * as $lustre from "../lustre/lustre.mjs";
import * as $effect from "../lustre/lustre/effect.mjs";
import { CustomType as $CustomType } from "./gleam.mjs";
import {
  do_initial_uri as initial_uri,
  do_init,
  do_init as do_advanced,
  do_push,
  do_replace,
  do_load,
  do_forward,
  do_back,
} from "./modem.ffi.mjs";

export { initial_uri };

export class Options extends $CustomType {
  constructor(handle_internal_links, handle_external_links) {
    super();
    this.handle_internal_links = handle_internal_links;
    this.handle_external_links = handle_external_links;
  }
}

export function init(handler) {
  return $effect.from(
    (dispatch) => {
      return $bool.guard(
        !$lustre.is_browser(),
        undefined,
        () => {
          return do_init(
            (uri) => {
              let _pipe = uri;
              let _pipe$1 = handler(_pipe);
              return dispatch(_pipe$1);
            },
          );
        },
      );
    },
  );
}

export function advanced(options, handler) {
  return $effect.from(
    (dispatch) => {
      return $bool.guard(
        !$lustre.is_browser(),
        undefined,
        () => {
          return ((_capture) => { return do_advanced(_capture, options); })(
            (uri) => {
              let _pipe = uri;
              let _pipe$1 = handler(_pipe);
              return dispatch(_pipe$1);
            },
          );
        },
      );
    },
  );
}

export function load(uri) {
  return $effect.from(
    (_) => {
      return $bool.guard(
        !$lustre.is_browser(),
        undefined,
        () => { return do_load(uri); },
      );
    },
  );
}

export function forward(steps) {
  return $effect.from(
    (_) => {
      return $bool.guard(
        !$lustre.is_browser(),
        undefined,
        () => { return do_forward(steps); },
      );
    },
  );
}

export function back(steps) {
  return $effect.from(
    (_) => {
      return $bool.guard(
        !$lustre.is_browser(),
        undefined,
        () => { return do_back(steps); },
      );
    },
  );
}

const relative = /* @__PURE__ */ new Uri(
  /* @__PURE__ */ new None(),
  /* @__PURE__ */ new None(),
  /* @__PURE__ */ new None(),
  /* @__PURE__ */ new None(),
  "",
  /* @__PURE__ */ new None(),
  /* @__PURE__ */ new None(),
);

export function push(path, query, fragment) {
  return $effect.from(
    (_) => {
      return $bool.guard(
        !$lustre.is_browser(),
        undefined,
        () => {
          return do_push(
            relative.withFields({ path: path, query: query, fragment: fragment }),
          );
        },
      );
    },
  );
}

export function replace(path, query, fragment) {
  return $effect.from(
    (_) => {
      return $bool.guard(
        !$lustre.is_browser(),
        undefined,
        () => {
          return do_replace(
            relative.withFields({ path: path, query: query, fragment: fragment }),
          );
        },
      );
    },
  );
}
