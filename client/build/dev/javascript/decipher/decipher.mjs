import * as $birl from "../birl/birl.mjs";
import * as $json from "../gleam_json/gleam/json.mjs";
import * as $bit_array from "../gleam_stdlib/gleam/bit_array.mjs";
import * as $dict from "../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../gleam_stdlib/gleam/dynamic.mjs";
import { DecodeError } from "../gleam_stdlib/gleam/dynamic.mjs";
import * as $float from "../gleam_stdlib/gleam/float.mjs";
import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $pair from "../gleam_stdlib/gleam/pair.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $set from "../gleam_stdlib/gleam/set.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $uri from "../gleam_stdlib/gleam/uri.mjs";
import * as $version from "../stoiridh_version/stoiridh/version.mjs";
import { Ok, Error, toList, prepend as listPrepend } from "./gleam.mjs";

export function non_negative_int(dynamic) {
  return $result.try$(
    $dynamic.int(dynamic),
    (int) => {
      let $ = int >= 0;
      if ($) {
        return new Ok(int);
      } else {
        return new Error(
          toList([
            new DecodeError(
              "A non-negative int",
              $int.to_string(int),
              toList([]),
            ),
          ]),
        );
      }
    },
  );
}

export function int_string(dynamic) {
  return $result.try$(
    $dynamic.string(dynamic),
    (string) => {
      let _pipe = string;
      let _pipe$1 = $int.parse(_pipe);
      return $result.replace_error(
        _pipe$1,
        toList([new DecodeError("A stringified int", string, toList([]))]),
      );
    },
  );
}

export function float_string(dynamic) {
  return $result.try$(
    $dynamic.string(dynamic),
    (string) => {
      let _pipe = string;
      let _pipe$1 = $float.parse(_pipe);
      return $result.replace_error(
        _pipe$1,
        toList([new DecodeError("A stringified float", string, toList([]))]),
      );
    },
  );
}

export function number(dynamic) {
  return $dynamic.any(
    toList([
      $dynamic.float,
      (dynamic) => {
        let _pipe = $dynamic.int(dynamic);
        return $result.map(_pipe, $int.to_float);
      },
    ]),
  )(dynamic);
}

export function number_string(dynamic) {
  return $dynamic.any(
    toList([
      float_string,
      (dynamic) => {
        let _pipe = int_string(dynamic);
        return $result.map(_pipe, $int.to_float);
      },
    ]),
  )(dynamic);
}

export function non_empty_string(dynamic) {
  return $result.try$(
    $dynamic.string(dynamic),
    (string) => {
      if (string === "") {
        return new Error(
          toList([
            new DecodeError("A non-empty string", "An empty string", toList([])),
          ]),
        );
      } else {
        return new Ok(string);
      }
    },
  );
}

export function non_empty_list(decode) {
  return (dynamic) => {
    return $result.try$(
      $dynamic.list(decode)(dynamic),
      (list) => {
        let $ = $list.is_empty(list);
        if ($) {
          return new Error(
            toList([
              new DecodeError(
                "A non-empty list",
                "A list with at least 1 item",
                toList([]),
              ),
            ]),
          );
        } else {
          return new Ok(list);
        }
      },
    );
  };
}

export function all(decoders) {
  return (dynamic) => {
    return $list.fold_right(
      decoders,
      new Ok(toList([])),
      (list, decoder) => {
        let $ = decoder(dynamic);
        if (list.isOk() && $.isOk()) {
          let xs = list[0];
          let x = $[0];
          return new Ok(listPrepend(x, xs));
        } else if (list.isOk() && !$.isOk()) {
          let e = $[0];
          return new Error(e);
        } else if (!list.isOk() && $.isOk()) {
          let e = list[0];
          return new Error(e);
        } else {
          let e = list[0];
          let x = $[0];
          return new Error($list.append(e, x));
        }
      },
    );
  };
}

export function arraylike(decoder) {
  return (dynamic) => {
    return $result.try$(
      $dynamic.field("length", $dynamic.int)(dynamic),
      (length) => {
        return all(
          (() => {
            let list = $list.range(0, length - 1);
            return $list.map(
              list,
              (index) => {
                return $dynamic.field($int.to_string(index), decoder);
              },
            );
          })(),
        )(dynamic);
      },
    );
  };
}

export function set(decoder) {
  return (dynamic) => {
    let _pipe = dynamic;
    let _pipe$1 = $dynamic.any(
      toList([$dynamic.list(decoder), arraylike(decoder)]),
    )(_pipe);
    return $result.map(_pipe$1, $set.from_list);
  };
}

export function exact_set(decoder) {
  return (dynamic) => {
    return $result.try$(
      $dynamic.any(toList([$dynamic.list(decoder), arraylike(decoder)]))(
        dynamic,
      ),
      (list) => {
        let length = $list.length(list);
        let set$1 = $set.from_list(list);
        let $ = $set.size(set$1) === length;
        if ($) {
          return new Ok(set$1);
        } else {
          return new Error(
            toList([
              new DecodeError(
                "A list with no duplicate values",
                "A list with duplicate values",
                toList([]),
              ),
            ]),
          );
        }
      },
    );
  };
}

export function tagged_union(tag, variants) {
  let switch$ = $dict.from_list(variants);
  return (dynamic) => {
    return $result.try$(
      tag(dynamic),
      (kind) => {
        let $ = $dict.get(switch$, kind);
        if ($.isOk()) {
          let decoder = $[0];
          return decoder(dynamic);
        } else {
          let tags = (() => {
            let _pipe = $dict.keys(switch$);
            let _pipe$1 = $list.map(_pipe, $string.inspect);
            return $string.join(_pipe$1, " | ");
          })();
          let path = (() => {
            let $1 = tag($dynamic.from(undefined));
            if (!$1.isOk() &&
            $1[0].atLeastLength(1) &&
            $1[0].head instanceof DecodeError) {
              let path = $1[0].head.path;
              return path;
            } else {
              return toList([]);
            }
          })();
          return new Error(
            toList([new DecodeError(tags, $string.inspect(kind), path)]),
          );
        }
      },
    );
  };
}

export function enum$(variants) {
  return tagged_union(
    $dynamic.string,
    $list.map(
      variants,
      (_capture) => {
        return $pair.map_second(
          _capture,
          (variant) => { return (_) => { return new Ok(variant); }; },
        );
      },
    ),
  );
}

export function bool_string(dynamic) {
  return enum$(
    toList([
      ["true", true],
      ["True", true],
      ["on", true],
      ["On", true],
      ["yes", true],
      ["Yes", true],
      ["false", false],
      ["False", false],
      ["off", false],
      ["Off", false],
      ["no", false],
      ["No", false],
    ]),
  )(dynamic);
}

export function iso_8601(dynamic) {
  return $result.try$(
    $dynamic.string(dynamic),
    (string) => {
      let $ = $birl.parse(string);
      if ($.isOk()) {
        let time = $[0];
        return new Ok(time);
      } else {
        return new Error(
          toList([
            new DecodeError("An ISO 8601 date string", string, toList([])),
          ]),
        );
      }
    },
  );
}

export function unix_timestamp(dynamic) {
  let _pipe = dynamic;
  let _pipe$1 = $dynamic.any(toList([$dynamic.int, int_string]))(_pipe);
  return $result.map(_pipe$1, $birl.from_unix);
}

export function http_date(dynamic) {
  return $result.try$(
    $dynamic.string(dynamic),
    (string) => {
      let $ = $birl.from_http(string);
      if ($.isOk()) {
        let time = $[0];
        return new Ok(time);
      } else {
        return new Error(
          toList([new DecodeError("An HTTP date string", string, toList([]))]),
        );
      }
    },
  );
}

export function uri(dynamic) {
  return $result.try$(
    $dynamic.string(dynamic),
    (string) => {
      let $ = $uri.parse(string);
      if ($.isOk()) {
        let uri$1 = $[0];
        return new Ok(uri$1);
      } else {
        return new Error(
          toList([new DecodeError("A valid Gleam URI", string, toList([]))]),
        );
      }
    },
  );
}

export function base16(dynamic) {
  return $result.try$(
    $dynamic.string(dynamic),
    (string) => {
      let $ = $bit_array.base16_decode(string);
      if ($.isOk()) {
        let bit_array = $[0];
        return new Ok(bit_array);
      } else {
        return new Error(
          toList([
            new DecodeError("A valid base16-encoded string", string, toList([])),
          ]),
        );
      }
    },
  );
}

export function base64(dynamic) {
  return $result.try$(
    $dynamic.string(dynamic),
    (string) => {
      let $ = $bit_array.base64_decode(string);
      if ($.isOk()) {
        let bit_array = $[0];
        return new Ok(bit_array);
      } else {
        return new Error(
          toList([
            new DecodeError("A valid base64-encoded string", string, toList([])),
          ]),
        );
      }
    },
  );
}

export function semver(dynamic) {
  return $result.try$(
    $dynamic.string(dynamic),
    (string) => {
      let $ = $version.parse(string);
      if ($.isOk()) {
        let version = $[0];
        return new Ok(version);
      } else if (!$.isOk() && $[0] instanceof $version.InvalidVersion) {
        return new Error(
          toList([
            new DecodeError("A valid semantic version", string, toList([])),
          ]),
        );
      } else if (!$.isOk() && $[0] instanceof $version.NegativeValue) {
        return new Error(
          toList([
            new DecodeError("A valid semantic version", string, toList([])),
          ]),
        );
      } else if (!$.isOk() && $[0] instanceof $version.InvalidMajorVersion) {
        return new Error(
          toList([
            new DecodeError(
              "A valid semantic version",
              "A semantic version with an invalid major version number",
              toList([]),
            ),
          ]),
        );
      } else if (!$.isOk() && $[0] instanceof $version.InvalidMinorVersion) {
        return new Error(
          toList([
            new DecodeError(
              "A valid semantic version",
              "A semantic version with an invalid minor version number",
              toList([]),
            ),
          ]),
        );
      } else if (!$.isOk() && $[0] instanceof $version.InvalidPatchVersion) {
        return new Error(
          toList([
            new DecodeError(
              "A valid semantic version",
              "A semantic version with an invalid patch version number",
              toList([]),
            ),
          ]),
        );
      } else if (!$.isOk() && $[0] instanceof $version.InvalidPrerelease) {
        return new Error(
          toList([
            new DecodeError(
              "A valid semantic version",
              "A semantic version with an invalid prerelease",
              toList([]),
            ),
          ]),
        );
      } else {
        return new Error(
          toList([
            new DecodeError(
              "A valid semantic version",
              "A semantic version with invalid build metadata",
              toList([]),
            ),
          ]),
        );
      }
    },
  );
}

export function base64_url_encoded(dynamic) {
  return $result.try$(
    $dynamic.string(dynamic),
    (string) => {
      let $ = $bit_array.base64_url_decode(string);
      if ($.isOk()) {
        let bit_array = $[0];
        return new Ok(bit_array);
      } else {
        return new Error(
          toList([
            new DecodeError(
              "A valid base64-url-encoded string",
              string,
              toList([]),
            ),
          ]),
        );
      }
    },
  );
}

export function when(decoder, predicate) {
  return (dynamic) => {
    return $result.try$(
      decoder(dynamic),
      (value) => {
        let $ = predicate(value);
        if ($) {
          return new Ok(value);
        } else {
          return new Error(
            toList([
              new DecodeError(
                "A value that satisfies the predicate",
                $string.inspect(value),
                toList([]),
              ),
            ]),
          );
        }
      },
    );
  };
}

export function json_string(decoder) {
  return (dynamic) => {
    return $result.try$(
      $dynamic.string(dynamic),
      (json) => {
        let $ = $json.decode(json, decoder);
        if ($.isOk()) {
          let a = $[0];
          return new Ok(a);
        } else if (!$.isOk() && $[0] instanceof $json.UnexpectedFormat) {
          let errors = $[0][0];
          return new Error(errors);
        } else {
          return new Error(
            toList([
              new DecodeError("A valid JSON-encoded string", json, toList([])),
            ]),
          );
        }
      },
    );
  };
}

export function keys(dynamic) {
  let _pipe = dynamic;
  let _pipe$1 = $dynamic.dict($dynamic.string, $dynamic.dynamic)(_pipe);
  return $result.map(_pipe$1, $dict.keys);
}

function check_exact_object(return$, expected, dynamic) {
  return $result.try$(
    keys(dynamic),
    (keys) => {
      let found_keys = $set.from_list(keys);
      let difference = $set.to_list($set.difference(found_keys, expected));
      let $ = $list.is_empty(difference);
      if ($) {
        return new Ok(return$);
      } else {
        let expected_keys = (() => {
          let _pipe = expected;
          let _pipe$1 = $set.to_list(_pipe);
          return $string.join(_pipe$1, ", ");
        })();
        let extra_keys = (() => {
          let _pipe = difference;
          return $string.join(_pipe, ", ");
        })();
        return new Error(
          toList([
            new DecodeError(
              "An object with exactly these keys: " + expected_keys,
              "An object with these extra keys: " + extra_keys,
              toList([]),
            ),
          ]),
        );
      }
    },
  );
}

export function exact_object1(constructor, field1) {
  let expected_keys = $set.from_list(toList([field1[0]]));
  return (dynamic) => {
    let _pipe = dynamic;
    let _pipe$1 = $dynamic.decode1(
      constructor,
      $dynamic.field(field1[0], field1[1]),
    )(_pipe);
    return $result.then$(
      _pipe$1,
      (_capture) => {
        return check_exact_object(_capture, expected_keys, dynamic);
      },
    );
  };
}

export function exact_object2(constructor, field1, field2) {
  let expected_keys = $set.from_list(toList([field1[0]]));
  return (dynamic) => {
    let _pipe = dynamic;
    let _pipe$1 = $dynamic.decode2(
      constructor,
      $dynamic.field(field1[0], field1[1]),
      $dynamic.field(field2[0], field2[1]),
    )(_pipe);
    return $result.then$(
      _pipe$1,
      (_capture) => {
        return check_exact_object(_capture, expected_keys, dynamic);
      },
    );
  };
}

export function exact_object3(constructor, field1, field2, field3) {
  let expected_keys = $set.from_list(toList([field1[0], field2[0], field3[0]]));
  return (dynamic) => {
    let _pipe = dynamic;
    let _pipe$1 = $dynamic.decode3(
      constructor,
      $dynamic.field(field1[0], field1[1]),
      $dynamic.field(field2[0], field2[1]),
      $dynamic.field(field3[0], field3[1]),
    )(_pipe);
    return $result.then$(
      _pipe$1,
      (_capture) => {
        return check_exact_object(_capture, expected_keys, dynamic);
      },
    );
  };
}

export function exact_object4(constructor, field1, field2, field3, field4) {
  let expected_keys = $set.from_list(
    toList([field1[0], field2[0], field3[0], field4[0]]),
  );
  return (dynamic) => {
    let _pipe = dynamic;
    let _pipe$1 = $dynamic.decode4(
      constructor,
      $dynamic.field(field1[0], field1[1]),
      $dynamic.field(field2[0], field2[1]),
      $dynamic.field(field3[0], field3[1]),
      $dynamic.field(field4[0], field4[1]),
    )(_pipe);
    return $result.then$(
      _pipe$1,
      (_capture) => {
        return check_exact_object(_capture, expected_keys, dynamic);
      },
    );
  };
}

export function exact_object5(
  constructor,
  field1,
  field2,
  field3,
  field4,
  field5
) {
  let expected_keys = $set.from_list(
    toList([field1[0], field2[0], field3[0], field4[0], field5[0]]),
  );
  return (dynamic) => {
    let _pipe = dynamic;
    let _pipe$1 = $dynamic.decode5(
      constructor,
      $dynamic.field(field1[0], field1[1]),
      $dynamic.field(field2[0], field2[1]),
      $dynamic.field(field3[0], field3[1]),
      $dynamic.field(field4[0], field4[1]),
      $dynamic.field(field5[0], field5[1]),
    )(_pipe);
    return $result.then$(
      _pipe$1,
      (_capture) => {
        return check_exact_object(_capture, expected_keys, dynamic);
      },
    );
  };
}

export function exact_object6(
  constructor,
  field1,
  field2,
  field3,
  field4,
  field5,
  field6
) {
  let expected_keys = $set.from_list(
    toList([field1[0], field2[0], field3[0], field4[0], field5[0], field6[0]]),
  );
  return (dynamic) => {
    let _pipe = dynamic;
    let _pipe$1 = $dynamic.decode6(
      constructor,
      $dynamic.field(field1[0], field1[1]),
      $dynamic.field(field2[0], field2[1]),
      $dynamic.field(field3[0], field3[1]),
      $dynamic.field(field4[0], field4[1]),
      $dynamic.field(field5[0], field5[1]),
      $dynamic.field(field6[0], field6[1]),
    )(_pipe);
    return $result.then$(
      _pipe$1,
      (_capture) => {
        return check_exact_object(_capture, expected_keys, dynamic);
      },
    );
  };
}

function index_list(idx, decoder) {
  return (dynamic) => {
    return $result.try$(
      $dynamic.list($dynamic.dynamic)(dynamic),
      (list) => {
        let $ = idx >= 0;
        if ($) {
          let _pipe = list;
          let _pipe$1 = $list.drop(_pipe, idx);
          let _pipe$2 = $list.first(_pipe$1);
          let _pipe$3 = $result.replace_error(
            _pipe$2,
            toList([
              new DecodeError(
                ("A list with at least" + $int.to_string(idx + 1)) + "elements",
                ("A list with" + $int.to_string($list.length(list))) + "elements",
                toList([$int.to_string(idx)]),
              ),
            ]),
          );
          return $result.then$(_pipe$3, decoder);
        } else {
          return new Error(
            toList([
              new DecodeError(
                "An 'index' decoder with a non-negative index",
                $int.to_string(idx),
                toList([]),
              ),
            ]),
          );
        }
      },
    );
  };
}

export function index(idx, decoder) {
  return $dynamic.any(
    toList([
      $dynamic.element(idx, decoder),
      $dynamic.field($int.to_string(idx), decoder),
      index_list(idx, decoder),
    ]),
  );
}

function do_at(path, decoder, dynamic) {
  if (path.hasLength(0)) {
    return decoder(dynamic);
  } else {
    let head = path.head;
    let rest = path.tail;
    let $ = $int.parse(head);
    if ($.isOk()) {
      let idx = $[0];
      let _pipe = dynamic;
      let _pipe$1 = index(idx, $dynamic.dynamic)(_pipe);
      return $result.then$(
        _pipe$1,
        (_capture) => { return do_at(rest, decoder, _capture); },
      );
    } else {
      let _pipe = dynamic;
      let _pipe$1 = $dynamic.field(head, $dynamic.dynamic)(_pipe);
      return $result.then$(
        _pipe$1,
        (_capture) => { return do_at(rest, decoder, _capture); },
      );
    }
  }
}

export function at(path, decoder) {
  return (dynamic) => { return do_at(path, decoder, dynamic); };
}
