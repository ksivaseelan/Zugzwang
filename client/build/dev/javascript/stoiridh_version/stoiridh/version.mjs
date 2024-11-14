import * as $bool from "../../gleam_stdlib/gleam/bool.mjs";
import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import * as $order from "../../gleam_stdlib/gleam/order.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
} from "../gleam.mjs";
import * as $sint from "../stoiridh/internal/int.mjs";

export class InvalidVersion extends $CustomType {}

export class InvalidMajorVersion extends $CustomType {}

export class InvalidMinorVersion extends $CustomType {}

export class InvalidPatchVersion extends $CustomType {}

export class InvalidPrerelease extends $CustomType {}

export class InvalidBuildMetadata extends $CustomType {}

export class NegativeValue extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Version extends $CustomType {
  constructor(major, minor, patch, prerelease, build_metadata) {
    super();
    this.major = major;
    this.minor = minor;
    this.patch = patch;
    this.prerelease = prerelease;
    this.build_metadata = build_metadata;
  }
}

class Parser extends $CustomType {
  constructor(input, version_parts) {
    super();
    this.input = input;
    this.version_parts = version_parts;
  }
}

export function new$(major, minor, patch) {
  return $result.try$(
    (() => {
      let _pipe = major;
      let _pipe$1 = $sint.is_positive(_pipe);
      return $result.map_error(
        _pipe$1,
        (error) => { return new NegativeValue(error); },
      );
    })(),
    (major) => {
      return $result.try$(
        (() => {
          let _pipe = minor;
          let _pipe$1 = $sint.is_positive(_pipe);
          return $result.map_error(
            _pipe$1,
            (error) => { return new NegativeValue(error); },
          );
        })(),
        (minor) => {
          return $result.try$(
            (() => {
              let _pipe = patch;
              let _pipe$1 = $sint.is_positive(_pipe);
              return $result.map_error(
                _pipe$1,
                (error) => { return new NegativeValue(error); },
              );
            })(),
            (patch) => {
              return new Ok(
                new Version(major, minor, patch, new None(), new None()),
              );
            },
          );
        },
      );
    },
  );
}

function check_version_parts(version_parts) {
  if (version_parts.hasLength(3)) {
    let major$1 = version_parts.head;
    let minor$1 = version_parts.tail.head;
    let patch$1 = version_parts.tail.tail.head;
    return new Ok([major$1, minor$1, patch$1]);
  } else if (version_parts.hasLength(2)) {
    return new Error(new InvalidPatchVersion());
  } else if (version_parts.hasLength(1)) {
    return new Error(new InvalidMinorVersion());
  } else if (version_parts.hasLength(0)) {
    return new Error(new InvalidMajorVersion());
  } else {
    return new Error(new InvalidVersion());
  }
}

function do_parse_version_numbers(loop$input, loop$buffer, loop$version_parts) {
  while (true) {
    let input = loop$input;
    let buffer = loop$buffer;
    let version_parts = loop$version_parts;
    if (input.hasLength(1)) {
      let elem = input.head;
      let version_number = buffer + elem;
      return $bool.guard(
        $string.starts_with(version_number, "00"),
        new Error(new InvalidVersion()),
        () => {
          let $ = $int.parse(version_number);
          if ($.isOk()) {
            let version_part = $[0];
            return do_parse_version_numbers(
              toList([]),
              "",
              (() => {
                let _pipe = version_parts;
                return $list.append(_pipe, toList([version_part]));
              })(),
            );
          } else {
            return new Error(new InvalidVersion());
          }
        },
      );
    } else if (input.atLeastLength(2) &&
    ((input.tail.head === "-") || (input.tail.head === "+"))) {
      let elem = input.head;
      let delimiter = input.tail.head;
      let remaining_graphemes = input.tail.tail;
      let $ = $int.parse(buffer + elem);
      if ($.isOk()) {
        let version_part = $[0];
        let version_parts$1 = (() => {
          let _pipe = version_parts;
          return $list.append(_pipe, toList([version_part]));
        })();
        let _pipe = version_parts$1;
        let _pipe$1 = check_version_parts(_pipe);
        return $result.map(
          _pipe$1,
          (version_parts) => {
            let major$1 = version_parts[0];
            let minor$1 = version_parts[1];
            let patch$1 = version_parts[2];
            return new Parser(
              listPrepend(delimiter, remaining_graphemes),
              (() => {
                let _pipe$2 = $dict.new$();
                let _pipe$3 = $dict.insert(
                  _pipe$2,
                  "major",
                  $int.to_string(major$1),
                );
                let _pipe$4 = $dict.insert(
                  _pipe$3,
                  "minor",
                  $int.to_string(minor$1),
                );
                return $dict.insert(_pipe$4, "patch", $int.to_string(patch$1));
              })(),
            );
          },
        );
      } else {
        return new Error(new InvalidVersion());
      }
    } else if (input.atLeastLength(2) && input.tail.head === ".") {
      let elem = input.head;
      let remaining_graphemes = input.tail.tail;
      let version_number = buffer + elem;
      return $bool.guard(
        $string.starts_with(version_number, "00"),
        new Error(new InvalidVersion()),
        () => {
          let $ = $int.parse(version_number);
          if ($.isOk()) {
            let version_part = $[0];
            return do_parse_version_numbers(
              remaining_graphemes,
              "",
              (() => {
                let _pipe = version_parts;
                return $list.append(_pipe, toList([version_part]));
              })(),
            );
          } else {
            return new Error(new InvalidVersion());
          }
        },
      );
    } else if (input.atLeastLength(1) && input.head === ".") {
      let remaining_graphemes = input.tail;
      loop$input = remaining_graphemes;
      loop$buffer = buffer;
      loop$version_parts = version_parts;
    } else if (input.atLeastLength(1)) {
      let elem = input.head;
      let remaining_graphemes = input.tail;
      let $ = $int.parse(elem);
      if ($.isOk()) {
        loop$input = remaining_graphemes;
        loop$buffer = buffer + elem;
        loop$version_parts = version_parts;
      } else {
        return new Error(new InvalidVersion());
      }
    } else {
      let _pipe = version_parts;
      let _pipe$1 = check_version_parts(_pipe);
      return $result.map(
        _pipe$1,
        (version_parts) => {
          let major$1 = version_parts[0];
          let minor$1 = version_parts[1];
          let patch$1 = version_parts[2];
          return new Parser(
            toList([]),
            (() => {
              let _pipe$2 = $dict.new$();
              let _pipe$3 = $dict.insert(
                _pipe$2,
                "major",
                $int.to_string(major$1),
              );
              let _pipe$4 = $dict.insert(
                _pipe$3,
                "minor",
                $int.to_string(minor$1),
              );
              return $dict.insert(_pipe$4, "patch", $int.to_string(patch$1));
            })(),
          );
        },
      );
    }
  }
}

function parse_version_numbers(input) {
  return do_parse_version_numbers(input, "", toList([]));
}

function transform_to_version(input) {
  return $result.try$(
    (() => {
      let _pipe = input;
      let _pipe$1 = $dict.get(_pipe, "major");
      let _pipe$2 = $result.map_error(
        _pipe$1,
        (_) => { return new InvalidMajorVersion(); },
      );
      return $result.then$(
        _pipe$2,
        (major) => {
          let $ = $int.parse(major);
          if ($.isOk()) {
            let major$1 = $[0];
            return new Ok(major$1);
          } else {
            return new Error(new InvalidMajorVersion());
          }
        },
      );
    })(),
    (major) => {
      return $result.try$(
        (() => {
          let _pipe = input;
          let _pipe$1 = $dict.get(_pipe, "minor");
          let _pipe$2 = $result.map_error(
            _pipe$1,
            (_) => { return new InvalidMinorVersion(); },
          );
          return $result.then$(
            _pipe$2,
            (minor) => {
              let $ = $int.parse(minor);
              if ($.isOk()) {
                let minor$1 = $[0];
                return new Ok(minor$1);
              } else {
                return new Error(new InvalidMinorVersion());
              }
            },
          );
        })(),
        (minor) => {
          return $result.try$(
            (() => {
              let _pipe = input;
              let _pipe$1 = $dict.get(_pipe, "patch");
              let _pipe$2 = $result.map_error(
                _pipe$1,
                (_) => { return new InvalidPatchVersion(); },
              );
              return $result.then$(
                _pipe$2,
                (patch) => {
                  let $ = $int.parse(patch);
                  if ($.isOk()) {
                    let patch$1 = $[0];
                    return new Ok(patch$1);
                  } else {
                    return new Error(new InvalidPatchVersion());
                  }
                },
              );
            })(),
            (patch) => {
              let prerelease$1 = (() => {
                let _pipe = input;
                let _pipe$1 = $dict.get(_pipe, "prerelease");
                return $option.from_result(_pipe$1);
              })();
              let build_metadata$1 = (() => {
                let _pipe = input;
                let _pipe$1 = $dict.get(_pipe, "build_metadata");
                return $option.from_result(_pipe$1);
              })();
              return new Ok(
                new Version(major, minor, patch, prerelease$1, build_metadata$1),
              );
            },
          );
        },
      );
    },
  );
}

function validate_identifiers(identifiers, predicate) {
  let $ = (() => {
    let _pipe = identifiers;
    return $list.all(_pipe, predicate);
  })();
  if ($) {
    return new Ok(new Some($string.join(identifiers, ".")));
  } else {
    return new Error(new InvalidPrerelease());
  }
}

function is_valid_identifier(identifier) {
  let _pipe = identifier;
  let _pipe$1 = $string.to_graphemes(_pipe);
  return $list.all(
    _pipe$1,
    (c) => {
      let c$1 = (() => {
        let $ = $string.to_utf_codepoints(c);
        if (!$.atLeastLength(1)) {
          throw makeError(
            "let_assert",
            "stoiridh/version",
            598,
            "",
            "Pattern match failed, no pattern matched the value.",
            { value: $ }
          )
        }
        let char = $.head;
        let _pipe$2 = char;
        return $string.utf_codepoint_to_int(_pipe$2);
      })();
      return (((c$1 === 45) || ((c$1 >= 48) && (c$1 <= 57))) || ((c$1 >= 65) && (c$1 <= 90))) || ((c$1 >= 97) && (c$1 <= 122));
    },
  );
}

function parse_build_metadata(parser) {
  let $ = $list.first(parser.input);
  if ($.isOk() && $[0] === "+") {
    let $1 = $list.rest(parser.input);
    if ($1.isOk()) {
      let input = $1[0];
      return $result.try$(
        (() => {
          let _pipe = input;
          let _pipe$1 = $string.join(_pipe, "");
          let _pipe$2 = $string.split(_pipe$1, ".");
          let _pipe$3 = validate_identifiers(
            _pipe$2,
            (i) => { return !$string.is_empty(i) && is_valid_identifier(i); },
          );
          return $result.replace_error(_pipe$3, new InvalidBuildMetadata());
        })(),
        (build_metadata) => {
          if (build_metadata instanceof Some) {
            let bm = build_metadata[0];
            return new Ok(
              (() => {
                let _pipe = parser.version_parts;
                return $dict.insert(_pipe, "build_metadata", bm);
              })(),
            );
          } else {
            return new Error(new InvalidBuildMetadata());
          }
        },
      );
    } else {
      return new Error(new InvalidBuildMetadata());
    }
  } else {
    return new Ok(parser.version_parts);
  }
}

export function with_build_metadata(version, build_metadata) {
  return $result.try$(
    (() => {
      let _pipe = build_metadata;
      let _pipe$1 = $string.split(_pipe, ".");
      let _pipe$2 = validate_identifiers(
        _pipe$1,
        (i) => { return !$string.is_empty(i) && is_valid_identifier(i); },
      );
      return $result.replace_error(_pipe$2, new InvalidBuildMetadata());
    })(),
    (build_metadata) => {
      let _pipe = version;
      return $result.map(
        _pipe,
        (v) => { return v.withFields({ build_metadata: build_metadata }); },
      );
    },
  );
}

function has_leading_zeros(identifier) {
  let _pipe = identifier;
  return $string.starts_with(_pipe, "00");
}

function do_parse_prerelease(loop$parser, loop$input, loop$buffer) {
  while (true) {
    let parser = loop$parser;
    let input = loop$input;
    let buffer = loop$buffer;
    if (input.atLeastLength(1) && input.head === "+") {
      let remaining_graphemes = input.tail;
      return $result.try$(
        (() => {
          let _pipe = buffer;
          let _pipe$1 = $string.split(_pipe, ".");
          let _pipe$2 = validate_identifiers(
            _pipe$1,
            (i) => {
              return !($string.is_empty(i) || has_leading_zeros(i)) && is_valid_identifier(
                i,
              );
            },
          );
          return $result.replace_error(_pipe$2, new InvalidPrerelease());
        })(),
        (prerelease) => {
          if (prerelease instanceof Some) {
            let p = prerelease[0];
            return new Ok(
              new Parser(
                listPrepend("+", remaining_graphemes),
                (() => {
                  let _pipe = parser.version_parts;
                  return $dict.insert(_pipe, "prerelease", p);
                })(),
              ),
            );
          } else {
            return new Error(new InvalidPrerelease());
          }
        },
      );
    } else if (input.atLeastLength(1)) {
      let elem = input.head;
      let remaining_graphemes = input.tail;
      loop$parser = parser;
      loop$input = remaining_graphemes;
      loop$buffer = buffer + elem;
    } else {
      return $result.try$(
        (() => {
          let _pipe = buffer;
          let _pipe$1 = $string.split(_pipe, ".");
          let _pipe$2 = validate_identifiers(
            _pipe$1,
            (i) => {
              return !($string.is_empty(i) || has_leading_zeros(i)) && is_valid_identifier(
                i,
              );
            },
          );
          return $result.replace_error(_pipe$2, new InvalidPrerelease());
        })(),
        (prerelease) => {
          if (prerelease instanceof Some) {
            let p = prerelease[0];
            return new Ok(
              new Parser(
                toList([]),
                (() => {
                  let _pipe = parser.version_parts;
                  return $dict.insert(_pipe, "prerelease", p);
                })(),
              ),
            );
          } else {
            return new Error(new InvalidPrerelease());
          }
        },
      );
    }
  }
}

function parse_prerelease(parser) {
  let $ = $list.first(parser.input);
  if ($.isOk() && $[0] === "-") {
    let $1 = $list.rest(parser.input);
    if ($1.isOk()) {
      let input = $1[0];
      return do_parse_prerelease(parser, input, "");
    } else {
      return new Error(new InvalidPrerelease());
    }
  } else {
    return new Ok(parser);
  }
}

export function parse(input) {
  let graphemes = (() => {
    let _pipe = input;
    return $string.to_graphemes(_pipe);
  })();
  return $bool.guard(
    $list.is_empty(graphemes),
    new Error(new InvalidVersion()),
    () => {
      let _pipe = graphemes;
      let _pipe$1 = parse_version_numbers(_pipe);
      let _pipe$2 = $result.then$(_pipe$1, parse_prerelease);
      let _pipe$3 = $result.then$(_pipe$2, parse_build_metadata);
      return $result.then$(_pipe$3, transform_to_version);
    },
  );
}

export function with_prerelease(version, prerelease) {
  return $result.try$(
    (() => {
      let _pipe = prerelease;
      let _pipe$1 = $string.split(_pipe, ".");
      return validate_identifiers(
        _pipe$1,
        (i) => {
          return !($string.is_empty(i) || has_leading_zeros(i)) && is_valid_identifier(
            i,
          );
        },
      );
    })(),
    (prerelease) => {
      let _pipe = version;
      return $result.map(
        _pipe,
        (v) => { return v.withFields({ prerelease: prerelease }); },
      );
    },
  );
}

export function major(version) {
  return version.major;
}

export function minor(version) {
  return version.minor;
}

export function patch(version) {
  return version.patch;
}

export function prerelease(version) {
  return version.prerelease;
}

export function build_metadata(version) {
  return version.build_metadata;
}

export function to_string(version) {
  let v = ((($int.to_string(version.major) + ".") + $int.to_string(
    version.minor,
  )) + ".") + $int.to_string(version.patch);
  let p = (() => {
    let _pipe = version;
    let _pipe$1 = prerelease(_pipe);
    let _pipe$2 = $option.then$(
      _pipe$1,
      (p) => { return new Some($string.concat(toList(["-", p]))); },
    );
    return $option.unwrap(_pipe$2, "");
  })();
  let b = (() => {
    let _pipe = version;
    let _pipe$1 = build_metadata(_pipe);
    let _pipe$2 = $option.then$(
      _pipe$1,
      (b) => { return new Some($string.concat(toList(["+", b]))); },
    );
    return $option.unwrap(_pipe$2, "");
  })();
  return $string.concat(toList([v, p, b]));
}

function equal_to(a, b) {
  return ((a.major === b.major) && (a.minor === b.minor)) && (a.patch === b.patch);
}

function less(a, b) {
  return ((a.major < b.major) || ((a.major === b.major) && (a.minor < b.minor))) || (((a.major === b.major) && (a.minor === b.minor)) && (a.patch < b.patch));
}

function compare_identifiers(a, b) {
  let left = $int.parse(a);
  let right = $int.parse(b);
  if (left.isOk() && right.isOk()) {
    let l = left[0];
    let r = right[0];
    return $int.compare(l, r);
  } else if (left.isOk() && !right.isOk()) {
    return new $order.Lt();
  } else if (!left.isOk() && right.isOk()) {
    return new $order.Gt();
  } else {
    return $string.compare(a, b);
  }
}

function do_compare_prerelease(loop$a, loop$b) {
  while (true) {
    let a = loop$a;
    let b = loop$b;
    if (a.hasLength(1) && b.hasLength(1)) {
      let a$1 = a.head;
      let b$1 = b.head;
      return compare_identifiers(a$1, b$1);
    } else if (a.atLeastLength(1) && b.atLeastLength(1)) {
      let a$1 = a.head;
      let arest = a.tail;
      let b$1 = b.head;
      let brest = b.tail;
      let $ = compare_identifiers(a$1, b$1);
      if ($ instanceof $order.Eq) {
        loop$a = arest;
        loop$b = brest;
      } else if ($ instanceof $order.Gt) {
        return new $order.Gt();
      } else {
        return new $order.Lt();
      }
    } else if (a.hasLength(1) && b.hasLength(0)) {
      return new $order.Gt();
    } else if (a.atLeastLength(1) && b.hasLength(0)) {
      return new $order.Gt();
    } else if (a.hasLength(0) && b.hasLength(1)) {
      return new $order.Lt();
    } else if (a.hasLength(0) && b.atLeastLength(1)) {
      return new $order.Lt();
    } else {
      return new $order.Eq();
    }
  }
}

function compare_prerelease(a, b) {
  let $ = $option.is_some(a.prerelease);
  let $1 = $option.is_some(b.prerelease);
  if (!$ && !$1) {
    return new $order.Eq();
  } else if (!$ && $1) {
    return new $order.Gt();
  } else if ($ && !$1) {
    return new $order.Lt();
  } else {
    let a_prerelease = (() => {
      let _pipe = a.prerelease;
      let _pipe$1 = $option.unwrap(_pipe, "");
      return $string.split(_pipe$1, ".");
    })();
    let b_prerelease = (() => {
      let _pipe = b.prerelease;
      let _pipe$1 = $option.unwrap(_pipe, "");
      return $string.split(_pipe$1, ".");
    })();
    return do_compare_prerelease(a_prerelease, b_prerelease);
  }
}

export function compare(a, b) {
  let $ = equal_to(a, b);
  if ($) {
    return compare_prerelease(a, b);
  } else {
    let $1 = less(a, b);
    if ($1) {
      return new $order.Lt();
    } else {
      return new $order.Gt();
    }
  }
}
