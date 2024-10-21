import * as $bool from "../../../gleam_stdlib/gleam/bool.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $io from "../../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $order from "../../../gleam_stdlib/gleam/order.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, toList, CustomType as $CustomType, isEqual } from "../../gleam.mjs";
import * as $cursor from "../../stoiridh/internal/cursor.mjs";
import { Cursor } from "../../stoiridh/internal/cursor.mjs";
import * as $types from "../../stoiridh/internal/types.mjs";
import { Major, Minor, Partial, Patch, Strict, Wildcard } from "../../stoiridh/internal/types.mjs";
import * as $version from "../../stoiridh/version.mjs";

class Constraint extends $CustomType {
  constructor(constraints) {
    super();
    this.constraints = constraints;
  }
}

export class InvalidConstraint extends $CustomType {}

class Equal extends $CustomType {}

class LessThan extends $CustomType {}

class LessThanOrEqualTo extends $CustomType {}

class GreaterThan extends $CustomType {}

class GreaterThanOrEqualTo extends $CustomType {}

class Caret extends $CustomType {}

class Tilde extends $CustomType {}

export function check(constraint, version) {
  let _pipe = constraint.constraints;
  return $list.all(
    _pipe,
    (c) => {
      if (c[0] instanceof Caret) {
        let v = c[1];
        $io.println_error(
          ((("^" + $version.to_string(v)) + " was not interpreted correctly. Please, open an issue at") + " https://gitlab.com/stoiridh-project/stoiridh-version/-/issues") + " with a reproductible example.",
        );
        return false;
      } else if (c[0] instanceof Tilde) {
        let v = c[1];
        $io.println_error(
          ((("~" + $version.to_string(v)) + " was not interpreted correctly. Please, open an issue at") + " https://gitlab.com/stoiridh-project/stoiridh-version/-/issues") + " with a reproductible example.",
        );
        return false;
      } else if (c[0] instanceof Equal) {
        let v = c[1];
        return isEqual($version.compare(version, v), new $order.Eq());
      } else if (c[0] instanceof LessThan) {
        let v = c[1];
        return isEqual($version.compare(version, v), new $order.Lt());
      } else if (c[0] instanceof LessThanOrEqualTo) {
        let v = c[1];
        let $ = $version.compare(version, v);
        if ($ instanceof $order.Lt) {
          return true;
        } else if ($ instanceof $order.Eq) {
          return true;
        } else {
          return false;
        }
      } else if (c[0] instanceof GreaterThan) {
        let v = c[1];
        return isEqual($version.compare(version, v), new $order.Gt());
      } else {
        let v = c[1];
        let $ = $version.compare(version, v);
        if ($ instanceof $order.Gt) {
          return true;
        } else if ($ instanceof $order.Eq) {
          return true;
        } else {
          return false;
        }
      }
    },
  );
}

function transform_to_version_constraint(state) {
  let cursor = state[0];
  let result = state[1];
  let version = state[2];
  if (result instanceof Strict) {
    if (cursor instanceof Cursor && cursor.version_part instanceof Major) {
      return $result.try$(
        (() => {
          let _pipe = $version.parse(version + ".0.0");
          return $result.map_error(
            _pipe,
            (_) => { return new InvalidConstraint(); },
          );
        })(),
        (version) => { return new Ok([new Partial(new Major()), version]); },
      );
    } else if (cursor instanceof Cursor && cursor.version_part instanceof Minor) {
      return $result.try$(
        (() => {
          let _pipe = $version.parse(version + ".0");
          return $result.map_error(
            _pipe,
            (_) => { return new InvalidConstraint(); },
          );
        })(),
        (version) => { return new Ok([new Partial(new Minor()), version]); },
      );
    } else {
      return $result.try$(
        (() => {
          let _pipe = $version.parse(version);
          return $result.map_error(
            _pipe,
            (_) => { return new InvalidConstraint(); },
          );
        })(),
        (version) => { return new Ok([new Strict(), version]); },
      );
    }
  } else if (result instanceof Wildcard && result.version_part instanceof Patch) {
    return $result.try$(
      (() => {
        let _pipe = $version.parse(version);
        return $result.map_error(
          _pipe,
          (_) => { return new InvalidConstraint(); },
        );
      })(),
      (version) => { return new Ok([new Wildcard(new Patch()), version]); },
    );
  } else if (result instanceof Wildcard && result.version_part instanceof Minor) {
    return $result.try$(
      (() => {
        let _pipe = $version.parse(version + ".0");
        return $result.map_error(
          _pipe,
          (_) => { return new InvalidConstraint(); },
        );
      })(),
      (version) => { return new Ok([new Wildcard(new Minor()), version]); },
    );
  } else if (result instanceof Wildcard && result.version_part instanceof Major) {
    return $result.try$(
      (() => {
        let _pipe = $version.parse(version + ".0.0");
        return $result.map_error(
          _pipe,
          (_) => { return new InvalidConstraint(); },
        );
      })(),
      (version) => { return new Ok([new Wildcard(new Major()), version]); },
    );
  } else if (result instanceof Partial && result.version_part instanceof Patch) {
    return new Error(new InvalidConstraint());
  } else if (result instanceof Partial && result.version_part instanceof Minor) {
    return new Error(new InvalidConstraint());
  } else {
    return new Error(new InvalidConstraint());
  }
}

function parse_loosely(constraint) {
  let _pipe = constraint;
  let _pipe$1 = $string.trim(_pipe);
  let _pipe$2 = $string.to_graphemes(_pipe$1);
  let _pipe$3 = $list.try_fold(
    _pipe$2,
    [$cursor.new$(), new Strict(), ""],
    (state, current_char) => {
      let cursor = state[0];
      let result = state[1];
      let version = state[2];
      if (current_char === ".") {
        return new Ok([$cursor.next(cursor), result, version + "."]);
      } else if (current_char === "*") {
        return $bool.guard(
          !($string.is_empty(version) || $string.ends_with(version, ".")),
          new Error(new InvalidConstraint()),
          () => {
            let new_result = (() => {
              if (cursor instanceof Cursor &&
              cursor.version_part instanceof Major) {
                return new Wildcard(new Major());
              } else if (cursor instanceof Cursor &&
              cursor.version_part instanceof Minor) {
                return new Wildcard(new Minor());
              } else {
                return new Wildcard(new Patch());
              }
            })();
            return new Ok([cursor, new_result, version + "0"]);
          },
        );
      } else if (current_char === "x") {
        return $bool.guard(
          !($string.is_empty(version) || $string.ends_with(version, ".")),
          new Error(new InvalidConstraint()),
          () => {
            let new_result = (() => {
              if (cursor instanceof Cursor &&
              cursor.version_part instanceof Major) {
                return new Wildcard(new Major());
              } else if (cursor instanceof Cursor &&
              cursor.version_part instanceof Minor) {
                return new Wildcard(new Minor());
              } else {
                return new Wildcard(new Patch());
              }
            })();
            return new Ok([cursor, new_result, version + "0"]);
          },
        );
      } else if (current_char === "X") {
        return $bool.guard(
          !($string.is_empty(version) || $string.ends_with(version, ".")),
          new Error(new InvalidConstraint()),
          () => {
            let new_result = (() => {
              if (cursor instanceof Cursor &&
              cursor.version_part instanceof Major) {
                return new Wildcard(new Major());
              } else if (cursor instanceof Cursor &&
              cursor.version_part instanceof Minor) {
                return new Wildcard(new Minor());
              } else {
                return new Wildcard(new Patch());
              }
            })();
            return new Ok([cursor, new_result, version + "0"]);
          },
        );
      } else {
        let c = current_char;
        let $ = $int.parse(c);
        if ($.isOk()) {
          return new Ok([cursor, result, version + c]);
        } else {
          return new Error(new InvalidConstraint());
        }
      }
    },
  );
  return $result.then$(_pipe$3, transform_to_version_constraint);
}

function next_minor_version(version) {
  return $version.new$($version.major(version), $version.minor(version) + 1, 0);
}

function next_major_version(version) {
  return $version.new$($version.major(version) + 1, 0, 0);
}

function parse_version_constraint(operator, constraint) {
  return $result.try$(
    (() => {
      let _pipe = constraint;
      let _pipe$1 = parse_loosely(_pipe);
      return $result.then$(
        _pipe$1,
        (state) => {
          let constraint_result = state[0];
          let version = state[1];
          if (constraint_result instanceof Strict) {
            return new Ok(toList([[operator, version]]));
          } else if (constraint_result instanceof Wildcard &&
          constraint_result.version_part instanceof Major) {
            return new Error(new InvalidConstraint());
          } else if (constraint_result instanceof Partial &&
          constraint_result.version_part instanceof Patch) {
            if (operator instanceof LessThanOrEqualTo) {
              return $result.try$(
                (() => {
                  let _pipe$2 = next_minor_version(version);
                  return $result.map_error(
                    _pipe$2,
                    (_) => { return new InvalidConstraint(); },
                  );
                })(),
                (nmv) => { return new Ok(toList([[new LessThan(), nmv]])); },
              );
            } else {
              return new Ok(toList([[operator, version]]));
            }
          } else if (constraint_result instanceof Wildcard &&
          constraint_result.version_part instanceof Patch) {
            if (operator instanceof LessThanOrEqualTo) {
              return $result.try$(
                (() => {
                  let _pipe$2 = next_minor_version(version);
                  return $result.map_error(
                    _pipe$2,
                    (_) => { return new InvalidConstraint(); },
                  );
                })(),
                (nmv) => { return new Ok(toList([[new LessThan(), nmv]])); },
              );
            } else {
              return new Ok(toList([[operator, version]]));
            }
          } else if (constraint_result instanceof Partial &&
          constraint_result.version_part instanceof Minor) {
            if (operator instanceof GreaterThan) {
              return $result.try$(
                (() => {
                  let _pipe$2 = next_minor_version(version);
                  return $result.map_error(
                    _pipe$2,
                    (_) => { return new InvalidConstraint(); },
                  );
                })(),
                (nmv) => {
                  return new Ok(toList([[new GreaterThanOrEqualTo(), nmv]]));
                },
              );
            } else if (operator instanceof LessThanOrEqualTo) {
              return $result.try$(
                (() => {
                  let _pipe$2 = next_minor_version(version);
                  return $result.map_error(
                    _pipe$2,
                    (_) => { return new InvalidConstraint(); },
                  );
                })(),
                (nmv) => { return new Ok(toList([[new LessThan(), nmv]])); },
              );
            } else {
              return new Ok(toList([[operator, version]]));
            }
          } else if (constraint_result instanceof Partial &&
          constraint_result.version_part instanceof Major) {
            if (operator instanceof GreaterThan) {
              return $result.try$(
                (() => {
                  let _pipe$2 = next_major_version(version);
                  return $result.map_error(
                    _pipe$2,
                    (_) => { return new InvalidConstraint(); },
                  );
                })(),
                (nmv) => {
                  return new Ok(toList([[new GreaterThanOrEqualTo(), nmv]]));
                },
              );
            } else if (operator instanceof LessThanOrEqualTo) {
              return $result.try$(
                (() => {
                  let _pipe$2 = next_major_version(version);
                  return $result.map_error(
                    _pipe$2,
                    (_) => { return new InvalidConstraint(); },
                  );
                })(),
                (nmv) => { return new Ok(toList([[new LessThan(), nmv]])); },
              );
            } else {
              return new Ok(toList([[operator, version]]));
            }
          } else {
            if (operator instanceof GreaterThan) {
              return $result.try$(
                (() => {
                  let _pipe$2 = next_major_version(version);
                  return $result.map_error(
                    _pipe$2,
                    (_) => { return new InvalidConstraint(); },
                  );
                })(),
                (nmv) => {
                  return new Ok(toList([[new GreaterThanOrEqualTo(), nmv]]));
                },
              );
            } else if (operator instanceof LessThanOrEqualTo) {
              return $result.try$(
                (() => {
                  let _pipe$2 = next_major_version(version);
                  return $result.map_error(
                    _pipe$2,
                    (_) => { return new InvalidConstraint(); },
                  );
                })(),
                (nmv) => { return new Ok(toList([[new LessThan(), nmv]])); },
              );
            } else {
              return new Ok(toList([[operator, version]]));
            }
          }
        },
      );
    })(),
    (constraints) => { return new Ok(constraints); },
  );
}

function parse_equality_version_constraint(constraint) {
  return $result.try$(
    (() => {
      let _pipe = constraint;
      let _pipe$1 = parse_loosely(_pipe);
      return $result.then$(
        _pipe$1,
        (state) => {
          let constraint_result = state[0];
          let version = state[1];
          if (constraint_result instanceof Strict) {
            return new Ok(toList([[new Equal(), version]]));
          } else if (constraint_result instanceof Wildcard &&
          constraint_result.version_part instanceof Patch) {
            return $result.try$(
              (() => {
                let _pipe$2 = next_minor_version(version);
                return $result.map_error(
                  _pipe$2,
                  (_) => { return new InvalidConstraint(); },
                );
              })(),
              (nmv) => {
                return new Ok(
                  toList([
                    [new GreaterThanOrEqualTo(), version],
                    [new LessThan(), nmv],
                  ]),
                );
              },
            );
          } else if (constraint_result instanceof Partial &&
          constraint_result.version_part instanceof Patch) {
            return $result.try$(
              (() => {
                let _pipe$2 = next_minor_version(version);
                return $result.map_error(
                  _pipe$2,
                  (_) => { return new InvalidConstraint(); },
                );
              })(),
              (nmv) => {
                return new Ok(
                  toList([
                    [new GreaterThanOrEqualTo(), version],
                    [new LessThan(), nmv],
                  ]),
                );
              },
            );
          } else if (constraint_result instanceof Wildcard &&
          constraint_result.version_part instanceof Minor) {
            return $result.try$(
              (() => {
                let _pipe$2 = next_major_version(version);
                return $result.map_error(
                  _pipe$2,
                  (_) => { return new InvalidConstraint(); },
                );
              })(),
              (nmv) => {
                return new Ok(
                  toList([
                    [new GreaterThanOrEqualTo(), version],
                    [new LessThan(), nmv],
                  ]),
                );
              },
            );
          } else if (constraint_result instanceof Partial &&
          constraint_result.version_part instanceof Minor) {
            return $result.try$(
              (() => {
                let _pipe$2 = next_minor_version(version);
                return $result.map_error(
                  _pipe$2,
                  (_) => { return new InvalidConstraint(); },
                );
              })(),
              (nmv) => {
                return new Ok(
                  toList([
                    [new GreaterThanOrEqualTo(), version],
                    [new LessThan(), nmv],
                  ]),
                );
              },
            );
          } else if (constraint_result instanceof Wildcard &&
          constraint_result.version_part instanceof Major) {
            let $ = $version.to_string(version);
            if ($ === "0.0.0") {
              return new Ok(toList([[new GreaterThanOrEqualTo(), version]]));
            } else {
              return $result.try$(
                (() => {
                  let _pipe$2 = next_major_version(version);
                  return $result.map_error(
                    _pipe$2,
                    (_) => { return new InvalidConstraint(); },
                  );
                })(),
                (nmv) => {
                  return new Ok(
                    toList([
                      [new GreaterThanOrEqualTo(), version],
                      [new LessThan(), nmv],
                    ]),
                  );
                },
              );
            }
          } else {
            return $result.try$(
              (() => {
                let _pipe$2 = next_major_version(version);
                return $result.map_error(
                  _pipe$2,
                  (_) => { return new InvalidConstraint(); },
                );
              })(),
              (nmv) => {
                return new Ok(
                  toList([
                    [new GreaterThanOrEqualTo(), version],
                    [new LessThan(), nmv],
                  ]),
                );
              },
            );
          }
        },
      );
    })(),
    (constraints) => { return new Ok(constraints); },
  );
}

function parse_caret_version_constraint(constraint) {
  return $result.try$(
    (() => {
      let _pipe = constraint;
      let _pipe$1 = parse_loosely(_pipe);
      return $result.then$(
        _pipe$1,
        (state) => {
          let constraint_result = state[0];
          let version = state[1];
          if (constraint_result instanceof Strict) {
            let major = (() => {
              let _pipe$2 = version;
              return $version.major(_pipe$2);
            })();
            let minor = (() => {
              let _pipe$2 = version;
              return $version.minor(_pipe$2);
            })();
            if (major === 0 && minor === 0) {
              return new Ok(toList([[new Equal(), version]]));
            } else if (major === 0) {
              return $result.try$(
                (() => {
                  let _pipe$2 = version;
                  let _pipe$3 = next_minor_version(_pipe$2);
                  return $result.map_error(
                    _pipe$3,
                    (_) => { return new InvalidConstraint(); },
                  );
                })(),
                (nmv) => {
                  return new Ok(
                    toList([
                      [new GreaterThanOrEqualTo(), version],
                      [new LessThan(), nmv],
                    ]),
                  );
                },
              );
            } else {
              return $result.try$(
                (() => {
                  let _pipe$2 = version;
                  let _pipe$3 = next_major_version(_pipe$2);
                  return $result.map_error(
                    _pipe$3,
                    (_) => { return new InvalidConstraint(); },
                  );
                })(),
                (nmv) => {
                  return new Ok(
                    toList([
                      [new GreaterThanOrEqualTo(), version],
                      [new LessThan(), nmv],
                    ]),
                  );
                },
              );
            }
          } else if (constraint_result instanceof Partial &&
          constraint_result.version_part instanceof Minor) {
            let major = (() => {
              let _pipe$2 = version;
              return $version.major(_pipe$2);
            })();
            return $result.try$(
              (() => {
                if (major === 0) {
                  return $result.map(
                    (() => {
                      let _pipe$2 = version;
                      let _pipe$3 = next_minor_version(_pipe$2);
                      return $result.map_error(
                        _pipe$3,
                        (_) => { return new InvalidConstraint(); },
                      );
                    })(),
                    (nmv) => { return nmv; },
                  );
                } else {
                  return $result.map(
                    (() => {
                      let _pipe$2 = version;
                      let _pipe$3 = next_major_version(_pipe$2);
                      return $result.map_error(
                        _pipe$3,
                        (_) => { return new InvalidConstraint(); },
                      );
                    })(),
                    (nmv) => { return nmv; },
                  );
                }
              })(),
              (nmv) => {
                return new Ok(
                  toList([
                    [new GreaterThanOrEqualTo(), version],
                    [new LessThan(), nmv],
                  ]),
                );
              },
            );
          } else if (constraint_result instanceof Wildcard &&
          constraint_result.version_part instanceof Patch) {
            let major = (() => {
              let _pipe$2 = version;
              return $version.major(_pipe$2);
            })();
            return $result.try$(
              (() => {
                if (major === 0) {
                  return $result.map(
                    (() => {
                      let _pipe$2 = version;
                      let _pipe$3 = next_minor_version(_pipe$2);
                      return $result.map_error(
                        _pipe$3,
                        (_) => { return new InvalidConstraint(); },
                      );
                    })(),
                    (nmv) => { return nmv; },
                  );
                } else {
                  return $result.map(
                    (() => {
                      let _pipe$2 = version;
                      let _pipe$3 = next_major_version(_pipe$2);
                      return $result.map_error(
                        _pipe$3,
                        (_) => { return new InvalidConstraint(); },
                      );
                    })(),
                    (nmv) => { return nmv; },
                  );
                }
              })(),
              (nmv) => {
                return new Ok(
                  toList([
                    [new GreaterThanOrEqualTo(), version],
                    [new LessThan(), nmv],
                  ]),
                );
              },
            );
          } else if (constraint_result instanceof Wildcard &&
          constraint_result.version_part instanceof Major) {
            return new Error(new InvalidConstraint());
          } else {
            return $result.try$(
              (() => {
                let _pipe$2 = version;
                let _pipe$3 = next_major_version(_pipe$2);
                return $result.map_error(
                  _pipe$3,
                  (_) => { return new InvalidConstraint(); },
                );
              })(),
              (nmv) => {
                return new Ok(
                  toList([
                    [new GreaterThanOrEqualTo(), version],
                    [new LessThan(), nmv],
                  ]),
                );
              },
            );
          }
        },
      );
    })(),
    (constraints) => { return new Ok(constraints); },
  );
}

function parse_tilde_version_constraint(constraint) {
  return $result.try$(
    (() => {
      let _pipe = constraint;
      let _pipe$1 = parse_loosely(_pipe);
      return $result.then$(
        _pipe$1,
        (state) => {
          let constraint_result = state[0];
          let version = state[1];
          if (constraint_result instanceof Strict) {
            return $result.try$(
              (() => {
                let _pipe$2 = version;
                let _pipe$3 = next_minor_version(_pipe$2);
                return $result.map_error(
                  _pipe$3,
                  (_) => { return new InvalidConstraint(); },
                );
              })(),
              (nmv) => {
                return new Ok(
                  toList([
                    [new GreaterThanOrEqualTo(), version],
                    [new LessThan(), nmv],
                  ]),
                );
              },
            );
          } else if (constraint_result instanceof Partial &&
          constraint_result.version_part instanceof Minor) {
            return $result.try$(
              (() => {
                let _pipe$2 = version;
                let _pipe$3 = next_minor_version(_pipe$2);
                return $result.map_error(
                  _pipe$3,
                  (_) => { return new InvalidConstraint(); },
                );
              })(),
              (nmv) => {
                return new Ok(
                  toList([
                    [new GreaterThanOrEqualTo(), version],
                    [new LessThan(), nmv],
                  ]),
                );
              },
            );
          } else if (constraint_result instanceof Wildcard &&
          constraint_result.version_part instanceof Patch) {
            return $result.try$(
              (() => {
                let _pipe$2 = version;
                let _pipe$3 = next_minor_version(_pipe$2);
                return $result.map_error(
                  _pipe$3,
                  (_) => { return new InvalidConstraint(); },
                );
              })(),
              (nmv) => {
                return new Ok(
                  toList([
                    [new GreaterThanOrEqualTo(), version],
                    [new LessThan(), nmv],
                  ]),
                );
              },
            );
          } else if (constraint_result instanceof Wildcard &&
          constraint_result.version_part instanceof Major) {
            return new Error(new InvalidConstraint());
          } else {
            return $result.try$(
              (() => {
                let _pipe$2 = version;
                let _pipe$3 = next_major_version(_pipe$2);
                return $result.map_error(
                  _pipe$3,
                  (_) => { return new InvalidConstraint(); },
                );
              })(),
              (nmv) => {
                return new Ok(
                  toList([
                    [new GreaterThanOrEqualTo(), version],
                    [new LessThan(), nmv],
                  ]),
                );
              },
            );
          }
        },
      );
    })(),
    (constraints) => { return new Ok(constraints); },
  );
}

function parse_constraint(constraint) {
  if (constraint.startsWith("^")) {
    let c = constraint.slice(1);
    return $result.try$(
      parse_caret_version_constraint(c),
      (versions) => { return new Ok(versions); },
    );
  } else if (constraint.startsWith("~")) {
    let c = constraint.slice(1);
    return $result.try$(
      parse_tilde_version_constraint(c),
      (versions) => { return new Ok(versions); },
    );
  } else if (constraint.startsWith("<=")) {
    let c = constraint.slice(2);
    return $result.try$(
      parse_version_constraint(new LessThanOrEqualTo(), c),
      (versions) => { return new Ok(versions); },
    );
  } else if (constraint.startsWith("<")) {
    let c = constraint.slice(1);
    return $result.try$(
      parse_version_constraint(new LessThan(), c),
      (versions) => { return new Ok(versions); },
    );
  } else if (constraint.startsWith(">=")) {
    let c = constraint.slice(2);
    return $result.try$(
      parse_version_constraint(new GreaterThanOrEqualTo(), c),
      (versions) => { return new Ok(versions); },
    );
  } else if (constraint.startsWith(">")) {
    let c = constraint.slice(1);
    return $result.try$(
      parse_version_constraint(new GreaterThan(), c),
      (versions) => { return new Ok(versions); },
    );
  } else if (constraint.startsWith("=")) {
    let c = constraint.slice(1);
    return $result.try$(
      parse_equality_version_constraint(c),
      (versions) => { return new Ok(versions); },
    );
  } else {
    let c = constraint;
    return $result.try$(
      parse_equality_version_constraint(c),
      (versions) => { return new Ok(versions); },
    );
  }
}

function do_parse_constraints(constraints) {
  return $result.try$(
    (() => {
      let _pipe = constraints;
      return $list.try_fold(
        _pipe,
        new Constraint(toList([])),
        (cs, c) => {
          return $result.try$(
            parse_constraint($string.trim(c)),
            (parsed_constraints) => {
              return new Ok(
                new Constraint($list.append(cs.constraints, parsed_constraints)),
              );
            },
          );
        },
      );
    })(),
    (parsed_constraints) => { return new Ok(parsed_constraints); },
  );
}

function parse_constraints(constraint) {
  if (constraint === "=*") {
    return $result.try$(
      (() => {
        let _pipe = $version.new$(0, 0, 0);
        return $result.map_error(
          _pipe,
          (_) => { return new InvalidConstraint(); },
        );
      })(),
      (version) => {
        return new Ok(
          new Constraint(toList([[new GreaterThanOrEqualTo(), version]])),
        );
      },
    );
  } else if (constraint === "*") {
    return $result.try$(
      (() => {
        let _pipe = $version.new$(0, 0, 0);
        return $result.map_error(
          _pipe,
          (_) => { return new InvalidConstraint(); },
        );
      })(),
      (version) => {
        return new Ok(
          new Constraint(toList([[new GreaterThanOrEqualTo(), version]])),
        );
      },
    );
  } else {
    let constraints = $string.split(constraint, ",");
    let $ = $list.is_empty(constraints);
    if ($) {
      return do_parse_constraints(toList([constraint]));
    } else {
      return do_parse_constraints(constraints);
    }
  }
}

export function new$(constraints) {
  return parse_constraints(constraints);
}
