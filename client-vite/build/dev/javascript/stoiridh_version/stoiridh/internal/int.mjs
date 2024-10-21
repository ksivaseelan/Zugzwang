import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import { Ok, Error } from "../../gleam.mjs";

export function is_positive(value) {
  if (value >= 0) {
    let value$1 = value;
    return new Ok(value$1);
  } else {
    return new Error($int.to_string(value) + " is not a positive integer.");
  }
}
