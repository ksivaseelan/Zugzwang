import { CustomType as $CustomType } from "../../gleam.mjs";
import * as $types from "../../stoiridh/internal/types.mjs";
import { Major, Minor, Patch } from "../../stoiridh/internal/types.mjs";

export class Cursor extends $CustomType {
  constructor(version_part) {
    super();
    this.version_part = version_part;
  }
}

export function new$() {
  return new Cursor(new Major());
}

export function next(cursor) {
  if (cursor instanceof Cursor && cursor.version_part instanceof Major) {
    return new Cursor(new Minor());
  } else if (cursor instanceof Cursor && cursor.version_part instanceof Minor) {
    return new Cursor(new Patch());
  } else {
    return new Cursor(new Patch());
  }
}
