import { CustomType as $CustomType } from "../../gleam.mjs";

export class Partial extends $CustomType {
  constructor(version_part) {
    super();
    this.version_part = version_part;
  }
}

export class Strict extends $CustomType {}

export class Wildcard extends $CustomType {
  constructor(version_part) {
    super();
    this.version_part = version_part;
  }
}

export class Major extends $CustomType {}

export class Minor extends $CustomType {}

export class Patch extends $CustomType {}
