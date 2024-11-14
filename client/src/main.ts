import "./style.css";
// @ts-ignore
import { main } from "./app.gleam";
import { main as pixi } from "./pixi.ts";

document.addEventListener("DOMContentLoaded", main);
document.addEventListener("DOMContentLoaded", pixi);
