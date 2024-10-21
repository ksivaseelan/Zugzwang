import gleam from "vite-gleam";
import * as path from "node:path";
import { defineConfig } from "vite";

export default defineConfig({
  resolve: {
    alias: {
      "@assets": path.resolve(__dirname, "./assets"),
    },
  },
  build: {
    sourcemap: true,
  },
  plugins: [gleam()],
});
