import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";

export default defineConfig(({ command }) => {
  if (command === "build") {
    // Production settings
    return { plugins: [elmPlugin({ debug: false, optimize: true })] };
  } else {
    // Dev settings
    return { plugins: [elmPlugin({ debug: false, optimize: false })] };
  }
});
