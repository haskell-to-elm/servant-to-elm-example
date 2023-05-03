import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";

export default defineConfig(({ command }) => {
  if (command === "build") {
    // Production settings
    return {
      plugins: [elmPlugin({ debug: false, optimize: true })],
      server: {
        port: 8080,
      },
    };
  } else {
    // Dev settings
    return {
      plugins: [elmPlugin({ debug: false, optimize: false })],
      server: {
        port: 8080,
      },
    };
  }
});
