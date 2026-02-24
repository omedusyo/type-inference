import { defineConfig } from 'vite';
import elmPlugin from 'vite-plugin-elm';

export default defineConfig({
  base: './',
  server: {
    // hmr: false,
    watch: null // Disable the crashing debugger
  },
  plugins: [
    elmPlugin({
      optimize: false,
      debug: false     // Disable the crashing debugger
    })
  ],
  build: {
    outDir: 'dist',
    emptyOutDir: true,
  }
});
