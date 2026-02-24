import { defineConfig } from 'vite';
import elmPlugin from 'vite-plugin-elm';

export default defineConfig({
  base: './',
  server: {
    hmr: false // Disables Hot Module Replacement
  },
  plugins: [
    elmPlugin({
      optimize: false,
    })
  ],
  build: {
    outDir: 'dist',
    emptyOutDir: true,
  }
});
