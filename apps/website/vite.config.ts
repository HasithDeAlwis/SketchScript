/// <reference types='vitest' />
import { defineConfig } from 'vite';
import { reactRouter } from '@react-router/dev/vite';
// import { nxViteTsPaths } from '@nx/vite/plugins/nx-tsconfig-paths.plugin';
import tsconfigPaths from 'vite-tsconfig-paths';
import netlifyPlugin from '@netlify/vite-plugin-react-router'
import path from 'path'
import tailwindcss from "@tailwindcss/vite";

export default defineConfig(() => ({
  resolve: {
    alias: {
      '@website': path.resolve(__dirname, '../../libs/website'),
      '@shared': path.resolve(__dirname, '../../libs/shared')
    }
  },
  server: {
    port: 4200,
    host: 'localhost',
  },
  preview: {
    port: 4300,
    host: 'localhost',
  },
  plugins: [
    netlifyPlugin(),
    reactRouter(),
    tsconfigPaths(),
    tailwindcss(),
  ],
  build: {
    rollupOptions: {
      external: ['shiki']
    }
  },
}));
