/// <reference types='vitest' />
import { defineConfig } from 'vite';
import { reactRouter } from '@react-router/dev/vite';
// import { nxViteTsPaths } from '@nx/vite/plugins/nx-tsconfig-paths.plugin';
import { nxCopyAssetsPlugin } from '@nx/vite/plugins/nx-copy-assets.plugin';
import netlifyPlugin from '@netlify/vite-plugin-react-router'
import path from 'path'
import tailwindcss from "@tailwindcss/vite";

export default defineConfig(() => ({
  root: __dirname,
  cacheDir: '../../node_modules/.vite/apps/website',
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
    // nxViteTsPaths(),
    tailwindcss(),
    nxCopyAssetsPlugin(['*.md']),
  ],
  // Uncomment this if you are using workers.
  // worker: {
  //  plugins: [ nxViteTsPaths() ],
  // },
  build: {
    rollupOptions: {
      external: ['shiki']
    }
  },
  // test: {
  //   watch: false,
  //   globals: true,
  //   environment: 'jsdom',
  //   include: ['{src,tests}/**/*.{test,spec}.{js,mjs,cjs,ts,mts,cts,jsx,tsx}'],
  //   reporters: ['default'],
  //   coverage: {
  //     reportsDirectory: '../../coverage/apps/website',
  //     provider: 'v8' as const,
  //   },
  // },
}));
