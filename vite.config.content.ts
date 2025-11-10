// vite.config.content.ts
import type { UserConfig } from 'vite';

const config: UserConfig = {
	build: {
		outDir: 'dist',
		emptyOutDir: false,          // don’t wipe overlay build
		rollupOptions: {
			input: 'src/ts/content/content.ts',
			output: {
				// ensure a single-file bundle with no `import` in content.js
				inlineDynamicImports: true,
				// (optional) wrap as IIFE to be extra safe for content scripts
				format: 'iife',
				entryFileNames: 'content.js',
				chunkFileNames: 'content.[name].js',
				assetFileNames: 'content.[name].[ext]',
			},
		},
	},
};

export default config;
// import { defineConfig } from 'vite';
//
// export default defineConfig({
// 	build: {
// 		rollupOptions: {
// 			input: {
// 				// your existing content entry:
// 				content: 'src/ts/content/content.ts',
// 				// new overlay entry:
// 				overlay: 'src/ts/resources/overlay.ts',
// 			},
// 			output: {
// 				// keep it simple: flat files in dist root
// 				entryFileNames: '[name].js',
// 			}
// 		}
// 	}
// });
