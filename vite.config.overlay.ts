// vite.config.overlay.ts
import type { UserConfig } from 'vite';

const config: UserConfig = {
	build: {
		outDir: 'dist',
		emptyOutDir: false,          // keep content.js
		rollupOptions: {
			input: 'src/ts/resources/overlay.ts',
			output: {
				inlineDynamicImports: true,
				// overlay runs in an <iframe> page, iife is fine here too
				format: 'iife',
				entryFileNames: 'overlay.js',
				chunkFileNames: 'overlay.[name].js',
				assetFileNames: 'overlay.[name].[ext]',
			},
		},
	},
};

export default config;
