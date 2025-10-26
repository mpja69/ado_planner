import { defineConfig } from 'vite'
import path from 'path'

// Bygger bara content-scriptet (MV3) och kopierar allt i /public till /dist
export default defineConfig({
	publicDir: 'public',
	build: {
		outDir: 'dist',
		emptyOutDir: true,
		rollupOptions: {
			input: {
				content: path.resolve(__dirname, 'src/ts/content.ts')
			},
			output: {
				// Se till att filen heter content.js i dist/
				entryFileNames: (chunk) =>
					chunk.name === 'content' ? 'content.js' : '[name].js',
				assetFileNames: '[name][extname]'
			}
		}
	}
})
