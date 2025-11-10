// src/ts/resources/overlay.ts
import { SP_REQ_ITERATIONS, SP_ITERATIONS, SP_REQ_AREAS, SP_AREAS } from '../shared/messages';


function wireElmIterationPorts(app: any) {
	if (app?.ports?.requestIterations) {
		app.ports.requestIterations.subscribe(() => {
			console.log('[SP][overlay] Elm requested iterations → posting', SP_REQ_ITERATIONS);
			window.parent.postMessage({ type: SP_REQ_ITERATIONS }, '*');
		});
	}

	window.addEventListener('message', (ev) => {
		const msg = ev.data;
		if (msg && msg.type === SP_ITERATIONS && Array.isArray(msg.piRoots)) {
			console.log('[SP][overlay] got', SP_ITERATIONS, '→ forwarding to Elm', msg.piRoots);
			app?.ports?.receiveIterations?.send(msg.piRoots);
		}
	});
}

function wireElmAreaPorts(app: any) {
	if (app?.ports?.requestAreas) {
		app.ports.requestAreas.subscribe(() => {
			console.log('[SP][overlay] Elm requested areas → posting', SP_REQ_AREAS);
			window.parent.postMessage({ type: SP_REQ_AREAS }, '*');
		});
	}

	window.addEventListener('message', (ev) => {
		const msg = ev.data;
		if (msg && msg.type === SP_AREAS && Array.isArray(msg.areas)) {
			console.log('[SP][overlay] got', SP_AREAS, '→ forwarding to Elm', msg.areas);
			// msg.areas ska vara [{ id, name }, ...]
			app?.ports?.receiveAreas?.send(msg.areas);
		}
	});
}



// Boot Elm after #app exists
function bootElm() {
	const node = document.getElementById('app');
	if (!node) {
		console.error('[SP][Overlay] #app not found in overlay.html');
		return;
	}

	function tryStart(attempt = 0) {
		const E = (window as any).Elm;
		if (!E) {
			if (attempt < 100) return setTimeout(() => tryStart(attempt + 1), 20);
			console.error('[SP][Overlay] Elm global missing after wait');
			return;
		}
		const mod = E.App;
		if (!mod) {
			console.error('[SP][Overlay] Elm exists but Elm.App missing. Elm keys:', Object.keys(E));
			return;
		}
		try {
			const app = mod.init({ node });
			console.log('[SP] Elm.App init OK');
			wireElmIterationPorts(app);
			wireElmAreaPorts(app);
		} catch (err) {
			console.error('[SP][Overlay] Elm.App.init error:', err);
		}
	}

	tryStart();
}

function attachMessageBridge() {
	console.debug('[SP][Overlay] message bridge attached');
	window.addEventListener('message', (ev: MessageEvent) => {
		const msg = ev.data;
		if (!msg || typeof msg !== 'object') return;

		if (msg.type === 'PING' && msg.from === 'ado') {
			console.debug('[SP][Overlay] got PING, sending PONG');
			window.parent.postMessage({ type: 'PONG', from: 'overlay' }, '*');
		}
	});
}

// Run
if (document.readyState === 'loading') {
	document.addEventListener('DOMContentLoaded', () => {
		bootElm();
		attachMessageBridge();
	});
} else {
	bootElm();
	attachMessageBridge();
}




import { SP_PING, SP_PONG } from '../shared/messages';

function log(...args: unknown[]) { console.log('[SP][overlay]', ...args); }

window.addEventListener('message', (ev: MessageEvent) => {
	const msg = ev.data;
	if (!msg || typeof msg !== 'object') return;
	if (msg.type === SP_PONG) log('got PONG from content/page');
});

// Send one PING shortly after Elm starts
setTimeout(() => {
	log('sending PING to parent');
	window.parent.postMessage({ type: SP_PING }, '*');
}, 500);


