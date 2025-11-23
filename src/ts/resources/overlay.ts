// src/ts/resources/overlay.ts
import {
	SP_REQ_ITERATIONS, SP_ITERATIONS,
	SP_REQ_AREAS, SP_AREAS,
	SP_REQ_DATA, SP_DATA,
	SP_AREA_FAVORITES,
	SP_SET_ITERATION,
	SP_SET_TESTS,
	SP_OPEN_WORKITEM,
	SP_ERROR,
} from '../shared/messages';

/** Wire Elm ports for iterations + areas and general message bridge */
function wireElm(app: any) {
	// Log vilka portar Elm faktiskt exponerar (bra felsökning)
	const ports = app?.ports ? Object.keys(app.ports) : [];
	console.log('[SP][overlay] Elm ports:', ports);

	// ---- Elm -> overlay: begär iterationer ----
	if (app?.ports?.requestIterations) {
		app.ports.requestIterations.subscribe(() => {
			console.log('[SP][overlay] Elm requested iterations → post', SP_REQ_ITERATIONS);
			window.parent.postMessage({ type: SP_REQ_ITERATIONS }, '*');
		});
	} else {
		console.warn('[SP][overlay] port requestIterations missing');
	}

	// ---- Elm -> overlay: begär areas ----
	if (app?.ports?.requestAreas) {
		app.ports.requestAreas.subscribe(() => {
			console.log('[SP][overlay] Elm requested areas → post', SP_REQ_AREAS);
			window.parent.postMessage({ type: SP_REQ_AREAS }, '*');
		});
	} else {
		console.warn('[SP][overlay] port requestAreas missing');
	}

	// Elm -> request data (area+pi)
	if (app?.ports?.requestData) {
		app.ports.requestData.subscribe((payload: { area: string; pi: string }) => {
			console.log('[SP][overlay] Elm requestData →', payload);
			window.parent.postMessage({ type: SP_REQ_DATA, areaRoot: payload.area, piRoot: payload.pi }, '*');
		});
	} else {
		console.warn('[SP][overlay] port requestData missing');
	}

	if (app.ports.sendSetIteration) {
		app.ports.sendSetIteration.subscribe(({ id, iterationPath }) => {
			console.log("[SP][overlay] sendSetIteration → post SP_SET_ITERATION", { id, iterationPath });

			window.parent.postMessage(
				{
					type: SP_SET_ITERATION,
					payload: { id, iterationPath },
				},
				"*"
			);
		});
	}
	if (app.ports.sendUpdateTests) {
		app.ports.sendUpdateTests.subscribe(payload => {
			console.log("[SP][overlay] sendUpdateTests → post SP_SET_TESTS", payload);
			window.parent.postMessage(
				{
					type: SP_SET_TESTS,
					payload
				},
				"*"
			);
		});
	}
	if (app.ports.openWorkItem) {
		app.ports.openWorkItem.subscribe(id => {
			console.log("[SP][overlay] openWorkItem → post SP_OPEN_WORKITEM", id);

			// Viktigt: prata med parent (content.js), inte försöka öppna själv
			window.parent.postMessage(
				{
					type: SP_OPEN_WORKITEM,
					payload: { id }
				},
				"*"
			);
		});
	}
	// ---- overlay ← content: en enda message-listener för allt ----
	const onMessage = (ev: MessageEvent) => {
		const msg = ev.data;
		if (!msg || typeof msg !== 'object') return;

		switch (msg.type) {

			case SP_AREAS: {
				// Förväntat: { areas: Array<{ id: string, name: string }> }
				if (Array.isArray(msg.areas)) {
					console.log('[SP][overlay] got', SP_AREAS, '→ forward to Elm', msg.areas);
					app?.ports?.receiveAreas?.send(msg.areas);
				}
				break;
			}
			case SP_AREA_FAVORITES: {
				if (Array.isArray(msg.favorites)) {
					console.log('[SP][overlay] got SP_AREA_FAVORITES → forward to Elm', msg.favorites);
					app?.ports?.receiveAreaFavorites?.send(msg.favorites);
				}
				break;
			}

			case SP_ITERATIONS: {
				if (Array.isArray(msg.iterations)) {
					app?.ports?.receivePiMeta?.send(msg.iterations);
				}
				break;
			}

			case SP_DATA: {
				if (msg.payload && app?.ports?.receiveData) {
					console.log('[SP][overlay] got SP_DATA → forward to Elm', { f: msg.payload.features?.length ?? 0, s: msg.payload.stories?.length ?? 0 });
					app.ports.receiveData.send(msg.payload);
				}
				break;
			}
			case SP_ERROR: {
				if (msg.error && app?.ports?.receiveData) {
					console.warn("[SP][overlay] forwarding SP_ERROR → Elm", msg.error);
					app.ports.receiveError?.send(msg.error);
				}
				break;
			}
			// (ev. fler typer senare)
		}
	};

	window.addEventListener('message', onMessage);

}

/** Starta Elm när #app finns */
function bootElm() {
	const node = document.getElementById('app');
	if (!node) {
		console.error('[SP][overlay] #app not found in overlay.html');
		return;
	}

	function tryStart(attempt = 0) {
		const E = (window as any).Elm;
		if (!E) {
			if (attempt < 100) return setTimeout(() => tryStart(attempt + 1), 20);
			console.error('[SP][overlay] Elm global missing after wait');
			return;
		}
		const mod = E.App;
		if (!mod) {
			console.error('[SP][overlay] Elm exists but Elm.App missing. Elm keys:', Object.keys(E));
			return;
		}
		try {
			const app = mod.init({ node });
			console.log('[SP] Elm.App init OK');
			wireElm(app);
		} catch (err) {
			console.error('[SP][overlay] Elm.App.init error:', err);
		}
	}

	tryStart();
}

// Run once DOM is ready
if (document.readyState === 'loading') {
	document.addEventListener('DOMContentLoaded', bootElm);
} else {
	bootElm();
}
