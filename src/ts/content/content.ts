// src/ts/content.ts

import { openOverlay } from "./mount";
import { fetchAreaRoots } from "../fetch/areas";

// ---- Config ----
const BTN_ID = 'sprintplanner-button';
const TOPBAR_SELECTOR = '.region-header-menubar';
const MAX_RETRIES = 30;      // ~15s om intervallet är 500ms
const RETRY_INTERVAL_MS = 500;

// ---- Helpers ----
function createButton(): HTMLButtonElement {
	const btn = document.createElement('button');
	btn.id = BTN_ID;
	btn.innerText = 'Sprint Planner';
	// ADO bolt styles så den smälter in
	btn.className = 'bolt-button bolt-icon-button enabled subtle bolt-focus-treatment';
	btn.style.marginLeft = '12px';
	btn.style.padding = '4px 12px';
	btn.onclick = openOverlay;
	return btn;
}

function tryInjectOnce(): boolean {
	if (document.getElementById(BTN_ID)) return true; // redan injicerad
	const topBar = document.querySelector(TOPBAR_SELECTOR);
	if (!topBar) return false;

	const btn = createButton();
	topBar.appendChild(btn);
	return true;
}

// ---- Strategy: quick try, then Observer, then interval fallback ----
function injectOverlayToggleButton() {
	// 1) snabb chans nu
	if (tryInjectOnce()) return;

	// 2) observer: när topbaren dyker upp
	const obs = new MutationObserver(() => {
		if (tryInjectOnce()) obs.disconnect();
	});
	obs.observe(document.documentElement, { childList: true, subtree: true });

	// 3) intervall fallback (garanterar försök även om observer missar)
	let tries = 0;
	const id = setInterval(() => {
		if (tryInjectOnce() || ++tries >= MAX_RETRIES) {
			clearInterval(id);
			obs.disconnect();
		}
	}, RETRY_INTERVAL_MS);
}

function init() {
	injectOverlayToggleButton();
}

if (document.readyState === 'loading') {
	document.addEventListener('DOMContentLoaded', init);
} else {
	init();
}




import { SP_PING, SP_PONG, SP_REQ_ITERATIONS, SP_ITERATIONS, SP_REQ_AREAS, SP_AREAS } from '../shared/messages';
function clog(...args: unknown[]) { console.log('[SP][content]', ...args); }

// tiny helper – adjust to however you already parse org/project
function getAdoContext() {
	// Example extraction from location:
	// https://dev.azure.com/{org}/{project}/_boards
	const m = location.pathname.match(/^\/([^/]+)\/([^/]+)\b/);
	const org = m?.[1] ?? '';
	const project = decodeURIComponent(m?.[2] ?? '');
	return { org, project };
}


window.addEventListener('message', (ev: MessageEvent) => {
	const msg = ev.data;
	if (!msg || typeof msg !== 'object') return;

	if (msg.type === SP_PING) {
		clog('got PING from overlay → replying PONG');
		(ev.source as WindowProxy | null)?.postMessage({ type: SP_PONG }, '*');
	}

	// NEW: overlay requests iterations
	if (msg?.type === SP_REQ_ITERATIONS) {
		console.log('[SP][content] got SP_REQ_ITERATIONS → replying SP_ITERATIONS');
		// TEMP: static two PIs — replace with real ADO later
		const piRoots = ['Contoso\\PI 1', 'Contoso\\PI 2'];
		// reply only to the iframe that asked
		(ev.source as WindowProxy)?.postMessage(
			{ type: SP_ITERATIONS, piRoots },
			ev.origin || '*'
		);
	}

	if (msg.type === SP_REQ_AREAS) {
		(async () => {
			try {
				const { org, project } = getAdoContext();
				const areas = await fetchAreaRoots(org, project);

				(ev.source as WindowProxy)?.postMessage(
					{ type: SP_AREAS, areas },
					ev.origin || '*'
				);
			} catch (err) {
				console.error('[SP][content] fetchAreaRoots failed', err);
				(ev.source as WindowProxy)?.postMessage(
					{ type: SP_AREAS, areas: [] },
					ev.origin || '*'
				);
			}
		})();
	}
});
