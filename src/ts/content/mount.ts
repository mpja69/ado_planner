// src/ts/content/mount.ts
import { SP_PING, SP_PONG } from '../shared/messages';

const OVERLAY_ID = 'sp-overlay-root';
const IFRAME_ID = 'sp-overlay-iframe';

let pongListener: ((ev: MessageEvent) => void) | null = null;

export function openOverlay() {
	if (document.getElementById(OVERLAY_ID)) return;

	// Backdrop
	const mask = document.createElement('div');
	mask.id = OVERLAY_ID;
	Object.assign(mask.style, {
		position: 'fixed',
		inset: '0',
		zIndex: '999999',
		background: 'var(--sp-backdrop, rgba(15,23,42,0.55))',
	});
	mask.addEventListener('click', (e) => {
		if (e.target === mask) closeOverlay();
	});

	// iFrame
	const frame = document.createElement('iframe');
	frame.id = IFRAME_ID;
	Object.assign(frame.style, {
		position: 'absolute',
		left: '5%',
		right: '5%',
		top: '5%',
		bottom: '5%',
		width: '90%',
		height: '90%',
		border: '0',
		borderRadius: '16px',
		boxShadow: '0 25px 80px rgba(0,0,0,0.35)',
		background: 'white',
	});
	frame.src = chrome.runtime.getURL('overlay.html');

	mask.appendChild(frame);
	document.body.appendChild(mask);

	// PING loop until we get PONG
	let gotPong = false;
	let tries = 0;
	const maxTries = 20;
	const intervalMs = 250;
	let pingTimer: number | null = null;

	const sendPing = () => {
		const win = frame.contentWindow;
		if (!win) return;
		try {
			win.postMessage({ type: SP_PING }, '*');
			console.debug('[SP][ADO] sent PING to overlay');
		} catch (e) {
			console.warn('[SP][ADO] postMessage failed', e);
		}
	};

	frame.addEventListener('load', () => {
		sendPing();
		pingTimer = window.setInterval(() => {
			if (gotPong || tries++ >= maxTries) {
				if (pingTimer) window.clearInterval(pingTimer);
				pingTimer = null;
				return;
			}
			sendPing();
		}, intervalMs);
	});

	pongListener = (ev: MessageEvent) => {
		const data = ev.data;
		if (!data || typeof data !== 'object') return;
		if (data.type === SP_PONG) {
			gotPong = true;
			if (pingTimer) window.clearInterval(pingTimer);
			pingTimer = null;
			console.debug('[SP][ADO] got PONG from overlay');
			// keep listener for later messages if needed
		}
	};

	window.addEventListener('message', pongListener);
}

export function closeOverlay() {
	if (pongListener) {
		window.removeEventListener('message', pongListener);
		pongListener = null;
	}
	document.getElementById(OVERLAY_ID)?.remove();
}
