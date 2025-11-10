// src/ts/ado/mount.ts
import { OverlayMessage, ParentMessage } from '../shared/messages';

const OVERLAY_ID = 'sp-overlay-root';
const IFRAME_ID = 'sp-overlay-iframe';

console.debug('[SP][ADO] mount loaded');  // add at top of file

export function openOverlay() {
	if (document.getElementById(OVERLAY_ID)) return;

	const mask = document.createElement('div');
	mask.id = OVERLAY_ID;
	Object.assign(mask.style, {
		position: 'fixed',
		inset: '0',
		zIndex: '999999',
		background: 'var(--sp-backdrop, rgba(15,23,42,0.55))'
	});
	mask.addEventListener('click', (e) => {
		if (e.target === mask) closeOverlay();
	});

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
		background: 'white'
	});
	frame.src = chrome.runtime.getURL('overlay.html');

	mask.appendChild(frame);
	document.body.appendChild(mask);

	// robust PING loop
	let gotPong = false;
	let tries = 0;
	const maxTries = 20;
	const intervalMs = 250;

	const sendPing = () => {
		const win = frame.contentWindow;
		if (!win) return;
		const msg: OverlayMessage = { type: 'PING', from: 'ado' };
		try {
			win.postMessage(msg, '*');
			console.debug('[SP][ADO] sent PING to overlay');
		} catch (e) {
			console.warn('[SP][ADO] postMessage failed', e);
		}
	};

	// fire on load + keep pinging until PONG
	frame.addEventListener('load', () => {
		sendPing();
		const id = setInterval(() => {
			if (gotPong || tries++ >= maxTries) return clearInterval(id);
			sendPing();
		}, intervalMs);
	});

	// listen for PONG
	const onMessage = (ev: MessageEvent<ParentMessage>) => {
		if (!ev || !ev.data) return;
		if (ev.data.type === 'PONG') {
			gotPong = true;
			console.debug('[SP][ADO] got PONG from overlay');
			// keep listener for later messages (iterations, etc.)
		}
	};
	window.addEventListener('message', onMessage);
}

export function closeOverlay() {
	document.getElementById(OVERLAY_ID)?.remove();
}
