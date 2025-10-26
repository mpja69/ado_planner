const OVERLAY_ID = 'sp-overlay-root';
const IFRAME_ID = 'sp-overlay-iframe';

/**
 * Öppnar en halvtransparent overlay med en centrerad iframe
 * som laddar vår public/overlay.html
 */
export function openOverlay() {
	if (document.getElementById(OVERLAY_ID)) return;

	// Mask/backdrop
	const mask = document.createElement('div');
	mask.id = OVERLAY_ID;
	Object.assign(mask.style, {
		position: 'fixed',
		inset: '0',
		zIndex: '999999', // högt nog över ADO
		background: 'var(--sp-backdrop, rgba(15,23,42,0.55))'
	});

	// Stäng overlay om man klickar utanför iFrame
	mask.addEventListener('click', (e) => {
		if (e.target === mask) closeOverlay();
	});

	// Själva iFrame-fönstret
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

	// Ladda vår overlay.html från extensionens bundle
	frame.src = chrome.runtime.getURL('overlay.html');

	mask.appendChild(frame);
	document.body.appendChild(mask);
}

/** Stänger och rensar overlay */
export function closeOverlay() {
	document.getElementById(OVERLAY_ID)?.remove();
}
