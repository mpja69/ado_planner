import { openOverlay } from './mount';

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
