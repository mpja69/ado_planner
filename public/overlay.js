(function boot() {
	const node = document.getElementById('app');
	if (!node) {
		console.error('[SP] #app hittades inte i overlay.html');
		return;
	}

	// Vänta tills Elm globalen finns (om nätet/disk är långsamt)
	function tryStart(attempt = 0) {
		const E = window.Elm;
		if (!E) {
			if (attempt < 100) return setTimeout(() => tryStart(attempt + 1), 20);
			console.error('[SP] Elm global saknas efter väntan');
			return;
		}

		// Vanligtvis finns E.App när modulnamnet är module Elm.App exposing (main)
		const mod = E.App;
		if (!mod) {
			console.error('[SP] Elm finns men Elm.App saknas. Keys på Elm:', Object.keys(E));
			return;
		}

		try {
			mod.init({ node });
			console.log('[SP] Elm.App init OK');
		} catch (err) {
			console.error('[SP] Elm.App.init fel:', err);
		}
	}

	tryStart();
})();
