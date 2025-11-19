// src/ts/content/content.ts
import { openOverlay } from "./mount";
import {
	SP_REQ_ITERATIONS, SP_ITERATIONS,
	SP_REQ_AREAS, SP_AREAS,
	SP_AREA_FAVORITES,
	SP_REQ_DATA, SP_DATA,
	SP_SET_ITERATION,
	SP_SET_TESTS,
	SP_OPEN_WORKITEM
} from '../shared/messages';
import { fetchProjectIterations, flattenPiRoots, pickCurrentAndNext } from "./fetch/iterations";
import { fetchAreaRoots } from "./fetch/areas";
import { fetchSprintPlannerData, updateWorkItemIteration, updateWorkItemTests } from "./fetch/workitems";

// ─────────────────────────────────────────────────────────────
// ADO topbar: inject overlay toggle button
// ─────────────────────────────────────────────────────────────

const BTN_ID = 'sprintplanner-button';
const TOPBAR_SELECTOR = '.region-header-menubar';
const MAX_RETRIES = 30;           // ~15s if 500ms
const RETRY_INTERVAL_MS = 500;

function createButton(): HTMLButtonElement {
	const btn = document.createElement('button');
	btn.id = BTN_ID;
	btn.innerText = 'Sprint Planner';
	btn.className = 'bolt-button bolt-icon-button enabled subtle bolt-focus-treatment';
	btn.style.marginLeft = '12px';
	btn.style.padding = '4px 12px';
	btn.onclick = openOverlay;
	return btn;
}

function tryInjectOnce(): boolean {
	if (document.getElementById(BTN_ID)) return true;
	const topBar = document.querySelector(TOPBAR_SELECTOR);
	if (!topBar) return false;
	topBar.appendChild(createButton());
	return true;
}

function injectOverlayToggleButton() {
	if (tryInjectOnce()) return;

	const obs = new MutationObserver(() => {
		if (tryInjectOnce()) obs.disconnect();
	});
	obs.observe(document.documentElement, { childList: true, subtree: true });

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

// ─────────────────────────────────────────────────────────────
// Message bridge: overlay <-> content (ADO page)
// ─────────────────────────────────────────────────────────────

function clog(...args: unknown[]) { console.log('[SP][content]', ...args); }

// Minimal kontext-parser för org/proj (justera vid behov)
function getAdoContext() {
	// https://dev.azure.com/{org}/{project}/...
	const m = location.pathname.match(/^\/([^/]+)\/([^/]+)\b/);
	const org = m?.[1] ?? '';
	const project = decodeURIComponent(m?.[2] ?? '');
	return { org, project };
}

// Svara *endast* till avsändaren (iframe:n)
function reply(ev: MessageEvent, payload: object) {
	(ev.source as WindowProxy | null)?.postMessage(payload, ev.origin || '*');
}

window.addEventListener('message', async (ev) => {
	const msg = ev.data;
	if (!msg || typeof msg !== 'object') return;

	switch (msg.type) {
		case SP_REQ_AREAS: {
			try {
				const { org, project } = getAdoContext();
				const areas = await fetchAreaRoots(org, project);
				clog('sent', SP_AREAS, '→', areas.length, 'areas');
				reply(ev, { type: SP_AREAS, areas });

				// TEMP: inga riktiga favoriter ännu → skicka tom lista
				const favoriteAreaIds: string[] = [];
				reply(ev, { type: SP_AREA_FAVORITES, favorites: favoriteAreaIds });
				clog('sent', SP_AREA_FAVORITES, '→', favoriteAreaIds);

			} catch (e) {
				console.error('[SP][content]', SP_REQ_AREAS, 'failed', e);
				reply(ev, { type: SP_AREAS, areas: [] });
				// Skicka tom favorites också vid fel, så Elm-koden inte väntar på något:
				reply(ev, { type: SP_AREA_FAVORITES, favorites: [] });
			}
			break;
		}
		case SP_REQ_ITERATIONS: {
			try {
				const { org, project } = getAdoContext();
				const tree = await fetchProjectIterations(org, project);

				// Build WIQL-friendly rows
				const piList = flattenPiRoots(tree);
				const [current, next] = pickCurrentAndNext(piList);

				// Send only current + next display roots to Elm
				const piRoots = [current, next].filter(Boolean) as string[];

				// Also send sprintCount meta (exclude IP sprint)
				const iterations = piRoots.map((r) => {
					const pi = piList.find((p) => p.root === r);
					return {
						root: r,
						sprintNames: pi ? pi.sprintNames : []
					};
				});
				reply(ev, { type: SP_ITERATIONS, iterations });

				console.log('[SP][content] sent SP_ITERATIONS', iterations);
			} catch (e) {
				reply(ev, { type: SP_ITERATIONS, iterations: [] });
				console.error('[SP][content] iterations failed', e);
			}
			break;
		}
		case SP_REQ_DATA: {
			try {
				console.log("[SP][content][SP_REQ_DATA]");
				const { org, project } = getAdoContext();
				const { areaRoot, piRoot } = msg;

				const { features, stories } = await fetchSprintPlannerData(
					org,
					project,
					areaRoot,
					piRoot
				);

				reply(ev, { type: SP_DATA, payload: { features, stories } });
				console.log(
					"[SP][content] sent SP_DATA → features:",
					features.length,
					"stories:",
					stories.length
				);
			} catch (e) {
				console.error("[SP][content] SP_REQ_DATA failed", e);
				reply(ev, { type: SP_DATA, payload: { features: [], stories: [] } });
			}
			break;
		}
		case SP_SET_ITERATION: {
			try {
				const { id, iterationPath } = msg.payload || {};
				if (typeof id !== "number" || typeof iterationPath !== "string") {
					console.warn("[SP][content] SP_SET_ITERATION → invalid payload", msg.payload);
					return;
				}

				const { org, project } = getAdoContext();
				console.log("[SP][content] SP_SET_ITERATION → update", { id, iterationPath });

				await updateWorkItemIteration(org, project, id, iterationPath);

				console.log("[SP][content] iteration updated OK", { id, iterationPath });
			} catch (e) {
				console.error("[SP][content] SP_SET_ITERATION failed", e);
			}
			break;
		}
		case SP_SET_TESTS: {
			try {
				const { org, project } = getAdoContext();
				const p = msg.payload as {
					id: number;
					sit: boolean;
					uat: boolean;
					e2e: boolean;
					sitTag: string;
					uatTag: string;
					e2eTag: string;
				};

				console.log("[SP][content] SP_SET_TESTS → update", p);

				await updateWorkItemTests(org, project, p);

				console.log("[SP][content] tests updated OK", p);
			} catch (e) {
				console.error("[SP][content] SP_SET_TESTS failed", e, msg.payload);
			}
			break;
		}
		case SP_OPEN_WORKITEM: {
			const { payload } = msg;
			if (!payload || typeof payload.id !== "number") {
				console.warn("[SP][content] SP_OPEN_WORKITEM missing id", payload);
				return;
			}

			const { org, project } = getAdoContext();
			const url =
				`https://dev.azure.com/` +
				encodeURIComponent(org) +
				"/" +
				encodeURIComponent(project) +
				"/_workitems/edit/" +
				String(payload.id);

			console.log("[SP][content] SP_OPEN_WORKITEM → window.open", url);
			window.open(url, "_blank", "noopener,noreferrer");
			break;
		}
		// (ev. fler typer senare)
	}
});


