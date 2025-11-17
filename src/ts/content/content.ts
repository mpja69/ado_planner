// src/ts/content/content.ts
import { openOverlay } from "./mount";
import {
	SP_PING, SP_PONG,
	SP_REQ_ITERATIONS, // SP_ITERATIONS,
	SP_REQ_AREAS, SP_AREAS,
	SP_PI_META,
	SP_AREA_FAVORITES,
	SP_SET_ITERATION,
} from '../shared/messages';
import { fetchProjectIterations, flattenPiRoots, pickCurrentAndNext } from "./fetch/iterations";
import { fetchAreaRoots } from "./fetch/areas";
import { SP_REQ_DATA, SP_DATA } from '../shared/messages';
import { runWiql, runWiqlLinks, wiqlFeatures, wiqlStoriesUnderFeatureIds } from './fetch/wiql';
import { chunk, fetchWorkItemsBatch, updateWorkItemIteration } from "./fetch/workitems";
import { toFeatureDto, toStoryDto, type FeatureDto, type StoryDto } from './fetch/mappers';

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
				const meta = piRoots.map((r) => {
					const pi = piList.find((p) => p.root === r);
					return {
						root: r,
						sprintNames: pi ? pi.sprintNames : []
					};
				});
				reply(ev, { type: SP_PI_META, meta });

				console.log('[SP][content] sent SP_PI_META', piRoots, meta);
			} catch (e) {
				// reply(ev, { type: SP_ITERATIONS, piRoots: [] });
				reply(ev, { type: SP_PI_META, meta: [] });
				console.error('[SP][content] iterations failed', e);
			}
			break;
		}
		case SP_PING: {
			reply(ev, { type: SP_PONG });
			break;
		}
		case SP_REQ_DATA: {
			try {
				console.log("[SP][content][SP_REQ_DATA]")
				const { org, project } = getAdoContext();
				const { areaRoot, piRoot } = msg;

				// 1) IDs via WIQL
				console.log("[SP][content][running WIQL for features]")
				const featureIds = await runWiql(org, project, wiqlFeatures(areaRoot, piRoot));

				console.log("[SP][content][getting storyWiql for featureIds]:", featureIds)
				const storyWiql = wiqlStoriesUnderFeatureIds(featureIds);
				console.log("[SP][content][storyWiql]: ", storyWiql)

				const links = await runWiqlLinks(org, project, wiqlStoriesUnderFeatureIds(featureIds));
				console.log("[SP][content][links]: ", links);

				// parentMap: child (story) -> parent (feature)
				const parentMap = new Map<number, number>();
				for (const L of links) {
					if (typeof L.source === 'number' && typeof L.target === 'number') {
						parentMap.set(L.target, L.source);
					}
				}

				// 1) collect all numeric targets
				const rawTargets = links
					.map(l => l?.target)
					.filter((id): id is number => typeof id === 'number');

				// 2) de-dupe
				const uniqTargets = Array.from(new Set(rawTargets));

				// 3) remove any IDs that are actually featureIds (guards against Epic→Feature hops)
				const storyIds = uniqTargets.filter(id => !featureIds.includes(id));

				console.log("[SP][content][storyIds] (after filtering features): ", storyIds);
				console.log('[SP][content] parentMap size:', parentMap.size);
				console.log('[SP][content] sample parent pair:', storyIds[0], '->', parentMap.get(storyIds[0]));
				// Optional: short-circuit to reduce noise
				// if (featureIds.length === 0 && storyIds.length === 0) {
				// 	reply(ev, { type: SP_DATA, payload: { features: [], stories: [] } });
				// 	break;
				// }

				// 2) Fetch full items (chunked — ADO limit is 200)
				const FEATURE_FIELDS = [
					'System.Id', 'System.WorkItemType', 'System.Title', 'System.State',
					'System.AreaPath', 'System.IterationPath', 'System.Tags'
				];
				const STORY_FIELDS = [
					'System.Id', 'System.WorkItemType', 'System.Title', 'System.State',
					'System.AreaPath', 'System.IterationPath'
				];

				// features
				const featureChunks = chunk(featureIds, 200);
				const featureBatches = await Promise.all(
					featureChunks.map(ids => fetchWorkItemsBatch(org, project, ids, FEATURE_FIELDS))
				);
				const featureItems = featureBatches.flat();

				// stories
				const storyChunks = chunk(storyIds, 200);
				const storyBatches = await Promise.all(
					storyChunks.map(ids => fetchWorkItemsBatch(org, project, ids, STORY_FIELDS))
				);
				const storyItems = storyBatches.flat();

				console.log('[SP][content] batch counts →',
					{
						featChunks: featureChunks.length, storyChunks: storyChunks.length,
						features: featureItems.length, stories: storyItems.length
					});

				console.log('[SP][content] samples → featureItems[0]:', featureItems[0]);
				console.log('[SP][content] samples → storyItems[0]:', storyItems[0]);

				// 3) Transform (guard with || [])
				const featureDtos = (featureItems || []).map(toFeatureDto);
				// const stories = (storyItems || []).map(toStoryDto);
				const storyDtos = storyItems.map(wi => toStoryDto(wi, parentMap.get(wi.id)));


				reply(ev, { type: SP_DATA, payload: { features: featureDtos, stories: storyDtos } });
				console.log('[SP][content] sent SP_DATA → features:', featureDtos.length, 'stories:', storyDtos.length);
			} catch (e) {
				console.error('[SP][content] SP_REQ_DATA failed', e);
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
		// (ev. fler typer senare)
	}
});




