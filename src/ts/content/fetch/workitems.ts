// src/ts/content/fetch/workitems.ts
//
import { adoPostJson } from './http';
import { toFeatureDto, toStoryDto, type FeatureDto, type StoryDto } from './mappers';
import { runWiql, runWiqlLinks, wiqlFeatures, wiqlStoriesUnderFeatureIds } from './wiql';

export type WorkItemRef = {
	id: number;
	fields?: Record<string, unknown>;
	relations?: Array<{ rel: string; url: string; attributes?: Record<string, unknown> }>;
};


export function chunk<T>(xs: T[], n = 200): T[][] {
	const out: T[][] = [];
	for (let i = 0; i < xs.length; i += n) out.push(xs.slice(i, i + n));
	return out;
}

// src/ts/content/fetch/workitems.ts
export async function fetchWorkItemsBatch(
	org: string,
	project: string,
	ids: number[],
	fields: string[]
): Promise<WorkItemRef[]> {
	if (!ids || ids.length === 0) return [];

	const url =
		`https://dev.azure.com/${encodeURIComponent(org)}/${encodeURIComponent(project)}` +
		`/_apis/wit/workitemsbatch?api-version=7.1-preview.1`;

	const body = { ids, fields }; // ← no $expand

	try {
		const resp = await adoPostJson<{ value?: WorkItemRef[] }>(url, body);
		return Array.isArray(resp?.value) ? resp!.value! : [];
	} catch (e) {
		console.error('[SP][content] batch fetch failed', e);
		return [];
	}
}



export async function updateWorkItemIteration(
	org: string,
	project: string,
	id: number,
	iterationPath: string
): Promise<void> {
	const base =
		`https://dev.azure.com/${encodeURIComponent(org)}` +
		`/${encodeURIComponent(project)}`;

	const url = `${base}/_apis/wit/workitems/${id}?api-version=7.1-preview.3`;

	const body = [
		{
			op: "add", // funkar även om fältet redan finns; "replace" hade också funkat
			path: "/fields/System.IterationPath",
			value: iterationPath,
		},
	];

	const res = await fetch(url, {
		method: "PATCH",
		credentials: "include",
		headers: {
			"Content-Type": "application/json-patch+json",
		},
		body: JSON.stringify(body),
	});

	if (!res.ok) {
		const text = await res.text().catch(() => "");
		console.error("[SP][content] updateWorkItemIteration failed", res.status, text);
		throw new Error(`ADO PATCH failed ${res.status}`);
	}

	console.log("[SP][content] updateWorkItemIteration OK", { id, iterationPath });
}

type WorkItemWithTags = {
	fields?: {
		"System.Tags"?: string;
	};
};

export async function updateWorkItemTests(
	org: string,
	project: string,
	p: {
		id: number;
		sit: boolean;
		uat: boolean;
		e2e: boolean;
		sitTag: string;
		uatTag: string;
		e2eTag: string;
	}
): Promise<void> {
	const baseUrl = `https://dev.azure.com/${encodeURIComponent(org)}/${encodeURIComponent(
		project
	)}/_apis/wit/workitems/${p.id}`;

	// 1) Läs nuvarande tags
	const getUrl = `${baseUrl}?api-version=7.1-preview.3&fields=System.Tags`;
	console.log("[SP][content] updateWorkItemTests GET", getUrl);

	const getRes = await fetch(getUrl, {
		method: "GET",
		credentials: "include",
		headers: {
			"Content-Type": "application/json"
		}
	});

	if (!getRes.ok) {
		throw new Error(
			`GET work item failed ${getRes.status}: ${await getRes.text()}`
		);
	}

	const current: WorkItemWithTags = await getRes.json();
	const rawTags = current.fields?.["System.Tags"] ?? "";

	// 2) Splitta och normalisera
	const existing = rawTags
		.split(";")
		.map(s => s.trim())
		.filter(Boolean);

	const testNames = [p.sitTag, p.uatTag, p.e2eTag].filter(Boolean);

	// 3) Ta bort gamla test-taggar
	const withoutTests = existing.filter(
		t => !testNames.includes(t)
	);

	// 4) Lägg till enligt booleans
	const finalTags: string[] = [...withoutTests];

	if (p.sit && p.sitTag) finalTags.push(p.sitTag);
	if (p.uat && p.uatTag) finalTags.push(p.uatTag);
	if (p.e2e && p.e2eTag) finalTags.push(p.e2eTag);

	const newTagString = finalTags.join("; ");

	// 5) PATCH tillbaka
	const patchUrl = `${baseUrl}?api-version=7.1-preview.3`;
	console.log("[SP][content] updateWorkItemTests PATCH", patchUrl, newTagString);

	// VIKTIGT: använd "replace" om fältet redan finns, annars "add"
	const hasExisting = rawTags.trim().length > 0;

	const patchBody = [
		{
			op: hasExisting ? "replace" : "add",
			path: "/fields/System.Tags",
			value: newTagString
		}
	];

	const patchRes = await fetch(patchUrl, {
		method: "PATCH",
		credentials: "include",
		headers: {
			"Content-Type": "application/json-patch+json"
		},
		body: JSON.stringify(patchBody)
	});

	if (!patchRes.ok) {
		const bodyText = await patchRes.text();
		console.error("[SP][content] updateWorkItemTests failed", patchRes.status, bodyText);
		throw new Error(
			`PATCH work item tags failed ${patchRes.status}: ${bodyText}`
		);
	}

	console.log("[SP][content] updateWorkItemTests OK", {
		id: p.id,
		tags: newTagString
	});
}



export async function fetchSprintPlannerData(
	org: string,
	project: string,
	areaRoot: string,
	piRoot: string
): Promise<{ features: FeatureDto[]; stories: StoryDto[] }> {
	// 1) IDs via WIQL
	console.log("[SP][content][SP_REQ_DATA] wiql for features");
	const featureIds = await runWiql(org, project, wiqlFeatures(areaRoot, piRoot));

	console.log("[SP][content] featureIds:", featureIds);

	// 2) Länkar Feature → Story
	const storyWiql = wiqlStoriesUnderFeatureIds(featureIds);
	console.log("[SP][content][storyWiql]: ", storyWiql);

	const links = await runWiqlLinks(org, project, storyWiql);
	console.log("[SP][content][links]: ", links);

	// parentMap: child (story) -> parent (feature)
	const parentMap = new Map<number, number>();
	for (const L of links) {
		if (typeof L.source === "number" && typeof L.target === "number") {
			parentMap.set(L.target, L.source);
		}
	}

	// 3) storyIds = alla target-ID:n, minus ev. featureIds
	const rawTargets = links
		.map(l => l?.target)
		.filter((id): id is number => typeof id === "number");

	const uniqTargets = Array.from(new Set(rawTargets));
	const storyIds = uniqTargets.filter(id => !featureIds.includes(id));

	console.log("[SP][content][storyIds] (after filtering features): ", storyIds);
	console.log("[SP][content] parentMap size:", parentMap.size);
	if (storyIds[0] != null) {
		console.log(
			"[SP][content] sample parent pair:",
			storyIds[0],
			"->",
			parentMap.get(storyIds[0])
		);
	}

	// 4) Fetcha work items i chunkar
	const FEATURE_FIELDS = [
		"System.Id",
		"System.WorkItemType",
		"System.Title",
		"System.State",
		"System.AreaPath",
		"System.IterationPath",
		"System.Tags",
		"Microsoft.VSTS.Common.StackRank"
	];
	const STORY_FIELDS = [
		"System.Id",
		"System.WorkItemType",
		"System.Title",
		"System.State",
		"System.AreaPath",
		"System.IterationPath"
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

	console.log("[SP][content] batch counts →", {
		featChunks: featureChunks.length,
		storyChunks: storyChunks.length,
		features: featureItems.length,
		stories: storyItems.length
	});

	console.log("[SP][content] samples → featureItems[0]:", featureItems[0]);
	console.log("[SP][content] samples → storyItems[0]:", storyItems[0]);

	// 5) Transform
	const featureDtos = (featureItems || []).map(toFeatureDto);
	const storyDtos = storyItems.map(wi => toStoryDto(wi, parentMap.get(wi.id)));
	featureDtos.sort((a, b) => {
		if (a.stackRank == null && b.stackRank == null) return 0;
		if (a.stackRank == null) return 1;
		if (b.stackRank == null) return -1;
		return a.stackRank - b.stackRank;
	});
	return { features: featureDtos, stories: storyDtos };
}

