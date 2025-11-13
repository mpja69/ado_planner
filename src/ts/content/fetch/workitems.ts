// src/ts/content/fetch/workitems.ts
//
import { adoPostJson } from './http';
// import { wiqlStoriesByParents } from './wiql';

export type WorkItemRef = {
	id: number;
	fields?: Record<string, unknown>;
	relations?: Array<{ rel: string; url: string; attributes?: Record<string, unknown> }>;
};

// type BatchResponse = {
// 	value: WorkItemRef[];
// };

// const FIELDS = [
// 	"System.Id",
// 	"System.WorkItemType",
// 	"System.Title",
// 	"System.State",
// 	"System.AreaPath",
// 	"System.IterationPath",
// 	"System.Tags"
// ] as const;

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

// export async function fetchStoryIdsForFeatures(
// 	org: string,
// 	project: string,
// 	featureIds: number[],
// 	runWiql: (org: string, project: string, wiql: string) => Promise<number[]>
// ): Promise<number[]> {
// 	const batches = chunk(featureIds, 300);
// 	const lists = await Promise.all(
// 		batches.map(b => runWiql(org, project, wiqlStoriesByParents(b)))
// 	);
// 	// de-dupe (just in case)
// 	return Array.from(new Set(lists.flat()));
// }
