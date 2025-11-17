// src/ts/content/fetch/workitems.ts
//
import { adoPostJson } from './http';

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



// PATCH: update System.IterationPath for a single work item
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
