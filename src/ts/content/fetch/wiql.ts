// src/ts/content/fetch/wiql.ts
import { adoPostJson } from './http';

type WiqlWorkItemsResult = {
	workItems?: { id: number }[];
};

export async function runWiql(
	org: string,
	project: string,
	wiql: string
): Promise<number[]> {
	const url =
		`https://dev.azure.com/${encodeURIComponent(org)}/${encodeURIComponent(project)}` +
		`/_apis/wit/wiql?api-version=7.1-preview.2`;

	const body = { query: wiql };
	const res = await adoPostJson<WiqlWorkItemsResult>(url, body);

	const items = res.workItems ?? [];
	console.log('[SP][content][runWiql] response keys:', Object.keys(res));
	return items.map(w => w.id);
}



// ----- WIQL → link relations (workitemLinks) -----

// Form enligt ADO workitemLinks:
// - root rows:   { rel: null, source: null, target: { id, url } }
// - link rows:   { rel: "System.LinkTypes.Hierarchy-Forward",
//                  source: { id, url }, target: { id, url } }
type WiqlRelationsRow = {
	rel?: string | null;
	source?: { id: number; url?: string } | null;
	target?: { id: number; url?: string } | null;
};

type WiqlLinksResult = {
	workItemRelations?: WiqlRelationsRow[];
};

export type WiqlLink = {
	rel?: string;
	source?: number; // featureId
	target?: number; // storyId
};

export async function runWiqlLinks(
	org: string,
	project: string,
	wiql: string
): Promise<WiqlLink[]> {
	const url =
		`https://dev.azure.com/${encodeURIComponent(org)}/${encodeURIComponent(project)}` +
		`/_apis/wit/wiql?api-version=7.1-preview.2`;

	const body = { query: wiql };
	const res = await adoPostJson<WiqlLinksResult>(url, body);

	const rows = res.workItemRelations ?? [];
	console.log('[SP][content][runWiqlLinks] raw first row:', rows[0]);
	console.log('[SP][content][runWiqlLinks] raw second row:', rows[1]);

	return rows.map(r => ({
		rel: r.rel ?? undefined,
		source: r.source?.id,
		target: r.target?.id
	}));
}

// ----- WIQL builders -----

// Features in area + PI
export function wiqlFeatures(areaRoot: string, piRoot: string): string {
	return `
SELECT [System.Id]
FROM workitems
WHERE
  [System.TeamProject] = @project
  AND [System.WorkItemType] = 'Feature'
  AND [System.AreaPath] UNDER "${areaRoot}"
  AND [System.IterationPath] UNDER "${piRoot}"
ORDER BY [System.ChangedDate] DESC
`.trim();
}

// Stories under a list of feature IDs
export function wiqlStoriesUnderFeatureIds(featureIds: number[]): string {
	if (featureIds.length === 0) {
		// harmless query returning nothing
		return `
SELECT [System.Id]
FROM workitemLinks
WHERE
  [Source].[System.Id] IN (-1)
MODE (Recursive)
`.trim();
	}

	const idList = featureIds.join(',');
	return `
SELECT [System.Id]
FROM workitemLinks
WHERE
  [Source].[System.Id] IN (${idList})
  AND [System.Links.LinkType] = 'System.LinkTypes.Hierarchy-Forward'
  AND [Target].[System.WorkItemType] IN ('User Story','Product Backlog Item','Story')
MODE (Recursive)
`.trim();
}
