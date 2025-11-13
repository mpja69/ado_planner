// src/ts/content/fetch/iterations.ts
import { adoJson } from "./http";

/** Shape from ADO classificationnodes/iterations */
export type IterationNode = {
	id?: number;
	name: string;              // e.g. "PI 30", "PI 30 Sprint 1", "Iteration", "<Project>"
	path?: string;             // e.g. "<Project>\\Iteration\\PI 30\\PI 30 Sprint 1" (API artifact)
	attributes?: {
		startDate?: string;      // ISO
		finishDate?: string;     // ISO
		timeFrame?: "past" | "current" | "future";
	};
	hasChildren?: boolean;
	children?: IterationNode[];
};

export type IterationsTree = IterationNode;

/**
 * Fetch the project's iteration tree.
 * GET https://dev.azure.com/{org}/{project}/_apis/wit/classificationnodes/iterations?$depth=4&api-version=7.1-preview.2
 */
export async function fetchProjectIterations(
	org: string,
	project: string
): Promise<IterationsTree> {
	const url =
		`https://dev.azure.com/${encodeURIComponent(org)}/${encodeURIComponent(project)}` +
		`/_apis/wit/classificationnodes/iterations?$depth=4&api-version=7.1-preview.2`;
	return adoJson<IterationsTree>(url);
}

/**
 * Flatten the tree into PI rows that WIQL can use:
 * - root: "<Project>\\PI N"  (no "\Iteration\" segment)
 * - sprintNames: children names under the PI (excluding "IP Sprint")
 * - start/finish: PI dates if available
 */
export function flattenPiRoots(root: IterationNode): Array<{
	root: string;
	start?: Date;
	finish?: Date;
	sprintNames: string[];
}> {
	const project = root.name;
	const piNodes = (root.children ?? []).filter((n) => n.name?.startsWith("PI "));

	const rows = piNodes.map((pi) => {
		const start = pi.attributes?.startDate ? new Date(pi.attributes.startDate) : undefined;
		const finish = pi.attributes?.finishDate ? new Date(pi.attributes.finishDate) : undefined;

		const sprintNames =
			(pi.children ?? [])
				.map((c) => c.name)
				.filter((nm) => !/IP\s*Sprint/i.test(nm)); // drop IP sprint

		return {
			root: `${project}\\${pi.name}`, // <- WIQL-friendly path
			start,
			finish,
			sprintNames,
		};
	});

	// Sort by start date if present
	return rows.sort((a, b) => (a.start?.getTime() ?? 0) - (b.start?.getTime() ?? 0));
}

/** Pick current and next PI by dates; fallback to nearest future or last past. */
export function pickCurrentAndNext(
	pis: Array<{ root: string; start?: Date; finish?: Date }>,
	now = new Date()
): [string | undefined, string | undefined] {
	if (pis.length === 0) return [undefined, undefined];

	let idx = pis.findIndex((p) => p.start && p.finish && p.start <= now && now < p.finish);
	if (idx === -1) {
		// nearest future; otherwise last past
		const futureIdx = pis.findIndex((p) => p.start && p.start > now);
		idx = futureIdx !== -1 ? futureIdx : pis.length - 1;
	}
	const nextIdx = Math.min(idx + 1, pis.length - 1);
	const uniq = (xs: Array<string | undefined>) =>
		Array.from(new Set(xs.filter(Boolean) as string[]));

	const [curr, next] = uniq([pis[idx]?.root, pis[nextIdx]?.root]);
	return [curr, nextIdx !== idx ? next : undefined];
}
