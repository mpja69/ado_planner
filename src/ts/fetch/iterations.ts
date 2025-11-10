// src/ts/fetch/iterations.ts
export type PiInfo = {
	root: string;        // e.g. "Contoso\\PI 1"
	sprints: string[];   // ["PI 1 Sprint 1", ...] (no IP sprint)
};

export type IterationsPayload = {
	piRoots: string[];     // ["Contoso\\PI 1", "Contoso\\PI 2"]
	piTable: PiInfo[];     // details per PI
};

// Helper: drop the IP sprint names
function isRegularSprint(name: string): boolean {
	const upper = name.toUpperCase();
	return !(upper.includes("IP") && upper.includes("SPRINT"));
}

/**
 * Fetch team iterations with depth=2, map to PI + sprint names.
 * org/project/team must match the page you’re on.
 */
export async function fetchIterations(org: string, project: string, team: string): Promise<IterationsPayload> {
	const url = `https://dev.azure.com/${org}/${project}/${team}/_apis/work/teamsettings/iterations?$depth=2&api-version=7.0`;
	const res = await fetch(url, { credentials: "include" });
	if (!res.ok) throw new Error(`ADO iterations HTTP ${res.status}`);

	const data = await res.json();
	// ADO returns a flat list with children; we consider level-1 nodes as PIs.
	const pis = (data.value ?? []).filter((n: any) => (n.name ?? "").toUpperCase().startsWith("PI"));

	const piTable: PiInfo[] = pis.map((pi: any) => {
		const root = pi.path as string;              // e.g. "Contoso\\PI 1"
		const children = (pi.children ?? []) as any[];
		const sprints = children
			.map((c) => c.name as string)
			.filter(Boolean)
			.filter(isRegularSprint);
		return { root, sprints };
	});

	return {
		piRoots: piTable.map((p) => p.root),
		piTable,
	};
}
