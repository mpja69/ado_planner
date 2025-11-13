// src/ts/content/fetch/areas.ts

import { adoJson } from "./http";

type AdoClassificationNode = {
	name: string;
	path: string;                 // e.g. "\\Contoso\\ART"
	children?: AdoClassificationNode[];
};

const API_VER = '7.1-preview.2'; // stable for classification nodes


/**
 * Returns the top-level Area children (ART roots).
 * Maps each to { id: "<Project>\\<ART>", name: "<ART>" }
 */
export async function fetchAreaRoots(org: string, project: string): Promise<Array<{ id: string; name: string }>> {
	// Get the root Areas node with one level of children:
	// GET https://dev.azure.com/{org}/{project}/_apis/wit/classificationnodes/areas?$depth=1
	const url = `https://dev.azure.com/${encodeURIComponent(org)}/${encodeURIComponent(project)}` +
		`/_apis/wit/classificationnodes/areas?$depth=1&api-version=${API_VER}`;

	const root = await adoJson<AdoClassificationNode>(url);
	const kids = root.children ?? [];

	// Top-level children under Areas are your ART roots
	// Build id as "<Project>\\<ART>" to match your Elm sample expectations.
	return kids.map(child => ({
		id: `${project}\\${child.name}`,
		name: child.name
	}));
}
