// ─────────────────────────────────────────────────────────────
// src/ts/content/fetch/mappers.ts
// ─────────────────────────────────────────────────────────────

import type { WorkItemRef } from "./workitems";

export type FeatureDto = {
	id: number;
	title: string;
	state: string;
	areaPath: string;
	iterationPath: string;
	tags: string[];
};

export type StoryDto = {
	id: number;
	parentId: number;        // 0 if no parent
	title: string;
	state: string;
	areaPath: string;
	iterationPath: string;
};
/**
 * Extract parent feature id from relations (if present)
 */
export function extractParentIdFromRelations(
	rels?: Array<{ rel: string; url: string }>
): number | null {
	if (!rels) return null;
	const parentRel = rels.find(r => r.rel === "System.LinkTypes.Hierarchy-Reverse");
	if (!parentRel) return null;
	const m = parentRel.url.match(/\/(\d+)$/);
	return m ? parseInt(m[1], 10) : null;
}

export function toFeatureDto(wi: WorkItemRef): FeatureDto {
	const f = (wi.fields ?? {}) as Record<string, unknown>;
	const rawTags = (f['System.Tags'] as string) || '';
	const tags = rawTags
		? rawTags.split(';').map(s => s.trim()).filter(Boolean)
		: [];

	return {
		id: wi.id ?? 0,
		title: (f['System.Title'] as string) || '(untitled)',
		state: (f['System.State'] as string) || '',
		areaPath: (f['System.AreaPath'] as string) || '',
		iterationPath: (f['System.IterationPath'] as string) || '',
		tags
	};
}

export function toStoryDto(wi: WorkItemRef): StoryDto {
	const f = (wi.fields ?? {}) as Record<string, unknown>;;
	const rels = Array.isArray(wi.relations) ? wi.relations : []; // ← guard

	const parentId =
		rels
			.filter(r => r.rel === 'System.LinkTypes.Hierarchy-Reverse')
			.map(r => {
				const idStr = r.url?.split('/').pop() ?? '';
				const idNum = parseInt(idStr, 10);
				return Number.isFinite(idNum) ? idNum : 0;
			})[0] ?? 0;

	return {
		id: wi.id ?? 0,
		parentId,
		title: (f['System.Title'] as string) || '(untitled)',
		state: (f['System.State'] as string) || '',
		areaPath: (f['System.AreaPath'] as string) || '',
		iterationPath: (f['System.IterationPath'] as string) || ''
	};
}
