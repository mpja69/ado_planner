// src/ts/shared/messages.ts

// Requests from App.elm -> overlay.ts -> content.ts
export const SP_REQ_ITERATIONS = 'SP_REQ_ITERATIONS';
export const SP_REQ_AREAS = 'SP_REQ_AREAS';
export const SP_REQ_DATA = 'SP_REQ_DATA';

export type OverlayToContent =
	| { type: typeof SP_REQ_ITERATIONS }
	| { type: typeof SP_REQ_AREAS }
	| { type: typeof SP_REQ_DATA; areaRoot: string; piRoot: string }; // NEW




// Answers from content.ts -> overlay.ts -> App.elm
export const SP_AREAS = 'SP_AREAS';
export const SP_ITERATIONS = 'SP_ITERATIONS' as const;
export type iterations = {
	root: string;
	sprintNames: string[];
};
export const SP_DATA = 'SP_DATA';
export const SP_AREA_FAVORITES = 'SP_AREA_FAVORITES' as const;

// Errors from content.ts -> overlay.ts -> App.elm
export const SP_ERROR = "SP_ERROR" as const;
export type ErrorPayload = {
	source: string;     // t.ex. "areas", "iterations", "data"
	message: string;    // kort human-friendly
	detail?: string;    // valfri för djupare loggning
};


export type ContentToOverlay =
	| { type: typeof SP_AREAS; areas: Array<{ id: string; name: string }> }
	| { type: typeof SP_ITERATIONS; iterations: iterations[] }
	| {
		type: typeof SP_DATA;
		data: {
			features: Array<{
				id: number;
				title: string;
				state: string;
				areaPath: string;
				iterationPath: string;
				tags: string[];
				stackRank: number | null;
			}>;
			stories: Array<{
				id: number;
				title: string;
				state: string;
				areaPath: string;
				iterationPath: string;
				parentId: number;
			}>;
		};
	}
	| { type: typeof SP_AREA_FAVORITES; favorites: string[] }
	| { type: typeof SP_ERROR; error: ErrorPayload };


// Commands from App.elm -> overlay
export const SP_SET_ITERATION = "SP_SET_ITERATION";
export const SP_SET_TESTS = "SP_SET_TESTS";
export const SP_OPEN_WORKITEM = "SP_OPEN_WORKITEM"

