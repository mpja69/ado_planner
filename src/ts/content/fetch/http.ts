// src/ts/content/fetch/http.ts

export async function adoJson<T>(url: string): Promise<T> {
	const res = await fetch(url, { credentials: 'include' });
	if (!res.ok) {
		const txt = await res.text().catch(() => "");
		throw new Error(`ADO ${res.status} ${res.statusText}: ${txt}`);
	}
	return res.json();
}


export async function adoPostJson<T>(url: string, body: any): Promise<T> {
	const res = await fetch(url, {
		method: 'POST',
		credentials: 'include',
		headers: {
			'Content-Type': 'application/json',
		},
		body: JSON.stringify(body),
	});
	if (!res.ok) {
		const txt = await res.text().catch(() => '');
		throw new Error(`ADO POST failed ${res.status}: ${url}\nBody: ${txt}`);
	}
	return (await res.json()) as T;
}
