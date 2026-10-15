import { APIRequestContext, expect } from "@playwright/test";
import compose from "docker-compose";
import path from "path";

export const repositoryRoot = path.join(__dirname, "..");

// The local build, which .\bundle.ps1 -environment local writes, runs from its own directory,
// where its compose file finds the .env beside it.
export const localStack = {
    cwd: path.join(repositoryRoot, "dist/local"),
    config: "docker-compose.yml",
};

// docker-compose rejects with its own `{ exitCode, out, err }` result rather than an Error,
// which Playwright reports as a failure carrying neither a message nor any compose output.
export async function rethrowComposeError(command: () => Promise<unknown>): Promise<void> {
    try {
        await command();
    } catch (rejection) {
        const { err } = rejection as { err?: string };
        throw new Error(err || String(rejection));
    }
}

// compose returns once the containers start, which is well before the node container
// has installed its dependencies and opened its port. Asking for a non-empty game
// list rather than any answer at all also waits out the seed.
export async function waitForApi(request: APIRequestContext): Promise<void> {
    await expect(async () => {
        const response = await request.get("/api/games", { timeout: 5_000 });
        expect(response.status()).toBe(200);
        expect((await response.json()).length).toBeGreaterThan(0);
    }).toPass({ timeout: 60_000, intervals: [500] });
}
