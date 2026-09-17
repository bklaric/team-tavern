import { APIRequestContext, expect } from "@playwright/test";
import compose from "docker-compose";
import path from "path";

export const repositoryRoot = path.join(__dirname, "..");

export const testStack = {
    cwd: repositoryRoot,
    config: "stacks/docker-compose.test.yml",
    // The compose file declares the Caddy host port as a required variable, so compose
    // refuses to start without the env file that sets it. The nested array keeps the
    // flag and its value as separate argv entries, and a compose option has to precede
    // the subcommand, which is where this library puts them.
    composeOptions: [["--env-file", "stacks/test.env"]],
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
