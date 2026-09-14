import { expect, test as setup } from "@playwright/test";
import compose from "docker-compose";
import fs from "fs";
import path from "path";

const repositoryRoot = path.join(__dirname, "..");

const testStack = {
    cwd: repositoryRoot,
    config: "stacks/docker-compose.test.yml",
    // The compose file declares the Caddy host ports as required variables, so compose
    // refuses to start without the env file that sets them. The nested array keeps the
    // flag and its value as separate argv entries, and a compose option has to precede
    // the subcommand, which is where this library puts them.
    composeOptions: [["--env-file", "stacks/test.env"]],
};

// The stack serves these out of the repository rather than out of an image, so without
// them it answers nothing and the wait below is all that would notice, a minute later.
const builtBundles = ["dist-client/index.html", "dist-server/server.js", "dist-test/discord-stub.js"];

setup("boot the test stack", async ({ request }) => {
    const missing = builtBundles.filter(bundle => !fs.existsSync(path.join(repositoryRoot, bundle)));
    if (missing.length > 0)
        throw new Error(`${missing.join(", ")} missing. Run ./build.sh before npm test.`);

    // -v takes the Postgres volume with it, so the stack below seeds a fresh database.
    await rethrowComposeError(() => compose.downAll({ ...testStack, commandOptions: ["-v"] }));
    await rethrowComposeError(() => compose.upAll(testStack));

    // compose returns once the containers start, which is well before the node container
    // has installed its dependencies and opened its port. Asking for a non-empty game
    // list rather than any answer at all also waits out the seed.
    await expect(async () => {
        const response = await request.get("/api/games", { timeout: 5_000 });
        expect(response.status()).toBe(200);
        expect((await response.json()).length).toBeGreaterThan(0);
    }).toPass({ timeout: 60_000, intervals: [500] });
});

// docker-compose rejects with its own `{ exitCode, out, err }` result rather than an Error,
// which Playwright reports as a failure carrying neither a message nor any compose output.
async function rethrowComposeError(command: () => Promise<unknown>): Promise<void> {
    try {
        await command();
    } catch (rejection) {
        const { err } = rejection as { err?: string };
        throw new Error(err || String(rejection));
    }
}
