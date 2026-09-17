import { test as setup } from "@playwright/test";
import compose from "docker-compose";
import fs from "fs";
import path from "path";
import { repositoryRoot, rethrowComposeError, testStack, waitForApi } from "./stack";

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

    await waitForApi(request);
});
