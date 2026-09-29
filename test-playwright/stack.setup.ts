import { test as setup } from "@playwright/test";
import compose from "docker-compose";
import fs from "fs";
import path from "path";
import { repositoryRoot, rethrowComposeError, testStack, waitForApi } from "./stack";

// The stack serves these out of the repository rather than out of an image, so without
// them it answers nothing and the wait below is all that would notice, a minute later.
const builtBundles = [
    "release/client/index.html", "release/server/server.js", "release/caddy/base.Caddyfile",
    "dist-test/discord-stub.js", "dist-test/mail-stub.js"];

setup("boot the test stack", async ({ request }) => {
    const missing = builtBundles.filter(bundle => !fs.existsSync(path.join(repositoryRoot, bundle)));
    if (missing.length > 0)
        throw new Error(`${missing.join(", ")} missing. Run ./build.sh before npm test.`);

    // -v takes the Postgres volume with it, so the stack below seeds a fresh database.
    // --remove-orphans takes the containers of services the compose file no longer
    // names, which would otherwise keep the host port.
    await rethrowComposeError(() => compose.downAll({ ...testStack, commandOptions: ["-v", "--remove-orphans"] }));
    await rethrowComposeError(() => compose.upAll(testStack));

    await waitForApi(request);
});
