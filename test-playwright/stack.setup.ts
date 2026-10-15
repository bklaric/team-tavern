import { test as setup } from "@playwright/test";
import compose from "docker-compose";
import fs from "fs";
import path from "path";
import { localStack, rethrowComposeError, waitForApi } from "./stack";

// The stack runs the local build rather than images, so without it the stack answers
// nothing and the wait below is all that would notice, a minute later. The stand-ins are
// what tell a local build from a server's.
const builtFiles = [
    "docker-compose.yml", ".env", "Caddyfile", "base.Caddyfile",
    "client/index.html", "server/server.js",
    "discord-stub/main.js", "mail-stub/main.js", "steam-stub/main.js", "google-stub/main.js"];

setup("boot the local stack", async ({ request }) => {
    const missing = builtFiles.filter(file => !fs.existsSync(path.join(localStack.cwd, file)));
    if (missing.length > 0)
        throw new Error(`dist/local lacks ${missing.join(", ")}. Run .\\bundle.ps1 -environment local before npm test.`);

    // Down and up again, so every container starts afresh, the mail stub with no mail.
    // --remove-orphans takes the containers of services the compose file no longer
    // names, which would otherwise keep their names and the host port.
    await rethrowComposeError(() => compose.downAll({ ...localStack, commandOptions: ["--remove-orphans"] }));
    await rethrowComposeError(() => compose.upAll(localStack));

    // Postgres keeps its cluster between runs, so the site's database is built anew from
    // the seed rather than thrown away with a volume.
    await rethrowComposeError(() => compose.exec("tt-postgres", ["bash", "/test-seed/seed.sh", "--fresh"], localStack));

    await waitForApi(request);
});
