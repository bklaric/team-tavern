import { defineConfig, devices } from "@playwright/test";

export default defineConfig({
    // Every spec shares the one test compose project, so they cannot run in parallel.
    workers: 1,
    // Opening the report server on a failure would block the terminal the run came from.
    reporter: [["html", { open: "never" }]],
    use: {
        // The test Caddy serves the site block `localhost` on a shifted HTTPS port under a
        // certificate it issues itself, so verification has to be off. Its port 80 redirect
        // drops the shifted port, which lands on the development stack, so talk to the
        // HTTPS port directly rather than letting a redirect pick the target.
        baseURL: "https://localhost:8443",
        ignoreHTTPSErrors: true,
        trace: "on-first-retry",
    },
    projects: [
        {
            name: "stack-setup",
            testDir: "./test-playwright",
            testMatch: /stack\.setup\.ts$/,
            // Room for `down -v` and `up -d` on top of the readiness deadline the setup
            // enforces itself, so that deadline is what reports a stack that never answers.
            timeout: 180_000,
        },
        {
            name: "integration",
            testDir: "./test-playwright/integration",
            use: { ...devices["Desktop Chrome"] },
            fullyParallel: false,
            dependencies: ["stack-setup"],
        },
    ],
});
