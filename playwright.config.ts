import { defineConfig, devices } from "@playwright/test";

export default defineConfig({
    // Every spec shares the one local stack, so they cannot run in parallel.
    workers: 1,
    // Opening the report server on a failure would block the terminal the run came from.
    reporter: [["html", { open: "never" }]],
    use: {
        // The host port stacks/local.docker-compose.yml publishes Caddy on.
        baseURL: "http://localhost:8020",
        trace: "on-first-retry",
    },
    projects: [
        {
            name: "stack-setup",
            testDir: "./test-playwright",
            testMatch: /stack\.setup\.ts$/,
            // Room for `down`, `up -d` and the seed on top of the readiness deadline the setup
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
