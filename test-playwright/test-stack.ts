import compose from "docker-compose";
import path from "path";

export const repositoryRoot = path.join(__dirname, "..");

export const testStack = {
    cwd: repositoryRoot,
    config: "stacks/docker-compose.test.yml",
    // The compose file declares the Caddy host ports as required variables, so compose
    // refuses to start without the env file that sets them. The nested array keeps the
    // flag and its value as separate argv entries, and a compose option has to precede
    // the subcommand, which is where this library puts them.
    composeOptions: [["--env-file", "stacks/test.env"]],
};

// Runs one statement against the test database and returns its rows, one string per row
// with columns separated by `|`. Credentials come from test.env, which the postgres
// container carries in its environment.
export async function queryTestDatabase(sql: string): Promise<string[]> {
    const result = await rethrowComposeError(() => compose.exec(
        "postgres",
        ["sh", "-c", `psql -U "$POSTGRES_USER" -d "$POSTGRES_DB" -AtX -v ON_ERROR_STOP=1 -c "$0"`, sql],
        testStack));
    return result.out.split("\n").filter(row => row !== "");
}

// docker-compose rejects with its own `{ exitCode, out, err }` result rather than an Error,
// which Playwright reports as a failure carrying neither a message nor any compose output.
export async function rethrowComposeError<T>(command: () => Promise<T>): Promise<T> {
    try {
        return await command();
    } catch (rejection) {
        const { err } = rejection as { err?: string };
        throw new Error(err || String(rejection));
    }
}
