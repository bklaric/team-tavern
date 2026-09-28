// Copies every game's cover into the release, and beside it in `400/` the
// 400x600 copy the site's covers load wherever they are shown smaller than a
// tile on a wide phone. The originals stay for shared links, which want 600x900.
//
//   node build-covers.mjs <covers directory> <release directory>

import { copyFile, mkdir, readdir } from "node:fs/promises";
import { join } from "node:path";
import sharp from "sharp";

const [from, to] = process.argv.slice(2);
await mkdir(join(to, "400"), { recursive: true });

for (const file of await readdir(from)) {
    const cover = sharp(join(from, file));
    const { format, width, height } = await cover.metadata();
    if (format !== "webp" || width !== 600 || height !== 900)
        throw new Error(`${file} is a ${width}x${height} ${format}, where every cover is a 600x900 webp.`);
    await copyFile(join(from, file), join(to, file));
    await cover.resize(400, 600).webp({ quality: 70, effort: 6 }).toFile(join(to, "400", file));
}
