import { readFile } from "node:fs/promises";
import { createServer } from "node:http";
import { dirname, extname, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";

import { chromium } from "playwright";

const browserDir = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(browserDir, "../../..");
const contentTypes = new Map([
  [".html", "text/html; charset=utf-8"],
  [".js", "text/javascript; charset=utf-8"],
  [".mjs", "text/javascript; charset=utf-8"],
  [".json", "application/json; charset=utf-8"],
  [".map", "application/json; charset=utf-8"],
  [".wasm", "application/wasm"]
]);

const server = createServer(async (request, response) => {
  try {
    const url = new URL(request.url ?? "/", "http://127.0.0.1");
    const relative = decodeURIComponent(url.pathname).replace(/^\/+/, "");
    const file = resolve(repositoryRoot, relative);
    if (file !== repositoryRoot && !file.startsWith(`${repositoryRoot}${sep}`)) {
      response.writeHead(403).end("forbidden");
      return;
    }
    const body = await readFile(file);
    response.writeHead(200, {
      "content-type": contentTypes.get(extname(file)) ?? "application/octet-stream",
      "cache-control": "no-store"
    });
    response.end(body);
  } catch (error) {
    response.writeHead(error?.code === "ENOENT" ? 404 : 500).end(String(error));
  }
});

await new Promise((accept, reject) => {
  server.once("error", reject);
  server.listen(0, "127.0.0.1", accept);
});

const address = server.address();
if (typeof address !== "object" || address === null) {
  await new Promise(resolveClose => server.close(resolveClose));
  throw new Error("browser smoke server did not expose a TCP address");
}

let browser;
let context;
try {
  browser = await chromium.launch({
    headless: true,
    args: [
      "--enable-unsafe-swiftshader",
      "--ignore-gpu-blocklist",
      "--use-angle=swiftshader"
    ]
  });
  context = await browser.newContext();
  const page = await context.newPage();
  const browserErrors = [];
  page.on("pageerror", error => browserErrors.push(error.stack ?? String(error)));
  page.on("console", message => {
    if (message.type() === "error") browserErrors.push(message.text());
  });
  await page.goto(
    `http://127.0.0.1:${address.port}/modules/surface-view-three/browser/smoke.html`,
    { waitUntil: "networkidle", timeout: 120_000 }
  );
  await page.waitForFunction(
    () => ["pass", "fail"].includes(document.body.dataset.scalafimThreeSmoke),
    undefined,
    { timeout: 120_000 }
  );
  const state = await page.locator("body").getAttribute("data-scalafim-three-smoke");
  const receipt = await page.evaluate(() => window.scalafimThreeSmoke);
  if (browserErrors.length > 0) {
    throw new Error(`browser emitted errors:\n${browserErrors.join("\n")}`);
  }
  if (state !== "pass" || receipt?.status !== "pass") {
    throw new Error(`Three.js smoke failed:\n${JSON.stringify(receipt, null, 2)}`);
  }
  process.stdout.write(`${JSON.stringify(receipt, null, 2)}\n`);
} finally {
  try {
    await context?.close();
  } finally {
    try {
      await browser?.close();
    } finally {
      await new Promise(resolveClose => server.close(resolveClose));
    }
  }
}
