const path = require('path');
const { chromium } = require('playwright');

async function main() {
  const baseUrl = process.env.MXCAD_URL || 'http://127.0.0.1:8000/index.html';
  const outDir = process.env.OUT_DIR || __dirname;

  const browser = await chromium.launch({ headless: true });
  const context = await browser.newContext({
    viewport: { width: 1600, height: 900 },
    deviceScaleFactor: 1,
  });
  const page = await context.newPage();

  page.on('console', (msg) => {
    // Useful when debugging flaky wasm loads.
    // eslint-disable-next-line no-console
    console.log(`[browser:${msg.type()}] ${msg.text()}`);
  });

  await page.goto(baseUrl, { waitUntil: 'domcontentloaded' });

  // Wait for mxdraw/mxcad UMD globals.
  await page.waitForFunction(() => !!window.MxCAD && !!window.MxCAD.createMxCad, null, { timeout: 120000 });

  // Wait for canvas to exist.
  await page.waitForSelector('#myCanvas', { timeout: 60000 });

  // Trigger the motor-lane sample render.
  await page.evaluate(() => window.drawDrainageMotorLaneFromSample());

  // Give wasm + drawing pipeline time to settle.
  await page.waitForTimeout(15000);

  const canvas = await page.$('#myCanvas');
  if (!canvas) throw new Error('Canvas #myCanvas not found');

  const fullPath = path.join(outDir, 'motor-lane-full.png');
  const canvasPath = path.join(outDir, 'motor-lane-canvas.png');

  await page.screenshot({ path: fullPath, fullPage: true });
  await canvas.screenshot({ path: canvasPath });

  // eslint-disable-next-line no-console
  console.log('Wrote screenshots:', { fullPath, canvasPath });

  await browser.close();
}

main().catch((err) => {
  // eslint-disable-next-line no-console
  console.error(err);
  process.exitCode = 1;
});
