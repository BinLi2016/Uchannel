const path = require('path');
const fs = require('fs');
const { chromium } = require('playwright');

async function ensureDir(p) {
  await fs.promises.mkdir(p, { recursive: true });
}

async function waitForMxCad(page) {
  await page.waitForFunction(() => !!window.MxCAD && !!window.MxCAD.createMxCad, null, { timeout: 120000 });
  await page.waitForSelector('#myCanvas', { timeout: 60000 });

  // Wait until the MXCAD instance is actually available.
  await page.waitForFunction(() => {
    try {
      const MX = window.MxCAD;
      if (!MX || !MX.MxCpp || typeof MX.MxCpp.getCurrentMxCAD !== 'function') return false;
      return !!MX.MxCpp.getCurrentMxCAD();
    } catch (_) {
      return false;
    }
  }, null, { timeout: 120000 });
}

async function runCase(page, baseUrl, outDir, name, drawFnSource, settleMs = 15000) {
  await page.goto(baseUrl, { waitUntil: 'domcontentloaded' });
  await waitForMxCad(page);

  await page.evaluate(drawFnSource);
  await page.waitForTimeout(settleMs);

  const canvas = await page.$('#myCanvas');
  if (!canvas) throw new Error('Canvas #myCanvas not found');

  const fullPath = path.join(outDir, `${name}-full.png`);
  const canvasPath = path.join(outDir, `${name}-canvas.png`);

  await page.screenshot({ path: fullPath, fullPage: true });
  await canvas.screenshot({ path: canvasPath });

  // eslint-disable-next-line no-console
  console.log('Wrote screenshots:', { name, fullPath, canvasPath });
}

async function main() {
  const baseUrl = process.env.MXCAD_URL || 'http://127.0.0.1:8000/index.html';
  const outDir = process.env.OUT_DIR || path.join(__dirname, 'out');
  await ensureDir(outDir);

  const browser = await chromium.launch({ headless: true });
  const context = await browser.newContext({
    viewport: { width: 1800, height: 1000 },
    deviceScaleFactor: 1,
  });
  const page = await context.newPage();

  page.on('console', (msg) => {
    // eslint-disable-next-line no-console
    console.log(`[browser:${msg.type()}] ${msg.text()}`);
  });

  const cases = [
    {
      name: '01-uchannel-table1',
      fn: () => window.drawUChannelTable1(),
      settleMs: 15000,
    },
    {
      name: '02-uchannel-qty2-yu1',
      fn: () => window.drawUChannelQuantityTable2('YU1'),
      settleMs: 15000,
    },
    {
      name: '03-uchannel-qty3-yu5',
      fn: () => window.drawUChannelQuantityTable3('YU5'),
      settleMs: 15000,
    },
    {
      name: '04-uchannel-layout-sheet',
      fn: () => {
        const data = window.createUChannelSampleData('YU1');
        window.drawUChannelLayoutSheet(data);
      },
      settleMs: 15000,
    },
    {
      name: '05-drainage-motor-lane',
      fn: () => window.drawDrainageMotorLaneFromSample(),
      settleMs: 18000,
    },
    {
      name: '06-divider-belt-layout',
      fn: () => window.drawDividerBeltLayoutSheet(),
      settleMs: 15000,
    },
    {
      name: '07-divider-belt-quantity',
      fn: () => window.drawDividerBeltQuantityTables(),
      settleMs: 15000,
    },
    {
      name: '08-pump-station-drainage',
      fn: () => window.drawPumpStationDrainageFromSample(),
      settleMs: 18000,
    },
    {
      name: '09-curb-stone',
      fn: () => window.drawCurbStoneFromSample(),
      settleMs: 15000,
    },
  ];

  for (const c of cases) {
    // eslint-disable-next-line no-console
    console.log('Running case:', c.name);
    await runCase(page, baseUrl, outDir, c.name, c.fn, c.settleMs);
  }

  await browser.close();
}

main().catch((err) => {
  // eslint-disable-next-line no-console
  console.error(err);
  process.exitCode = 1;
});
