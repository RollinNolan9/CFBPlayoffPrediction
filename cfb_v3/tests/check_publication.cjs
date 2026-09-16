const fs = require('node:fs');
const path = require('node:path');
const { pathToFileURL } = require('node:url');
const assert = require('node:assert/strict');
const { chromium } = require(process.argv[3] || 'playwright');
const output = path.resolve(process.argv[2]);
const expected = JSON.parse(fs.readFileSync(path.join(output, 'verification_expected.json'), 'utf8'));

(async () => {
  const browser = await chromium.launch({ headless: true, channel: 'msedge' });
  try {
    const page = await browser.newPage({ viewport: { width: 1440, height: 1000 } });
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    // A standalone dashboard must still work without network access.
    await page.route(/^https?:/, route => route.abort());
    await page.goto(pathToFileURL(path.join(output, 'dashboard.html')).href);
    await page.waitForSelector('#picks-table');
    const actual = await page.locator('#picks-table tbody tr').evaluateAll(rows => rows.map(row => ({
      cells: Array.from(row.querySelectorAll(':scope > td'), cell => cell.innerText),
      primetime: row.dataset.primetime
    })));
    assert.equal(actual.length, 49);
    actual.forEach((row, i) => {
      const want = expected[i];
      assert.equal(row.cells[1], want.market_line);
      assert.equal(row.cells[2], want.model_line);
      assert.equal(row.cells[3], want.ats_pick_line);
      assert.equal(row.cells[6], want.straight_up_pick);
      assert(row.cells[0].includes(want.kickoff_display));
      assert.equal(row.primetime, String(want.primetime));
    });
    await page.screenshot({ path: path.join(output, 'dashboard_desktop.png'), fullPage: true });
    await page.locator('[data-filter="primetime"]').click();
    assert.equal(await page.locator('#picks-table tbody tr:not([hidden])').count(), 22);
    await page.locator('#pick-search').fill('Ohio State');
    assert.equal(await page.locator('#picks-table tbody tr:not([hidden])').count(), 1);
    await page.locator('#pick-search').fill('no-such-team');
    assert.equal(await page.locator('#picks-table tbody tr:not([hidden])').count(), 0);
    await page.locator('#pick-search').fill('');
    await page.locator('[data-filter="all"]').click();
    await page.locator('#pick-sort').selectOption('edge_desc');
    const edges = await page.locator('#picks-table tbody tr').evaluateAll(rows => rows.map(row => Number(row.dataset.edge)));
    assert(edges.every((value, i) => i === 0 || value <= edges[i - 1]));
    await page.locator('#pick-sort').selectOption('original');
    await page.locator('#picks-table tbody tr').first().locator('summary').click();
    assert.equal(await page.locator('.row-details[open]').count(), 1);
    await page.keyboard.press('Escape');
    assert.equal(await page.locator('.row-details[open]').count(), 0);
    await page.getByRole('tab', { name: 'Model vs Market', exact: true }).click();
    const images = await page.locator('#model-vs-market img').evaluateAll(images => images.map(img => ({
      width: img.naturalWidth, height: img.naturalHeight
    })));
    assert(images.length >= 3 && images.every(img => img.width > 0 && img.height > 0));
    await page.screenshot({ path: path.join(output, 'dashboard_charts.png'), fullPage: true });
    await page.getByRole('tab', { name: 'Availability', exact: true }).click();
    assert(await page.getByText('AJ Surace will start', { exact: false }).count() > 0);
    await page.screenshot({ path: path.join(output, 'dashboard_availability.png'), fullPage: true });
    await page.getByRole('tab', { name: 'Slate', exact: true }).click();
    for (const width of [390, 768]) {
      await page.setViewportSize({ width, height: 844 });
      await page.screenshot({ path: path.join(output, `dashboard_${width}.png`), fullPage: true });
      const dimensions = await page.evaluate(() => ({ width: document.documentElement.clientWidth, scroll: document.documentElement.scrollWidth }));
      assert(dimensions.scroll <= dimensions.width + 1, `Page overflow at ${width}: ${JSON.stringify(dimensions)}`);
      await page.locator('[data-filter="primetime"]').click();
      assert.equal(await page.locator('#picks-table tbody tr:not([hidden])').count(), 22);
      await page.locator('[data-filter="all"]').click();
    }
    await page.goto(pathToFileURL(path.join(output, 'primetime_summary.html')).href);
    assert.equal(await page.locator('h3').count(), 22);
    await page.setViewportSize({ width: 1000, height: 1000 });
    await page.screenshot({ path: path.join(output, 'primetime_summary_preview.png'), fullPage: false });
    assert.deepEqual(errors, []);
    fs.writeFileSync(path.join(output, 'browser_verification.json'), JSON.stringify({
      rows: 49, primetime: 22, numericParity: true, offline: true,
      viewports: [1440, 768, 390], search: true, sort: true, details: true, chartsLoaded: true,
      summaryGames: 22, pageErrors: errors
    }, null, 2));
    console.log('Publication browser checks passed: 49 rows, 22 evening games, offline rendering, desktop/mobile controls.');
  } finally {
    await browser.close();
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
