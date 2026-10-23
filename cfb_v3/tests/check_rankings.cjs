// Offline check: node check_rankings.cjs OUTPUT_DIRECTORY PLAYWRIGHT_MODULE
const fs = require('node:fs');
const path = require('node:path');
const assert = require('node:assert/strict');
const { pathToFileURL } = require('node:url');
const { chromium } = require(process.argv[3] || 'playwright');
const directory = path.resolve(process.argv[2]);
const metadata = JSON.parse(fs.readFileSync(path.join(directory, 'qa.json'), 'utf8').replace(/^\uFEFF/, ''));

(async () => {
  const browser = await chromium.launch({ channel: 'msedge', headless: true });
  const errors = [], dimensions = [];
  try {
    const page = await browser.newPage({ viewport: { width: 1440, height: 1000 } });
    page.on('pageerror', error => errors.push(error.message));
    await page.route(/^https?:/, route => route.abort());
    await page.goto(pathToFileURL(path.join(directory, 'index.html')).href);
    const displayed = await page.locator('#rankings tr').evaluateAll(rows => rows.map(row => ({
      rank: Number(row.dataset.rank), team: row.querySelector('.team').textContent,
      record: row.querySelector('.record').textContent,
      rating: row.querySelector('.rating').firstChild.textContent,
      movement: row.querySelector('.movement')?.textContent || '',
      rating_change: row.querySelector('.rating-change')?.textContent || ''
    })));
    assert.equal(displayed.length, metadata.team_count);
    assert.equal(new Set(displayed.map(row => row.team)).size, metadata.team_count);
    displayed.forEach((row, i) => {
      assert.equal(row.rank, i + 1);
      assert(/^\d+–\d+$/.test(row.record));
      assert(Number.isFinite(Number(row.rating)));
    });
    const visible = () => page.locator('#rankings tr:visible').count();
    assert.equal(await visible(), 25);
    await page.locator('#all-view').click();
    assert.equal(await visible(), metadata.team_count);
    await page.locator('#top-view').click();
    await page.locator('#search').fill(displayed.at(-1).team);
    assert(await page.locator('#rankings tr:visible .team').allTextContents().then(names => names.includes(displayed.at(-1).team)));
    await page.locator('#search').fill('no_such_team_xyz');
    assert.equal(await visible(), 0);
    assert(await page.locator('#no-results').isVisible());
    await page.locator('#top-view').click();
    assert.equal(await visible(), 25);
    for (const width of [1440, 390]) {
      await page.setViewportSize({ width, height: 1000 });
      const size = await page.evaluate(() => ({ viewport: innerWidth, content: document.documentElement.scrollWidth }));
      assert(size.content <= size.viewport, `Horizontal overflow: ${JSON.stringify(size)}`);
      dimensions.push(size);
      await page.screenshot({ path: path.join(directory, width === 390 ? 'mobile_rankings.png' : 'desktop_rankings.png'), fullPage: true });
    }
    await page.locator('summary').click();
    assert(await page.locator('details').evaluate(element => element.open));
    if (fs.existsSync(path.join(directory, 'social-preview.html'))) {
      await page.setViewportSize({ width: 1200, height: 630 });
      await page.goto(pathToFileURL(path.join(directory, 'social-preview.html')).href);
      await page.evaluate(() => document.fonts.ready);
      assert.equal(await page.locator('.row').count(), 5);
      assert.deepEqual(await page.locator('.row .team').allTextContents(), displayed.slice(0, 5).map(row => row.team));
      await page.screenshot({ path: path.join(directory, 'social-preview.png') });
    }
    assert.deepEqual(errors, []);
    fs.writeFileSync(path.join(directory, 'browser_qa.json'), JSON.stringify({
      passed: true, checked_at: new Date().toISOString(), offline: true,
      checks: ['top25/all teams', 'search outside top25', 'empty search', 'reset', 'details', 'desktop/mobile layout', 'social preview when supplied'],
      dimensions, page_errors: errors, displayed_rows: displayed
    }, null, 2));
    console.log(`Rankings QA passed: ${displayed.length} teams, 25 default rows, desktop/mobile, offline interactions.`);
  } finally { await browser.close(); }
})().catch(error => { console.error(error); process.exitCode = 1; });
