// Offline QA for a completed publication. Usage:
// node check_current_publication.cjs <publication-dir> [playwright-module] [expected-rows] [article-pairs.json]
const fs = require('node:fs');
const path = require('node:path');
const crypto = require('node:crypto');
const assert = require('node:assert/strict');
const { pathToFileURL } = require('node:url');
const { chromium } = require(process.argv[3] || 'playwright');
const output = path.resolve(process.argv[2]);
const qa = path.join(output, 'qa');
fs.mkdirSync(qa, { recursive: true });

function csv(file) {
  const text = fs.readFileSync(file, 'utf8').replace(/^\uFEFF/, '');
  const records = []; let row = [], field = '', quoted = false;
  for (let i = 0; i < text.length; i++) {
    const char = text[i];
    if (char === '"') {
      if (quoted && text[i + 1] === '"') { field += '"'; i++; }
      else quoted = !quoted;
    } else if (!quoted && (char === ',' || char === '\n' || char === '\r')) {
      row.push(field); field = '';
      if (char !== ',') {
        if (char === '\r' && text[i + 1] === '\n') i++;
        if (row.some(Boolean)) records.push(row);
        row = [];
      }
    } else field += char;
  }
  assert(!quoted, `Unclosed CSV quote: ${file}`);
  if (field || row.length) { row.push(field); records.push(row); }
  const header = records.shift();
  return records.map(values => {
    assert.equal(values.length, header.length, `CSV field count: ${file}`);
    return Object.fromEntries(header.map((key, i) => [key, values[i]]));
  });
}
const truth = value => /^(true|1)$/i.test(String(value));
const one = value => Number(value).toFixed(1);
const hash = (file, algorithm = 'sha256') => crypto.createHash(algorithm).update(fs.readFileSync(file)).digest('hex');

(async () => {
  const manifest = JSON.parse(fs.readFileSync(path.join(output, 'publication_manifest.json'), 'utf8'));
  assert.equal(manifest.status, 'complete');
  assert.equal(manifest.model_refit, false);
  const expectedRows = Number(process.argv[4] || manifest.predictions);
  const expected = csv(path.join(output, 'predictions.csv'));
  assert.equal(expected.length, expectedRows);
  assert.equal(manifest.predictions, expectedRows);
  assert.equal(new Set(expected.map(row => row.game_id)).size, expectedRows);
  for (const source of manifest.sources) assert.equal(hash(source.path, 'md5'), source.md5, source.role);
  const frozenPath = manifest.sources.find(source => source.role === 'predictions').path;
  const frozen = new Map(csv(frozenPath).map(row => [row.game_id, row]));
  const snapshot = csv(path.join(output, 'market_snapshot.csv'));
  const snapshotByGame = new Map(snapshot.map(row => [row.game_id, row]));
  assert.equal(snapshotByGame.size, expectedRows);
  const capture = Date.parse(manifest.captured_at);
  assert(Number.isFinite(capture));
  assert(capture > Date.parse(manifest.frozen_model_as_of), 'Refreshed market must follow the frozen model');
  let maxMarginDifference = 0;
  let changedSpreads = 0, changedSides = 0;
  const easternHour = new Intl.DateTimeFormat('en-US', { timeZone: 'America/New_York', hour: 'numeric', hourCycle: 'h23' });
  for (const row of expected) {
    const old = frozen.get(row.game_id);
    assert(old, `Frozen game ${row.game_id}`);
    for (const field of ['home', 'away', 'season', 'week', 'straight_up_pick']) assert.equal(row[field], old[field], `${row.game_id} ${field}`);
    const margin = Number(row.expected_margin), spread = Number(row.market_home_spread);
    assert(Number.isFinite(margin) && Number.isFinite(spread));
    maxMarginDifference = Math.max(maxMarginDifference, Math.abs(margin - Number(old.expected_margin)));
    assert.equal(row.as_of, manifest.frozen_model_as_of);
    assert.equal(row.market_captured_at, manifest.captured_at);
    assert(Date.parse(row.kickoff) > capture, `Kickoff already passed at capture: ${row.game_id}`);
    assert.equal(Number(row.season), manifest.season);
    assert.equal(Number(row.week), manifest.week);
    const quote = snapshotByGame.get(row.game_id);
    assert.equal(Number(quote.home_spread), spread, `${row.game_id} snapshot spread`);
    assert.equal(quote.captured_at, manifest.captured_at);
    assert.equal(row.market_line, `${spread <= 0 ? row.home : row.away} ${one(spread <= 0 ? spread : -spread)}`);
    assert.equal(row.model_line, `${margin >= 0 ? row.home : row.away} -${one(Math.abs(margin))}`);
    assert.equal(row.straight_up_pick, margin >= 0 ? row.home : row.away);
    assert.equal(row.ats_pick, margin + spread >= 0 ? row.home : row.away);
    const pickSpread = row.ats_pick === row.home ? spread : -spread;
    assert.equal(row.ats_pick_line, `${row.ats_pick} ${pickSpread > 0 ? '+' : ''}${one(pickSpread)}`);
    const hour = Number(easternHour.format(new Date(row.kickoff)));
    assert.equal(truth(row.primetime), hour >= 19 || hour < 3, `${row.game_id} evening flag`);
    if (spread !== Number(old.market_home_spread)) changedSpreads++;
    if (row.ats_pick !== old.ats_pick) changedSides++;
  }
  assert(maxMarginDifference <= 1e-10, 'Frozen model margins changed beyond CSV serialization precision');
  assert.equal(changedSpreads, manifest.changed_spreads);
  assert.equal(changedSides, manifest.changed_sides);
  const evening = expected.filter(row => truth(row.primetime));
  assert.equal(evening.length, manifest.evening_games);
  const pairs = process.argv[5] ? JSON.parse(fs.readFileSync(process.argv[5], 'utf8')) : [];
  const articleMatchups = pairs.map(pair => {
    const matches = expected.filter(row => row.away === pair.away && row.home === pair.home);
    assert.equal(matches.length, 1, `Article matchup identity: ${pair.away} at ${pair.home}`);
    const row = matches[0];
    return Object.fromEntries(['game_id', 'away', 'home', 'kickoff', 'market_line', 'model_line', 'ats_pick_line', 'straight_up_pick'].map(key => [key, row[key]]));
  });

  const browser = await chromium.launch({ headless: true, channel: 'msedge' });
  const errors = [], checks = [], dimensions = [];
  try {
    const page = await browser.newPage({ viewport: { width: 1440, height: 1000 } });
    page.on('pageerror', error => errors.push(error.message));
    await page.route(/^https?:/, route => route.abort());
    await page.goto(pathToFileURL(path.join(output, manifest.dashboard)).href);
    await page.waitForSelector('#picks-table');
    const headers = await page.locator('#picks-table thead th').allTextContents();
    assert.deepEqual(headers.map(text => text.trim()), ['Game', 'Market', 'Model', 'ATS side', 'Edge', 'SU pick', 'Status', '']);
    const actual = await page.locator('#picks-table tbody tr').evaluateAll(rows => rows.map(row => ({
      cells: Array.from(row.querySelectorAll(':scope > td'), cell => cell.innerText.trim()),
      away: row.querySelector('.away-team').textContent.trim(), home: row.querySelector('.home-team').textContent.trim(),
      primetime: row.dataset.primetime, edge: Number(row.dataset.edge), order: Number(row.dataset.order),
      details: row.querySelector('.row-details').textContent
    })));
    assert.equal(actual.length, expectedRows);
    actual.forEach((row, i) => {
      const want = expected[i];
      assert.equal(row.order, i + 1);
      assert.equal(row.home, want.home); assert.equal(row.away, want.away);
      for (const [column, field] of [[1, 'market_line'], [2, 'model_line'], [3, 'ats_pick_line'], [5, 'straight_up_pick'], [6, 'status_label']]) {
        assert.equal(row.cells[column], want[field], `${want.game_id} ${field}`);
      }
      assert(row.cells[0].includes(want.kickoff_display));
      assert.equal(row.primetime, String(truth(want.primetime)));
      assert(Math.abs(row.edge - Number(want.pick_edge)) <= 0.000001);
      assert(!/\b(confidence|unvalidated|uncalibrated)\b/i.test(row.cells.join(' ') + row.details), `Unexpected validation label: ${want.game_id}`);
    });
    const text = await page.locator('body').innerText();
    assert(text.replace(/\s+/g, ' ').toLowerCase().includes(`${manifest.season} week ${manifest.week}`));
    assert(text.includes(manifest.captured_at)); assert(text.includes(manifest.frozen_model_as_of));
    assert(!/\b(confidence|unvalidated|uncalibrated)\b/i.test(text));
    checks.push('all displayed identities, market/model/ATS/SU/status values, eight-column layout, model/market timestamps');
    async function noOverflow(width) {
      const value = await page.evaluate(() => ({ width: document.documentElement.clientWidth, scroll: document.documentElement.scrollWidth }));
      assert(value.scroll <= value.width + 1, `Page overflow at ${width}: ${JSON.stringify(value)}`);
      dimensions.push(value);
    }
    await noOverflow(1440);
    await page.screenshot({ path: path.join(qa, 'dashboard_desktop.png'), fullPage: true });
    await page.locator('[data-filter="primetime"]').click();
    assert.equal(await page.locator('#picks-table tbody tr:not([hidden])').count(), evening.length);
    const query = evening[0].home;
    await page.locator('#pick-search').fill(query);
    const matching = expected.filter(row => truth(row.primetime) && `${row.game_label} ${row.ats_pick_line} ${row.data_flag}`.toLowerCase().includes(query.toLowerCase())).length;
    assert.equal(await page.locator('#picks-table tbody tr:not([hidden])').count(), matching);
    await page.locator('#pick-search').fill('no-such-team-qa');
    assert.equal(await page.locator('#picks-table tbody tr:not([hidden])').count(), 0);
    await page.locator('#pick-search').fill('');
    await page.locator('[data-filter="all"]').click();
    for (const direction of ['edge_desc', 'edge_asc']) {
      await page.locator('#pick-sort').selectOption(direction);
      const values = await page.locator('#picks-table tbody tr').evaluateAll(rows => rows.map(row => Number(row.dataset.edge)));
      assert(values.every((value, i) => i === 0 || (direction === 'edge_desc' ? value <= values[i - 1] : value >= values[i - 1])));
    }
    await page.locator('#pick-sort').selectOption('original');
    assert.deepEqual(await page.locator('#picks-table tbody tr').evaluateAll(rows => rows.map(row => Number(row.dataset.order))), expected.map((_, i) => i + 1));
    await page.locator('#picks-table tbody tr').first().locator('summary').click();
    assert.equal(await page.locator('.row-details[open]').count(), 1);
    await page.keyboard.press('Escape');
    assert.equal(await page.locator('.row-details[open]').count(), 0);
    checks.push('combined evening/search filters, empty search, ascending/descending edge sort, original order, details and Escape');
    await page.getByRole('tab', { name: 'Model vs Market', exact: true }).click();
    await page.waitForFunction(() => Array.from(document.querySelectorAll('#model-vs-market img')).every(img => img.complete));
    const chartImages = await page.locator('#model-vs-market img').evaluateAll(images => images.map(img => ({ width: img.naturalWidth, height: img.naturalHeight })));
    assert(chartImages.length >= 2 && chartImages.every(img => img.width > 0 && img.height > 0));
    await page.screenshot({ path: path.join(qa, 'dashboard_charts.png'), fullPage: true });
    await page.getByRole('tab', { name: 'Availability', exact: true }).click();
    assert(await page.locator('#availability').isVisible());
    await page.screenshot({ path: path.join(qa, 'dashboard_availability.png'), fullPage: true });
    await page.getByRole('tab', { name: 'Slate', exact: true }).click();
    await page.setViewportSize({ width: 390, height: 844 });
    await noOverflow(390);
    await page.screenshot({ path: path.join(qa, 'dashboard_mobile.png'), fullPage: true });
    await page.locator('[data-filter="primetime"]').click();
    assert.equal(await page.locator('#picks-table tbody tr:not([hidden])').count(), evening.length);
    await page.locator('[data-filter="all"]').click();
    await page.locator('#picks-table tbody tr').first().locator('summary').click();
    assert.equal(await page.locator('.row-details[open]').count(), 1);
    await noOverflow(390);
    await page.screenshot({ path: path.join(qa, 'dashboard_mobile_details.png'), fullPage: true });
    assert.deepEqual(errors, []);
    checks.push('offline charts and availability tabs, desktop/mobile layout, mobile filter/details');
    fs.writeFileSync(path.join(qa, 'publication_verification.json'), JSON.stringify({
      passed: true, verified_at: new Date().toISOString(), rows: expectedRows, evening_games: evening.length,
      market_captured_at: manifest.captured_at, frozen_model_as_of: manifest.frozen_model_as_of,
      max_margin_difference: maxMarginDifference, changed_spreads: changedSpreads, changed_sides: changedSides,
      chart_images: chartImages.length, offline: true, checks, dimensions, article_matchups: articleMatchups,
      sources: { predictions_sha256: hash(path.join(output, 'predictions.csv')), dashboard_sha256: hash(path.join(output, manifest.dashboard)),
        frozen_predictions_sha256: hash(frozenPath), qa_helper_sha256: hash(__filename) }, page_errors: errors
    }, null, 2));
    const failurePath = path.join(qa, 'publication_verification_failure.json');
    if (fs.existsSync(failurePath)) fs.unlinkSync(failurePath);
    console.log(`Publication QA passed: ${expectedRows} rows, ${evening.length} evening games, ${articleMatchups.length} article matchups, frozen margins unchanged.`);
  } finally { await browser.close(); }
})().catch(error => {
  fs.writeFileSync(path.join(qa, 'publication_verification_failure.json'), JSON.stringify({ error: error.stack }, null, 2));
  console.error(error); process.exitCode = 1;
});
