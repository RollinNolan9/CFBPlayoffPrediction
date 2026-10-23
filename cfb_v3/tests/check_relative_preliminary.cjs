// Generic offline check adapted from the verified Week 5 preliminary dashboard audit.
const fs = require('node:fs');
const path = require('node:path');
const assert = require('node:assert/strict');
const crypto = require('node:crypto');
const { pathToFileURL } = require('node:url');
const { chromium } = require(process.argv[3] || 'playwright');
const output = path.resolve(process.argv[2]);
const metadata = JSON.parse(fs.readFileSync(path.join(output, 'qa.json'), 'utf8'));
const qaDir = path.join(output, 'qa');
fs.mkdirSync(qaDir, { recursive: true });
const source = path.join(output, 'predictions.csv'), dashboard = path.join(output, 'dashboard.html');
const baselinePath = metadata.source_card;
function csv(file) {
  const text = fs.readFileSync(file, 'utf8').replace(/^\uFEFF/, '');
  const records = []; let row = [], field = '', quoted = false;
  for (let i = 0; i < text.length; i++) {
    const char = text[i];
    if (char === '"') { if (quoted && text[i+1] === '"') { field += '"'; i++; } else quoted = !quoted; }
    else if (!quoted && [',','\n','\r'].includes(char)) {
      row.push(field); field = '';
      if (char !== ',') { if (char === '\r' && text[i+1] === '\n') i++; if (row.some(Boolean)) records.push(row); row = []; }
    } else field += char;
  }
  assert(!quoted); if (field || row.length) { row.push(field); records.push(row); }
  const header = records.shift();
  return records.map(values => { assert.equal(values.length, header.length); return Object.fromEntries(header.map((k,i) => [k, values[i]])); });
}
const num = x => x === '' || x === 'NA' || x == null ? NaN : Number(x);
const one = x => x.toFixed(1);
const hash = file => crypto.createHash('sha256').update(fs.readFileSync(file)).digest('hex');
const status = x => ({official_pick:'Model pick', article_pick:'Article pick', forced_model_pick:'Article pick', large_spread_review:'Large spread', injury_conflict_review:'Injury review', transition_review:'Transition review'}[x] || 'Pass');
const expected = csv(source);
const baseline = new Map(csv(baselinePath).map(r=>[r.game_id,r]));
const comparison = new Map(csv(path.join(output,'comparison.csv')).map(r=>[r.game_id,r]));
const errors = [], checks = [], dimensions = [], filters = [];
(async () => {
  assert.equal(metadata.status, 'complete'); assert.equal(expected.length, metadata.predictions); assert.equal(new Set(expected.map(r=>r.game_id)).size,expected.length);
  const browser = await chromium.launch({headless:true, channel:'msedge'});
  try {
    const page = await browser.newPage({viewport:{width:1440,height:1000}});
    page.on('pageerror', e=>errors.push(e.message));
    await page.route(/^https?:/, route=>route.abort());
    await page.goto(pathToFileURL(dashboard).href); await page.waitForSelector('#picks-table');
    assert.deepEqual((await page.locator('#picks-table thead th').allTextContents()).map(x=>x.trim()),['Game','Market','Model','ATS side','Edge','SU pick','Status','']);
    const actual = await page.locator('#picks-table tbody tr').evaluateAll(rows=>rows.map(row=>({
      cells:Array.from(row.querySelectorAll(':scope > td'),c=>c.innerText.trim()),
      home:row.querySelector('.home-team').textContent.trim(),away:row.querySelector('.away-team').textContent.trim(),
      edge:Number(row.dataset.edge),order:Number(row.dataset.order),details:row.querySelector('.row-details').textContent,
      components:Array.from(row.querySelectorAll('.detail-panel > div')).find(el=>el.querySelector('strong')?.textContent==='Components')?.querySelector('p')?.textContent
    })));
    assert.equal(actual.length,expected.length);
    actual.forEach((row,i)=>{
      const want=expected[i], margin=num(want.expected_margin), spread=num(want.market_home_spread), homeEdge=margin+spread;
      assert.equal(row.home,want.home); assert.equal(row.away,want.away); assert.equal(row.order,i+1);
      assert(row.cells[0].includes(want.kickoff_display),`Kickoff display ${want.game_id}`);
      assert(Number.isFinite(margin) && Number.isFinite(spread));
      const pick=homeEdge>=0?want.home:want.away, selectedSpread=pick===want.home?spread:-spread;
      assert.equal(want.ats_pick,pick); assert.equal(want.straight_up_pick,margin>=0?want.home:want.away);
      const values={1:`${spread<=0?want.home:want.away} ${one(spread<=0?spread:-spread)}`,2:`${margin>=0?want.home:want.away} -${one(Math.abs(margin))}`,3:`${pick} ${selectedSpread>0?'+':''}${one(selectedSpread)}`,4:`+${one(Math.abs(homeEdge))}`,5:want.straight_up_pick,6:status(want.pick_status)};
      for (const [column,value] of Object.entries(values)) assert.equal(row.cells[Number(column)],value,`${want.game_id} column ${column}`);
      assert(Math.abs(row.edge-Math.abs(homeEdge))<=1e-6,`Edge ${want.game_id}`);
      assert(!/\b(confidence|unvalidated|uncalibrated)\b/i.test(row.cells.join(' ')+row.details));
      assert.equal(want.as_of, metadata.as_of); assert(Date.parse(want.market_captured_at) <= Date.parse(want.as_of)); assert(Date.parse(want.kickoff) > Date.parse(want.as_of));
      assert(want.line_source && want.line_source !== 'NA');
      assert(row.details.includes(want.top_drivers),`Drivers ${want.game_id}`);
      assert.equal(row.components,`Foundation ${values[2]}`,`Components ${want.game_id}`);
      assert(!/\b(NA|NaN|Infinity|Inf)\b/.test(row.cells.join(' ')+row.details),`Invalid metric ${want.game_id}`);
      const old=baseline.get(want.game_id), paired=comparison.get(want.game_id);
      assert(old && paired); assert.equal(want.home,old.home); assert.equal(want.away,old.away);
      assert.equal(spread,num(old.market_home_spread)); assert.equal(want.market_captured_at,old.market_captured_at);
      assert(Math.abs(margin-num(paired.opponent_relative_margin))<1e-10);
      assert.equal(paired.ats_side_changed==='TRUE',want.ats_pick!==old.ats_pick);
      assert.equal(paired.su_changed==='TRUE',want.straight_up_pick!==old.straight_up_pick);
    });
    const body=await page.locator('body').innerText();
    assert(body.replace(/\s+/g,' ').toLowerCase().includes(`${metadata.season} week ${metadata.week}`));
    // The dashboard labels the market retrieval time; the model cutoff is checked per CSV row above.
    assert(body.includes(expected[0].market_captured_at)); assert(!/\b(confidence|unvalidated|uncalibrated)\b/i.test(body));
    assert(body.includes(expected[0].publication_note));
    assert(!/\b(NA|NaN|Infinity|Inf)\b/.test(body));
    assert.equal(expected.filter(r=>r.ats_pick!==baseline.get(r.game_id).ats_pick).length,metadata.ats_side_changes);
    assert.equal(expected.filter(r=>r.straight_up_pick!==baseline.get(r.game_id).straight_up_pick).length,metadata.su_changes);
    checks.push('Every CSV row: identities, market/model/ATS/SU/status, numeric edge, kickoff and quote timestamps, confidence labels omitted');
    checks.push('Preliminary notice, feature drivers, component display, finite values, unchanged baseline lines and independently confirmed side-change counts');
    async function noOverflow(label) { const d=await page.evaluate(()=>({width:document.documentElement.clientWidth,scroll:document.documentElement.scrollWidth})); assert(d.scroll<=d.width+1,`Overflow ${label} ${JSON.stringify(d)}`); dimensions.push({label,...d}); }
    await noOverflow('desktop'); await page.screenshot({path:path.join(qaDir,'dashboard_desktop.png')});
    const visible=()=>page.locator('#picks-table tbody tr:not([hidden])').count();
    for(const name of ['article','review','primetime','all']) {
      const button=page.locator(`[data-filter="${name}"]`); if(await button.count()===0)continue;
      await button.click();
      const count=name==='all'?expected.length:expected.filter(r=>name==='article'?['article_pick','forced_model_pick'].includes(r.pick_status):name==='primetime'?r.primetime==='TRUE':r.pick_status.includes('review')).length;
      assert.equal(await visible(),count); filters.push({name,count});
    }
    await page.locator('[data-filter="all"]').click();
    const query=expected[0].home; await page.locator('#pick-search').fill(query);
    assert.equal(await visible(),expected.filter(r=>`${r.away} ${r.home} ${r.ats_pick} ${r.data_flag}`.toLowerCase().includes(query.toLowerCase())).length);
    await page.locator('#pick-search').fill('qa-no-such-team'); assert.equal(await visible(),0);
    await page.locator('#pick-search').fill(''); assert.equal(await visible(),expected.length);
    for(const direction of ['edge_desc','edge_asc']) {
      await page.locator('#pick-sort').selectOption(direction);
      const edges=await page.locator('#picks-table tbody tr').evaluateAll(rows=>rows.map(r=>Number(r.dataset.edge)));
      assert(edges.every((e,i)=>!i||(direction==='edge_desc'?e<=edges[i-1]:e>=edges[i-1])));
    }
    await page.locator('#pick-sort').selectOption('original');
    assert.deepEqual(await page.locator('#picks-table tbody tr').evaluateAll(rows=>rows.map(r=>Number(r.dataset.order))),expected.map((_,i)=>i+1));
    await page.locator('#picks-table tbody tr').first().locator('summary').click(); assert.equal(await page.locator('.row-details[open]').count(),1);
    await page.keyboard.press('Escape'); assert.equal(await page.locator('.row-details[open]').count(),0);
    checks.push('article/review/primetime/all filters, populated/empty search, ascending/descending edge sort, original order, details and Escape');
    await page.getByRole('tab',{name:'Model vs Market',exact:true}).click();
    await page.waitForFunction(()=>Array.from(document.querySelectorAll('#model-vs-market img')).every(img=>img.complete));
    const chartImages=await page.locator('#model-vs-market img').evaluateAll(images=>images.map(img=>({width:img.naturalWidth,height:img.naturalHeight})));
    assert(chartImages.length>=2 && chartImages.every(img=>img.width>0 && img.height>0));
    await noOverflow('desktop charts');
    await page.screenshot({path:path.join(qaDir,'dashboard_charts.png')});
    await page.getByRole('tab',{name:'Availability',exact:true}).click();
    assert(await page.locator('#availability').isVisible());
    await noOverflow('desktop availability');
    await page.getByRole('tab',{name:'Slate',exact:true}).click();
    checks.push('Model vs Market charts load offline and Availability tab opens');
    for(const width of [390,320]) {
      await page.setViewportSize({width,height:844}); await noOverflow(`mobile ${width}`);
      await page.locator('#pick-search').fill(query); assert.equal(await visible(),expected.filter(r=>`${r.away} ${r.home} ${r.ats_pick} ${r.data_flag}`.toLowerCase().includes(query.toLowerCase())).length); await page.locator('#pick-search').fill('');
      await page.locator('#picks-table tbody tr').first().locator('summary').click(); assert.equal(await page.locator('.row-details[open]').count(),1);
      await noOverflow(`mobile ${width} details`); await page.keyboard.press('Escape');
      if(width===390) {
        await page.locator('#picks-table').evaluate(table=>{for(let p=table.parentElement;p;p=p.parentElement)p.scrollLeft=0;});
        await page.screenshot({path:path.join(qaDir,'dashboard_mobile.png')});
      }
    }
    checks.push('desktop 1440px/mobile 390px and 320px: no page overflow, mobile search and details');
    assert.deepEqual(errors,[]);
    const report={passed:true,verified_at:new Date().toISOString(),rows:expected.length,market_captured_at:expected[0].market_captured_at,checks,filters,dimensions,page_errors:errors,offline:true,sources:{predictions:source,predictions_sha256:hash(source),dashboard,dashboard_sha256:hash(dashboard),audit_sha256:hash(__filename)}};
    fs.writeFileSync(path.join(qaDir,'browser_qa.json'),JSON.stringify(report,null,2)); console.log(JSON.stringify(report,null,2));
  } finally {await browser.close();}
})().catch(e=>{fs.writeFileSync(path.join(qaDir,'browser_qa_failure.json'),JSON.stringify({error:e.stack},null,2));console.error(e);process.exitCode=1;});
