# Public dashboard

The current Week 5 article link is:

https://rollinnolan9.github.io/CFBPlayoffPrediction/2026/week-5/

The site root redirects to the current card:

https://rollinnolan9.github.io/CFBPlayoffPrediction/

GitHub Pages publishes the root of the `gh-pages` branch. Its local checkout is
`../CFBPlayoffDashboardSite`, separate from the model checkout. It contains only
the generated dashboard, the root redirect, and static hosting configuration.
The API key, fitted model, source data, and local audit files are not uploaded.

## Refresh lines

Week 5 uses the frozen opponent-relative point forecast. Its publication command
is `Rscript cfb_v3/refresh_week5_relative_publication.R`. The Week 4 command below
is retained as the standard-model example; do not use it to replace the Week 5
opponent-relative projections.

From the model repository root, with `CFBD_API_KEY` available to R:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\cfb_v3\refresh_publication.R --predictions=cfb_v3/output/2026/week_4/live_live_2026_w04_20260924T173139Z.csv
```

The command preserves the saved projections and straight-up picks, recalculates
ATS picks using fresh DraftKings quotes, and writes a new timestamped directory
under `cfb_v3/output/2026/week_4_publication/`. It verifies every game's identity
and requires all games to be upcoming. Confidence and validation labels remain
omitted from the dashboard.

After checking the new `line_changes.csv`, `publication_manifest.json`, and
`dashboard.html`, copy that dashboard to
`../CFBPlayoffDashboardSite/2026/week-4/index.html`, commit it in that checkout,
and push `gh-pages`. Confirm that the public page shows the new capture time.
No automatic refresh has been scheduled.

## Model rankings link

The model Top 25 through Week 4 (Week 5 pregame) is published separately:

https://rollinnolan9.github.io/CFBPlayoffPrediction/2026/week-5/top-25/

The source is `output/2026/week_5_rankings/20260927_espn_supplement/index.html`.
The site copy adds canonical and social-sharing metadata; its body matches the
verified source exactly. `social-preview.png` is a deterministic 1200 x 630
graphic of the top five generated from that run's CSV. Paste the public URL
into a Substack Note; the image can also be uploaded directly to the Note.
The interactive dashboard runs on the linked website.

Published September 27, 2026 in `gh-pages` commit `7af7269`. Public HTML and
preview image were verified against the local files. Existing weekly picks
pages and the site root redirect remain unchanged. This page is a frozen
snapshot, including its timestamped AP-poll availability notice.

For future weeks, add a new week directory and update the root redirect. Keep
the Week 3 directory so existing article hyperlinks keep pointing to Week 3.

## September 18 publication

- Quotes retrieved September 18, 2026 at 19:20:58 UTC / 12:20:58 p.m. Pacific.
- 56 games; 11 changed spreads; zero changed ATS sides.
- Source: `output/2026/week_3_publication/20260918T192058.523Z/`.
- Deployed commit: `f6abbbe1f61eac5912b1cfa8972c29e56d525cef`.
- Public HTTPS access and all 56 displayed picks verified.

## September 25 publication

- Quotes retrieved September 25, 2026 at 19:44:53 UTC / 12:44:53 p.m. Pacific.
- 57 games; 18 changed spreads; one changed ATS side: California +2.5 replaced
  Clemson -1.5. Saved September 24 model projections and straight-up picks are unchanged.
- Source: `output/2026/week_4_publication/20260925T194453.667Z/`.
- Deployed commit: `ec111cf` on `gh-pages`; the root redirects to Week 4.
- Week 3 remains at its original article URL.
- All 57 displayed rows and the eight article matchups verified against the CSV;
  offline charts, filters, sorting, details and mobile layout passed.
- Public HTML matches the verified local output after normalizing line endings.
  Evidence: `qa/publication_verification.json` and `live_verification.json` in
  the publication directory.
- Reusable current-card browser check: `tests/check_current_publication.cjs`.
- Article graphics and their frozen source figures are in
  `../outputs/article-week-4/`; Nolan selected Fresno State -13.5 against Rice
  as Pick of the Week (Fresno State by 24.9; model-market gap 11.4 points).
  Its feature card is separate from the original eight-game picks board.

## October 2 publication

- Quotes retrieved October 2, 2026 at 19:02:39 UTC / 12:02:39 p.m. Pacific.
- 54 upcoming games; 33 changed spreads; two ATS side changes: Alabama -5.5
  replaced Mississippi State +6, and Ball State +21 replaced Toledo -19.5.
- Frozen September 30 opponent-relative projections and straight-up picks are
  unchanged. Injury adjustments and probability estimates remain absent.
- Source: `output/2026/week_5_publication/20261002T190239.907Z/`.
- Published commit `8123171` on `gh-pages`; the root redirects to Week 5.
- All displayed rows, filters, details, charts and mobile layouts passed the
  existing browser check. Public HTML matches the verified local dashboard
  after line-ending normalization. The existing Top 25 page remains available.
- Evidence: `qa/publication_verification.json` and `live_verification.json`.
- Article graphics: `../outputs/article-week-5/`, using the same quote snapshot
  and all seven saved original headshots. Nolan selected UNLV -2.5 against
  California as Pick of the Week (UNLV by 11.9; model-market gap 9.4 points).
  Its feature card is separate from the original ten-game picks board.

## October 4 rankings publication

- Public page: https://rollinnolan9.github.io/CFBPlayoffPrediction/2026/week-6/top-25/
- Source: `output/2026/week_6_rankings_relative/20261004T163129Z/`.
- Published `gh-pages` commit `fd2583f`; only the new HTML and social-preview PNG
  were added. The root continues to open the Week 5 picks, and previous URLs remain intact.
- Through Week 5: 138 teams, 390 completed games, and 18,906 ordered neutral
  matchups. The saved uncapped opponent-relative fit is reused, with both direct
  game-count predictors excluded. Arrows compare with the previous saved relative
  run, not the earlier public original-model rankings.
- Coverage: 270 required FBS games use official published EPA. Oregon State at
  Colorado State (401860899) uses a clearly labeled calculated supplement rebuilt
  from official CSU play-by-play after conflicting ESPN feeds were rejected.
  Last week's Oklahoma State-West Virginia supplement is replaced by official EPA.
- All 127 counted offensive plays match official statistics. The independent
  adapter audit checked all 155 modeled states, documented native scoring and
  end-of-half conventions, and verified four penalty-state corrections before EPA
  prediction. Model artifact hashes and all preexisting official data are unchanged.
- Corrected final rankings passed independent arithmetic, source coverage, record,
  movement, and displayed-value checks. Desktop/mobile interactions passed; live
  HTML and preview match the verified local files. Evidence: `independent_numeric_qa.json`,
  `espn_supplement/independent_adapter_qa.json`, `browser_qa.json`,
  `publication_verification.json`, and `live_verification.json`.
- The October 4 AP comparison was not verified from the official pages at
  18:01:42 UTC; the published snapshot marks comparison pending. AP voting is not
  a model input. No automatic poll refresh is scheduled.
