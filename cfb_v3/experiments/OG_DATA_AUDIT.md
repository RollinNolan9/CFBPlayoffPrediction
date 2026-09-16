# OG Historical Input Audit

Reviewed September 9, 2026. Scope: establish sources before fitting/scoring a fair
six-game OG versus v3 comparison. Production models, data and cards are unchanged.

## Important Correction

The cached 2024 SP+ inputs used by the new six-game replay match Bill Connelly's
**post-playoff `FBS FINAL` table for all 134 FBS teams, in all three numeric fields**.
Seven explicit source-name aliases resolve the initial 127 direct matches. The
extra cached `nationalAverages` row is not a team and has no publisher-team match.

For example, the final table lists Ohio State at 31.2 overall, 39.5 offense, 9.2
defense, with its completed 14-2 record. All three values equal the cached inputs.
The proof is the downloaded [author's 2024 workbook](https://docs.google.com/spreadsheets/d/1CJImfkg0ouHIIIGOWRfbvwC0TNWh76n47xkz8nqrVBc/edit),
its source hash, and the per-team `2024_final_sp_parity.csv` artifact.

**Our 11-0 replay cannot establish a pregame advantage over v3's 10-1.** Freezing
play-by-play did not remove hindsight from the annual ratings. The saved original
presentation still records the user's 11-0 bracket; this does not prove the user's
original run used final ratings. The fitted original forest and its exact live
inputs are still missing. [MODEL1_RESULTS.md](MODEL1_RESULTS.md) and future replay
report warnings have been corrected; old generated artifacts are preserved.

## Local Coverage

| Season | Eligible games | CFP | Pregame Elo, both teams | Closing lines | Six-game history ceiling |
|---|---:|---:|---:|---:|---:|
| 2022 | 737 | 3 | 737 | 737 | 391 |
| 2023 | 753 | 3 | 753 | 752 | 392 |
| 2024 | 763 | 11 | 763 | 763 | 412 |
| 2025 | 773 | 11 | 773 | 773 | 416 |
| Total | 3,026 | 28 | 3,026 | 3,025 | 1,611 |

Evaluation includes 38 conference championships and excludes non-CFP bowls.
No training/history bowl policy changed. Six-game counts require six finite
same-season EPA/WPA team-game rows strictly before Monday 00:00 UTC. They are an
upper bound, not exact OG coverage: original side-specific lookup, exclusions and
trained factor levels may reduce it. Early weeks cannot become an OG preseason
model merely because SP+ is now available.

Both v2/v3 public-rating CSVs and DuckDB rating tables were empty. Cached annual SP+
has team/year/three ratings but no historical availability or data-through times.
All cached annual rows fail the evidence gate. The generated audit's zero accepted
SP+ count describes these local inputs, not the public sources recovered afterward.

Only 108 of 5,916 comparable regular-season team-game rows match between the old
same-week Elo cache and explicitly pregame schedule Elo. This is a mapping diagnostic,
not proof of which endpoint vintage the original author used. Resolve weekly
semantics and the original latest-completed-game lookup before replacing values.
[CFBD games documentation](https://api.collegefootballdata.com/api/games) explicitly
distinguishes pregame and postgame Elo.

Historical closing spreads are available; historical Friday DraftKings/FanDuel
price snapshots are not established. Closing-line ATS is a retrospective comparison,
not evidence of returns at prices actually available for publication.

## Public Sources Recovered

The user has no ESPN+ subscription. Public author-shared workbooks were downloaded
without credentials; no paywall was bypassed. All files have retrieval timestamps
and SHA-256 hashes. Dates of discovery/download were not backdated.

| Source | Numeric FBS components recovered | Missing or unverified |
|---|---|---|
| [2022 workbook](https://docs.google.com/spreadsheets/d/1llrN8luL0XWuP8Y-Pb1NXKU84JhXLeUPafy1RfITEDw/edit) | Preseason; Weeks 2-8, 131 teams per table | Week 1 and Weeks 9 onward lack components in this workbook; publication evidence pending |
| [2023 published workbook](https://docs.google.com/spreadsheets/d/e/2PACX-1vRh9Slymcisd5-uEIvAD4zjGkJ7aeARseChhne-HdpyQeQiTSJeZD0WfyuG40O5S7Z20wz1XLYSUDUj/pubhtml) | Weeks 1-14, 133 teams per table | Bowl tab contains picks but not rating components; publication evidence pending |
| [2024 workbook](https://docs.google.com/spreadsheets/d/1CJImfkg0ouHIIIGOWRfbvwC0TNWh76n47xkz8nqrVBc/edit) | Weeks 1-9, 134 teams per table; final table retained only for contamination audit | Weeks 10 onward and bowl tab lack FBS components; final table prohibited for pregame replay |
| [2025 workbook](https://docs.google.com/spreadsheets/d/1a6hboWNnPeUzx5oUEjwJwf9vw4DuAV7lTaW4Q92Zpls/edit) | Weeks 0-15 plus bowl/Week 16 preview, 136 teams per table | Exact per-table publication/data cutoffs and later edits pending review |
| [2026 workbook](https://docs.google.com/spreadsheets/d/1vwoVl-Dxy0es87Z9I1RTvFzr72Lb1fAkREfbLxbK-eg/edit) | Weeks 0-2, 138 teams per table | Useful for prospective capture, not 2022-2025 historical scoring |
| [CFB TXT snapshots](https://cfbtxt.com/data/) | 2026 overall SP+ snapshots with dates; 1,380 SP+ rows | No offense/defense components, no 2022-2025 coverage in the downloaded file |

The extracted candidate file contains **6,976 team-snapshot rows**, all three ratings
finite. That includes 134 explicitly rejected final-2024 rows and 414 current-2026
rows; it is not 6,976 accepted historical observations. Every candidate remains
`evidence_reviewed = FALSE`, with availability/data-through timestamps unset.

FBS component tables can sit beside predictions on the same sheet, and column
positions change across years. Extraction uses actual column headers, checks
one row per canonical team per table, and does not substitute predictions, ordinal
ranks, lower-division ratings or the differently centered all-divisions scale.
Some spreadsheet record cells were auto-converted to dates; they are not trusted
as a machine-readable record history without separate validation.

Discovery provenance: the 2022/2023 links resolve from the author's public
[2022 short link](https://t.co/ld8Z9RqL9c) and [2023 short link](https://t.co/fjsWyQPXfP).
The author also shared the 2024 workbook in a dated
[November 17, 2024 post](https://bsky.app/profile/espnbillc.bsky.social/post/3lb6kz6kfw22w),
and the 2025 workbook in a [January 21, 2026 post](https://bsky.app/profile/espnbillc.bsky.social/post/3mcwuxsfgfs2q).
These establish the author's links, not unchanged historical contents of every tab.

## What CFBD Can And Cannot Supply

The [official SP+ API documentation](https://api.collegefootballdata.com/api/ratings)
exposes season/team queries, not a weekly or as-of query for SP+. Re-querying a past
season does not recover an earlier week's vintage. The Elo endpoint has week and
season-type parameters, but source/cutoff alignment still needs verification.
No authenticated requests or API credits were used for this audit.

Dated 2023/2024 ESPN article leads were found but their full tables required ESPN+.
A current mutable 2025 rankings hub now shows final ratings. Neither was treated
as an available historical snapshot. Third-party annual rankings and overall-only
archives were discovery leads, not substitutes for the OG's exact predictors.

## Next Decision

1. Prioritize publication/data-cutoff validation of the recovered **2023 and 2025**
   weekly tables. Cross-check dated author releases and historical game records;
   label reconstruction assumptions and any unrecoverable edits explicitly.
2. Resolve Elo timing and measure exact side-specific OG coverage. Missing weeks
   remain missing; do not carry ratings forward for months or pick replacements
   after seeing model results. Report missing CFP coverage separately.
3. Recover late-2022 and late-2024 components, particularly the pre-2024-CFP table.
   Only score games passing the [locked protocol](OG_COMPARISON_PROTOCOL.md).
   Disclose narrower coverage if a full four-season comparison remains impossible.

No multi-season six-game fitting/scoring, new ATS verdict, bowl experiment or model
promotion occurred. This audit advances source recovery and corrects the earlier
replay interpretation; it does not establish which model is better.

## Reproduction

Run from the repository root. Local audit is offline; source recovery needs internet
and the existing `httr`, `readxl`, `xml2`, `stringr`, `digest`, `jsonlite`, `rvest` packages.

```powershell
Rscript run_cfb_og_audit.R
# Replace AUDIT_RUN with the directory printed by the audit.
Rscript cfb_v3/experiments/recover_public_sp.R AUDIT_RUN
Rscript cfb_v3/experiments/inspect_recovered_sp.R AUDIT_RUN/public_sources
Rscript cfb_v2/tests/test_og_audit.R
```

Completed source capture and inspection:
`cfb_v3/output/experiments/og_audit/20260909T203217Z/public_sources/`.
`inventory.json` records source URLs/hashes and all workbook tabs; `inspection/`
contains table coverage, normalized candidates, and final-2024 parity evidence.
Downloads/derived rows are ignored by Git; scripts and this report are portable.
Run directories are not overwritten. Older `.RDataTmp` backups were not decoded;
filesystem dates were not treated as publication evidence.

Final offline audit rerun: `20260909T203958Z`. Eight audit tests and seven existing
replay tests pass; `git diff --check` passes. Existing R locale/package-build
warnings are environmental. Production engine/data/card hashes and the v2 database
hash are unchanged. New source scripts, audit outputs and report corrections were
not committed or pushed.
