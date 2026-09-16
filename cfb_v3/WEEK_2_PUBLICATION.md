# Week 2 Publication Build

This is a publication-only build for the September 9, 2026 v3 card. It does not
retrain the model, refresh team features, or assign points to injury reports.

From the repository root, with R and Quarto installed:

```powershell
Rscript cfb_v3/refresh_week2_publication_lines.R
Rscript cfb_v3/build_week2_publication.R cfb_v3/output/2026/week_2_publication/<snapshot-directory>
```

The first command prints the new snapshot directory. The second explicitly uses
that directory and fails on missing/duplicate DraftKings lines, mismatched teams,
missing evidence rows, or quotes captured after kickoff. It can also re-render
an existing snapshot without another network call. The CFBD key is read from
the normal R environment and is never written into publication files.

Outputs:

- `dashboard.html`: self-contained, read-only 49-game dashboard.
- `primetime_summary.html` and `.md`: separate 22-game evening brief.
- `predictions.csv`: frozen projected margins repriced at the snapshot spreads.
- `line_changes.csv`: old/new spreads, sides, and point differences for every game.
- `market_snapshot.csv` and `cfbd_lines_raw.rds`: source quote evidence.
- `publication_manifest.json`: source checksums and build counts.

September 11 snapshot: DraftKings via CFBD, retrieved at 19:55:49 UTC
(3:55 p.m. Eastern). All 49 games had quotes; 27 spreads moved and no ATS sides
flipped. No FanDuel quotes or spread prices were returned. The timestamp is
retrieval time, not the sportsbook's last-update time. Check the live book before
publishing or placing a wager.

The frozen ATS calibration is absent. The dashboard therefore displays
`Unvalidated`, not the neutral 50% fallback as though it were a calibrated estimate.
Article selections are preserved, but no selection is promoted to a best bet.
Availability notes are selective reporting context, not a complete injury audit.
Missing current FBS efficiency samples are visible even when the old data flag
said `standard`. The original prediction CSV and fitted model are untouched.

Broadcast metadata and editorial context live in `week2_publication_context.json`.
Hawaii's advertised 6 p.m. local start is displayed as midnight Sunday Eastern,
correcting the cached one-minute-before-midnight placeholder for presentation.

Tests:

```powershell
Rscript cfb_v3/tests/test_publication.R
Rscript cfb_v2/tests/test_v2.R
node cfb_v3/tests/check_publication.cjs <snapshot-directory> <path-to-playwright>
```

The browser check uses headless Edge, blocks external HTTP requests, verifies all
49 rendered rows against the CSV-derived expectations, and exercises filtering,
search, sort, details, charts, and mobile layouts. These files are local artifacts;
building them does not publish a public URL or push anything to GitHub.
