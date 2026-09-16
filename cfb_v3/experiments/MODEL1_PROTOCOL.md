# Archived Model (1) Replay

The user confirmed the January 2025 `model (1).Rmd` at commit `365ca12` and that
the reported 11-0 bracket was frozen before the 2024-season playoff began.
The previous five-game, round-updated reconstruction cannot disprove that record.
In particular, replacing same-season SP+ with previous-season SP+ materially changed
the old model. No historical replay should be described as the published record.

## Frozen Original Workflow

Read and execute only the original notebook's data-table transformations and
`predict_winner` function. Never execute API pulls, package installation, or arbitrary
notebook statements. Use the actual six-game window, 2014-2021 training range,
fixed mtry 6, 500 trees, default node size 5, selected 19 training columns including
the outcome, and original team exclusions. Since mtry is fixed, skip the reporting-
only 10-fold CV; the final forest uses that same setting. Seeds 20260909, 20260910,
and 20260911 are fixed before fitting, not selected for agreement with 11-0.

Use cached original PBP/SP/Elo from `.RData`, and the January 2025 Git copies of
the available PFF, coach, and injury CSVs under their archived filenames. Preserve
the original joins and report their row multiplicity; do not add predictors or
silently deduplicate the notebook's training rows. Verify the selected columns.

Restrict 2024 PBP to games before the first CFP kickoff, then build ONE prediction
table for the whole bracket. Preserve the original latest-home/latest-away row
selection. On neutral sites average the home orientation and the sign-reversed
away orientation, matching the user's original practice. First-round campus games
keep their actual orientation. Never update inputs with any 2024 CFP result.

Same-season SP+ and historical weekly Elo are available only as later saved tables,
not verified original December release snapshots. This replay may therefore include
hindsight in those ratings. Its results are archival diagnostics, NOT a leakage-free
backtest, proof of the reported 11-0, or grounds to promote a model. The exact fitted
forest, seed, and input snapshots from December 2024 have not been recovered.

## Frozen V3 Reference

Rebuild the 2024 pre-playoff matchup features from the existing v3 foundation with
one fixed cutoff for all rounds. Do not allow a first-round result into a later
round's features. Use the existing historical 2024 fold settings, chosen using
earlier seasons, and train only through 2023. Verify first-round predictions against
the saved rolling reference. Keep the round-updated table separate from this frozen
comparison. No production code, data, original notebook, or card is overwritten.

## Reporting

Lead with 2024 CFP straight-up picks, actual winner, both orientations where relevant,
and per-seed records. Distinguish actual-matchup accuracy from a frozen bracket that
advances each model's own predicted winners; wrong earlier picks change later opponents.
Generate that bracket without using game outcomes to choose who advances.
Report inability to reproduce 11-0 honestly rather than tuning until it appears.
Do not claim either reconstruction is the user's original published record.

No new bowl exclusion, PFF experiment, parameter search, or promotion is authorized
by this replay. Other years/full-season ATS are secondary to resolving this specific
frozen-bracket comparison and are not rerun here.

## Evidence Addendum After The First Replay

The first completed replay returned 11-0 in all three predeclared seeds. Subsequent
visual inspection found the saved 11-0 bracket on page 18 of the original presentation
`CFB Playoff  Bracket Prediction.pdf`. It matches the January Git archive byte-for-byte;
its creation metadata is December 13, 2024. This corroborates the user's account, but
does not restore the exact fitted forest or establish an independently timestamped
publication. This discovery did not change model settings or seed selection.

Missing 2024 PBP kickoff dates are recovered by game ID from the cached schedule.
Unresolved dates must not involve CFP participants; any other omitted games are logged.
The verified rerun removes empty rows produced by R's NA logical subsetting from the
v3 cutoff table, certifying a finite last-source timestamp as well as prediction parity.
