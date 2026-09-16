# Isolated Play Reconciliation

This helper audits supplied classifications. It does not classify events, remove
duplicates, modify EPA, write source files, or fit models. Production remains
outside its ownership.

## Public Interface

Load `cfb_v2/store.R`, `cfb_v2/features.R`, `cfb_v2/historical_data.R`,
`matchup_drive_audit.R`, and `play_cleanup_audit.R` from the project root.

```r
result <- play_cleanup_reconcile(classified_pbp, boxes)
```

The required classifier contract is the original full-source columns plus
`clean_giveaway` (numeric 0/1/NA), `clean_scrimmage` (logical TRUE/FALSE/NA),
`clean_status` (resolved/unresolved), `clean_reason`, and `clean_evidence`.
Original source fields remain unchanged. Missing status, reason or evidence
cannot certify an event count.

Required box columns: `game_id`, `season`, `team`, `turnovers`, `interceptions`,
`fumbles_lost`, `status`, `duplicate_status`. Preserve the original qualification
columns even when the caller has already selected matched/unique rows. Optional
home/away fields help identify unassignable possession teams.

Returned objects:

- `team_game_reconciliation`: one normalized team-game-season key, lower bounds,
  unknown counts, source and box status, full-game versus filtered counts,
  eligibility conflicts, blockers, and explicit count-only agreement.
- `exception_queue`: full event identity, original flags/EPA/text, classifier
  reasons/evidence, raw drive metadata, nearby same-game descriptions and
  team-level review records. Even missing/unlocalized events produce a review.
- `play_ledger`: the bounded source identity/context plus all classifier fields
  and derived audit masks, including original input row positions.
- `box_ledger`: every supplied box row with its normalized audit key; duplicate
  keys are quarantined, never silently joined or removed.
- `drive_ledger`: the existing drive helper's corroborating evidence, with season.
  It never assigns a drive-level event to an invented play.
- `limits`: interpretation and scope warnings for downstream reports.

`play_cleanup_model_masks(classified_pbp)` exposes
`audit_legacy_model_eligible`, the unchanged historical competitive/scrimmage/
finite-EPA gate. `audit_clean_model_eligible` is a separate diagnostic only;
the comparison must not substitute it for the legacy denominator.

`play_cleanup_read_audit_boxes(project)` optionally verifies and reads the sealed
20260912T032310.128Z box ledger, restricting it to the existing 888-team-game
cohort. This convenience reader also requires `external_verify` to be loaded.
The main orchestrator may pass its own equivalently qualified box table instead.

## Scope And Interpretation

The pure helper scopes source plays to supplied game IDs before expensive full-row
checks. Original input row numbers survive that subset. Unkeyed source rows remain
visible and block batch-level count agreement because their membership is unknown.
Combined years are supported; the orchestrator can process one season at a time.

All-game counts are computed before football-model filters. Resolved giveaways
excluded by the legacy scrimmage, competitive, or finite-EPA gates are displayed
separately from unresolved events. Missing EPA is an explicit exclusion reason,
not evidence that no giveaway occurred. Scrimmage disagreements are flagged, not
used to change the model denominator. Unknown counts describe observed rows;
missing source events do not have a defensible finite upper bound.

`count_agreement` may be TRUE while `count_reconciled` is FALSE: the known lower
bound can equal a box total despite unknown events or conflicting evidence.
Even a clean count-only agreement leaves `event_verification_complete=FALSE`.
Offsetting errors can cancel, and CFBD/ESPN may share the same underlying provider.

The helper does not authorize duplicate removal. It flags full-source duplicates
left in its input; the caller must prove compact-cache parity and remove only
verified complete-row duplicates before classification. Distinct plays sharing
numeric play IDs remain separate. Conflicting detailed identities are quarantined.

## Verification

Run from the repository root:

```r
source("cfb_v2/tests/test_play_cleanup_audit.R")
```

Fixtures test resolved/excluded/unknown counts; missing source, statuses and
evidence; conflicting and duplicate box keys; exact source duplicates versus
colliding numeric IDs; legacy filter parity; scrimmage/drive conflicts; scoped
combined years; empty inputs; evidence retention; and supplied Michigan/LSU
witness decisions. They validate reconciliation, not the other agent's classifier.
