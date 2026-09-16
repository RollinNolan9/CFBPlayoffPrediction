# Bounded Play Cleanup Protocol

Production v3 and published cards remain frozen. This is an offline correctness
experiment, not deployment. The source family and individual EPA values remain
unchanged. Historical source captures are reconstructed inputs, not proof of the
information available at every original publication timestamp.

## Fixed Comparisons

1. Replay frozen v3 using original inputs and saved past-only fold choices.
2. Remove only exact complete-source duplicates after proving compact-cache parity.
3. Apply the conservative shared event classifier on the deduplicated source.
   Change only turnover/havoc rates; preserve the original competitive scrimmage
   finite-EPA denominator. Eligibility contradictions and unresolved potentially
   eligible events invalidate affected team-game rate cells instead of becoming
   zero or shrinking the denominator. Existing past-only summaries and training
   imputation handle those missing cells; their coverage is reported explicitly.
4. Remove turnover/havoc predictors from the duplicate-only candidate as a fixed
   ablation, with all other fit settings unchanged.

No penalty selection, coach split, population, line, phase-weight or EPA changes.
Use the same 2,955 qualified-quote 2022-2025 games. Report all-game selections,
season, phase, CFP, neutral CFP and large-spread metrics. No subgroup promotion.
The reused historical years are not untouched validation, and historical quotes
lack verified execution timestamps and actual prices. -110 returns are illustrative.

## Audit Boundary

Reuse the fixed 888-team-game CFBD box sample, while applying the same classifier
to 2020-2025 inputs for consistent chronological training and prior formation.
Preserve raw rows and source hashes. Full-game reported turnovers are a check,
not a substitute for play events, filtered havoc, or EPA. Matching totals do not
prove event identities; unknowns and contradictory metadata remain review items.
Do not adjust rules based on ATS outcomes. Do not deploy while source/classifier
limits remain unexplained. Any further rule change requires rerunning this audit.
