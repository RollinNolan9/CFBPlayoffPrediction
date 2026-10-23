"""Rebuild the bounded injury pilot from retained local evidence; no network calls."""
from pathlib import Path
import json
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[2]
EXPERIMENTS = ROOT / "cfb_v3/experiments"
OUTPUT = ROOT / "cfb_v3/output/experiments/injury_pilot"


def run(script, *args):
    subprocess.run([sys.executable, "-B", str(EXPERIMENTS / script), *map(str, args)],
                   cwd=ROOT, check=True)


def main():
    # Cases were selected from dated availability evidence before evaluation.
    sample = [
        ("401628414", "Mississippi", "2024_ole_miss_asof_20241108T200000Z"),
        ("401752739", "Texas A&M", "2025_texas_a_m_asof_20251017T190000Z"),
        ("401628403", "Arkansas", "2024_arkansas_asof_20241025T190000Z"),
    ]
    histories = [{"game_id": game, "team": team,
                  "history_dir": str(OUTPUT / "player_data" / f"history_{suffix}_raw_involvement_ppa")}
                 for game, team, suffix in sample]
    required = [OUTPUT / "baseline/baseline.csv", OUTPUT / "baseline/schedule.csv",
                OUTPUT / "source_data/reports.csv", OUTPUT / "source_data/player_statuses.csv",
                OUTPUT / "source_data/coverage.csv"]
    required += [Path(row["history_dir"]) / "manifest.json" for row in histories]
    missing = [str(path) for path in required if not path.exists()]
    if missing:
        raise SystemExit("Retained evidence is required; no automatic fetching occurs:\n" + "\n".join(missing))
    config = OUTPUT / "histories.json"
    config.write_text(json.dumps(histories, indent=2), encoding="utf-8")
    run("injury_case_features.py", "--schedule", OUTPUT / "baseline/schedule.csv",
        "--reports", OUTPUT / "source_data/reports.csv",
        "--statuses", OUTPUT / "source_data/player_statuses.csv",
        "--histories", config, "--output", OUTPUT / "features")
    evaluations = OUTPUT / "evaluations"
    before = set(evaluations.iterdir()) if evaluations.exists() else set()
    run("injury_evaluate.py", "--baseline", OUTPUT / "baseline/baseline.csv",
        "--cases", OUTPUT / "features/case_features.csv",
        "--coverage", OUTPUT / "source_data/coverage.csv", "--output", evaluations)
    created = set(evaluations.iterdir()) - before
    if len(created) != 1:
        raise RuntimeError("Cannot uniquely identify this evaluation output")
    result = created.pop()
    run("injury_report.py", "--evaluation", result)
    print(f"Report: {result / 'report.html'}")


if __name__ == "__main__":
    main()
