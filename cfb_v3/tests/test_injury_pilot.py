"""Leakage, sign and paired-evaluation checks for the isolated injury pilot."""
import importlib.util
from pathlib import Path
import unittest

import numpy as np
import pandas as pd

module_path = Path(__file__).resolve().parents[1] / "experiments/injury_evaluate.py"
spec = importlib.util.spec_from_file_location("injury_evaluate", module_path)
pilot = importlib.util.module_from_spec(spec)
spec.loader.exec_module(pilot)


class InjuryPilotTests(unittest.TestCase):
    def setUp(self):
        self.baseline = pd.DataFrame({
            "game_id": ["1", "2", "3"], "season": [2024, 2025, 2025],
            "home": ["A", "A", "A"], "away": ["B", "B", "B"],
            "cutoff_utc": ["2024-10-11T19:00:00Z", "2025-10-10T19:00:00Z", "2025-10-10T19:00:00Z"],
            "expected_margin": [5., 5., 7.], "actual_margin": [3., 9., 7.],
            "reference_home_spread": [-4., -4., -7.],
        })
        self.cases = pd.DataFrame([{
            "game_id": str(game), "team": "A", "player_id": "100", "player_name": "Runner",
            "status": "Out", "position": "RB", "report_published_at": f"{year}-10-09T23:00:00Z",
            "source_url": "https://example.test/report", "prior_games": 4,
            "expected_involvements": 10., "player_ppa": .3, "replacement_ppa": .1,
            "raw_loss_points": 2., "latest_history_kickoff": f"{year}-10-04T16:00:00Z",
            "played_latest_game": True, "team_report_complete": True,
        } for game, year in [(1, 2024), (2, 2025)]])

    def test_2025_outcomes_do_not_choose_adjustment(self):
        rows, results, scale = pilot.evaluate(self.baseline, self.cases)
        self.assertEqual(scale, 1.)  # 2024 improves, although 2025 worsens.
        self.assertEqual(rows.loc[1, "margin_1"], 3.)
        self.assertEqual(rows.loc[2, "margin_1"], 7.)
        self.assertEqual(int(rows.affected.sum()), 2)
        self.assertEqual(int(results.games.max()), 1)
        self.assertTrue(rows.loc[0, "ats_side_changed_1"])

    def test_report_after_friday_cutoff_is_rejected(self):
        self.cases.loc[0, "report_published_at"] = "2024-10-12T12:00:00Z"
        with self.assertRaisesRegex(ValueError, "future-informed"):
            pilot.evaluate(self.baseline, self.cases)

    def test_target_game_stats_are_rejected(self):
        self.cases.loc[0, "latest_history_kickoff"] = "2024-10-12T16:00:00Z"
        with self.assertRaisesRegex(ValueError, "future-informed"):
            pilot.evaluate(self.baseline, self.cases)

    def test_signed_away_adjustment_is_preserved(self):
        self.cases["team"] = "B"
        self.cases["replacement_ppa"] = .5
        self.cases["raw_loss_points"] = -2.
        rows, _, _ = pilot.evaluate(self.baseline, self.cases)
        self.assertTrue(np.allclose(rows.loc[:1, "margin_1"], 3.))

    def test_persistent_absence_is_rejected(self):
        self.cases.loc[0, "played_latest_game"] = False
        with self.assertRaisesRegex(ValueError, "future-informed"):
            pilot.evaluate(self.baseline, self.cases)

    def test_pushes_are_not_wins(self):
        result = pilot.metrics(self.baseline.iloc[[2]], "expected_margin")
        self.assertEqual(result["ats_pushes"], 1)
        self.assertEqual(result["ats_wins"], 0)
        self.assertIsNone(result["ats_pct"])

    def test_nonfinite_baseline_cannot_be_counted_as_an_ats_loss(self):
        self.baseline.loc[0, "expected_margin"] = np.nan
        with self.assertRaisesRegex(ValueError, "(?i)nonfinite"):
            pilot.evaluate(self.baseline, self.cases)

    def test_duplicate_baseline_cannot_double_count_a_game(self):
        duplicate = pd.concat([self.baseline, self.baseline.iloc[[0]]], ignore_index=True)
        with self.assertRaisesRegex(ValueError, "(?i)duplicate"):
            pilot.evaluate(duplicate, self.cases)


if __name__ == "__main__":
    unittest.main()
