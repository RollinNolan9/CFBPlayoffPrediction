"""Synthetic report-update and historical source-identity regressions."""
import importlib.util
from pathlib import Path
import unittest

import pandas as pd


module_path = Path(__file__).resolve().parents[1] / "experiments/injury_case_features.py"
spec = importlib.util.spec_from_file_location("injury_case_features", module_path)
features = importlib.util.module_from_spec(spec)
spec.loader.exec_module(features)


class ReportSelectionTests(unittest.TestCase):
    def select(self, descriptions):
        reports, statuses = [], []
        for report_id, published, complete, players, updated in descriptions:
            reports.append({"report_id": report_id, "game_id": "target", "team": "A",
                "published": pd.Timestamp(published), "cutoff": pd.Timestamp("2025-10-17T19:00:00Z"),
                "team_report_complete": complete, "rb_report_complete": complete,
                "content_update_after_cutoff": updated})
            for player, status in players.items():
                statuses.append({"report_id": report_id, "game_id": "target", "team": "A",
                    "person_key": features.person_key(player), "status": status})
        return features.select_reports(pd.DataFrame(reports), pd.DataFrame(statuses,
            columns=["report_id", "game_id", "team", "person_key", "status"]))

    def test_newer_partial_clearance_blocks_old_out(self):
        selected, ledger = self.select([
            ("old", "2025-10-16T01:00:00Z", True, {"Runner": "Out"}, False),
            ("new", "2025-10-17T18:00:00Z", False, {"Runner": "Available"}, False)])
        self.assertTrue(selected.empty)
        self.assertEqual(ledger[0]["reason"], "latest_report_incomplete")

    def test_simultaneous_conflict_does_not_use_report_id_order(self):
        for out_id, available_id in [("z_out", "a_available"), ("a_out", "z_available")]:
            with self.subTest(out_id=out_id):
                selected, ledger = self.select([
                    (out_id, "2025-10-16T01:00:00Z", True, {"Runner": "Out"}, False),
                    (available_id, "2025-10-16T01:00:00Z", True, {"Runner": "Available"}, False)])
                self.assertTrue(selected.empty)
                self.assertEqual(ledger[0]["reason"], "conflicting_latest_reports_same_timestamp")

    def test_same_time_omission_in_complete_report_is_conflict(self):
        selected, _ = self.select([
            ("listed", "2025-10-16T01:00:00Z", True, {"Runner": "Out"}, False),
            ("not_listed", "2025-10-16T01:00:00Z", True, {}, False)])
        self.assertTrue(selected.empty)

    def test_agreeing_simultaneous_copies_are_usable(self):
        selected, ledger = self.select([
            ("one", "2025-10-16T01:00:00Z", True, {"Runner": "Out"}, False),
            ("two", "2025-10-16T01:00:00Z", True, {"Runner": "out"}, False)])
        self.assertEqual(len(selected), 1)
        self.assertTrue(ledger[0]["eligible"])

    def test_later_complete_clearance_supersedes_out(self):
        selected, _ = self.select([
            ("old", "2025-10-16T01:00:00Z", True, {"Runner": "Out"}, False),
            ("cleared", "2025-10-17T18:00:00Z", True, {}, False)])
        self.assertEqual(selected.report_id.tolist(), ["cleared"])

    def test_post_cutoff_update_does_not_change_friday_evidence(self):
        selected, _ = self.select([
            ("before", "2025-10-16T01:00:00Z", True, {"Runner": "Out"}, False),
            ("after", "2025-10-17T20:00:00Z", True, {"Runner": "Available"}, False)])
        self.assertEqual(selected.report_id.tolist(), ["before"])

    def test_mutated_latest_copy_cannot_fall_back_to_old_out(self):
        selected, ledger = self.select([
            ("old", "2025-10-16T01:00:00Z", True, {"Runner": "Out"}, False),
            ("updated_after_cutoff", "2025-10-17T18:00:00Z", True, {"Runner": "Out"}, True)])
        self.assertTrue(selected.empty)
        self.assertEqual(ledger[0]["reason"], "latest_report_content_updated_after_cutoff")


class HistoryIdentityTests(unittest.TestCase):
    def setUp(self):
        self.report = pd.Series({"team": "Mississippi", "season": 2024})
        self.schedule = pd.Series({"home": "Mississippi", "away": "Georgia", "season": 2024})
        self.specification = {"team": "Mississippi", "season": 2024}

    def test_official_team_alias_is_accepted(self):
        features.validate_history_identity({"team": "Ole Miss", "season": 2024},
            self.report, self.schedule, self.specification)

    def test_wrong_team_history_is_rejected(self):
        with self.assertRaisesRegex(ValueError, "manifest team"):
            features.validate_history_identity({"team": "Texas A&M", "season": 2024},
                self.report, self.schedule, self.specification)

    def test_future_season_history_is_rejected(self):
        with self.assertRaisesRegex(ValueError, "season differs"):
            features.validate_history_identity({"team": "Ole Miss", "season": 2025},
                self.report, self.schedule, self.specification)

    def test_wrong_specification_season_is_rejected(self):
        self.specification["season"] = 2025
        with self.assertRaisesRegex(ValueError, "season differs"):
            features.validate_history_identity({"team": "Ole Miss", "season": 2024},
                self.report, self.schedule, self.specification)

    def test_team_outside_scheduled_game_is_rejected(self):
        self.schedule["home"] = "Alabama"
        with self.assertRaisesRegex(ValueError, "scheduled game"):
            features.validate_history_identity({"team": "Ole Miss", "season": 2024},
                self.report, self.schedule, self.specification)


if __name__ == "__main__":
    unittest.main()
