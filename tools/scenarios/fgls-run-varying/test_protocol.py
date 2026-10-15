import copy
import json
from pathlib import Path
import unittest
import tempfile

from freeze_candidate import choose, validate_records

DIRECTORY = Path(__file__).resolve().parent


class RunShrinkageProtocolTests(unittest.TestCase):
    def setUp(self):
        self.protocol = json.loads((DIRECTORY / 'protocol.json').read_text())
        self.summary = dict(profile='development', failures=0, results=[
            dict(engine=engine, cell=cell['id'], n=512,
                 criteria=dict(complete=True, no_refusals=True, ar_rmse=True, coverage=False))
            for engine in self.protocol['candidate_engines'] for cell in self.protocol['cells']])

    def test_original_margins_and_every_scope_cell_remain_binding(self):
        old = json.loads((DIRECTORY.parent / 'fgls-voxelwise/protocol.json').read_text())
        self.assertEqual(self.protocol['acceptance'], old['acceptance'])
        self.assertEqual([c['id'] for c in self.protocol['cells']], [c['id'] for c in old['cells']])
        self.assertTrue(all(c['role'] == 'admission' for c in self.protocol['cells']))

    def test_selection_is_development_only_and_has_a_declared_tie_break(self):
        self.assertEqual(choose(self.protocol, self.summary)[0], 'run-shrink-075-kr')
        self.summary['results'][-1]['criteria']['coverage'] = True
        self.assertEqual(choose(self.protocol, self.summary)[0], 'run-shrink-050-kr')
        self.summary['profile'] = 'confirmation'
        with self.assertRaises(ValueError):
            choose(self.protocol, self.summary)

    def test_missing_cells_refusals_and_incomplete_counts_cannot_select_a_candidate(self):
        for mutation in ('missing', 'refusal', 'count', 'duplicate'):
            summary = copy.deepcopy(self.summary)
            if mutation == 'missing':
                summary['results'] = summary['results'][:-1] + summary['results'][1:4]
            elif mutation == 'refusal':
                summary['failures'] = 1
            elif mutation == 'count':
                for row in summary['results']:
                    row['n'] = 511
            else:
                for row in summary['results']:
                    row['cell'] = self.protocol['cells'][0]['id']
            with self.subTest(mutation=mutation), self.assertRaises(ValueError):
                choose(self.protocol, summary)

    def test_trial_slots_cannot_be_replaced_by_duplicates_or_wrong_profiles(self):
        protocol = dict(cells=[dict(id='cell')], profiles=dict(pilot=dict(replicates=2)))
        rows = [dict(cell='cell', profile='pilot', engine='engine', replicate=i) for i in range(2)]
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / 'records.jsonl'
            def write(values):
                path.write_text(''.join('FGLS_STUDY_TRIAL ' + json.dumps(row) + '\n' for row in values))
            write(rows)
            self.assertEqual(validate_records(path, protocol, 'pilot', ['engine']), 2)
            for values in (rows[:1], [rows[0], rows[0]], [rows[0], dict(rows[1], profile='confirmation')]):
                write(values)
                with self.assertRaises(ValueError):
                    validate_records(path, protocol, 'pilot', ['engine'])


if __name__ == '__main__':
    unittest.main()
