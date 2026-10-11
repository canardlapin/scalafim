import copy
import json
import subprocess
import tempfile
from pathlib import Path
import unittest
from unittest.mock import patch
import protocol
import resources

HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[2]

class PairedPilotProtocolTest(unittest.TestCase):
    def setUp(self):
        self.record=json.loads((HERE/'fixtures/historical-record.json').read_text())
        r=self.record
        self.cell=dict(id=r['scenario_id'])
        self.inputs=[dict(sha256=r['case_input_sha256'],root_seed64=r['root_seed64'])]

    def validate(self,rows):
        r=self.record
        return protocol.validate_rows(rows,self.cell,'historical',r['source_commit'],r['source_lock_sha256'],self.inputs,'fixture')

    def test_exact_historical_fixture_is_accepted(self):
        self.assertEqual(self.validate([self.record]),[])

    def test_actual_receipt_or_input_substitution_is_rejected(self):
        for field,value in [('case_input_sha256','0'*64),('source_commit','0'*40),
                            ('receipt_seed64',['1']*4),('root_seed64','1'),
                            ('receipt_algorithm','invented'),('seed_receipts_verified',False),
                            ('completed_compact_fits',795),('raw_exceedances',[0,0,0,0]),
                            ('phase','confirmation')]:
            with self.subTest(field=field):
                r=copy.deepcopy(self.record);r[field]=value
                with self.assertRaises(ValueError):self.validate([r])

    def test_missing_duplicate_and_foreign_slots_fail(self):
        for rows in ([],[self.record,self.record],[dict(self.record,rank_method=protocol.GAUSSIAN)]):
            with self.assertRaises(ValueError):self.validate(rows)

    def test_refusal_is_retained_and_never_becomes_success(self):
        r=dict(self.record,status='failed',reason='synthetic numerical refusal')
        self.assertEqual(self.validate([r]),[(0,'synthetic numerical refusal')])

    def test_assignment_matches_independent_retained_seed(self):
        a=protocol.assignment('fixture','rank-v2-resource-n80-p4-q6-intercept',0)
        self.assertEqual(a['root_seed64'],'5250993378739142209')
        # The retained resource fixtures used domain102; the new pilot explicitly
        # uses noise domain101. Neither its archived data nor its root is changed.
        old_noise=protocol.child_seed(int(a['root_seed64']),[(102,0)])
        self.assertEqual(protocol.r_state(old_noise),[814777009,1734919326,1740522659,1854669298,1494250189,1850162044])
        self.assertNotEqual(a['noise_seed64'],str(old_noise))
        self.assertNotEqual(a['root_seed64'],protocol.assignment('pilot',a['scenario_id'],0)['root_seed64'])
        self.assertEqual(len(set(a['gaussian_hypothesis_seed64'])),4)
        self.assertNotIn(a['permutation_seed64'],a['gaussian_hypothesis_seed64'])
        with self.assertRaises(ValueError):protocol.assignment('confirmation','x',0)

    def test_full_inventory_and_every_stream_are_preserved(self):
        proposal=json.loads((ROOT/'docs/verification/umvpa-rank-integration-20261008/paired-inventory-proposal.json').read_text())
        cells=protocol.inventory(proposal);protocol.validate_inventory(cells,proposal)
        with self.assertRaises(ValueError):protocol.validate_inventory(cells[:-1],proposal)
        changed=copy.deepcopy(cells);changed[0]['datasets']=199
        with self.assertRaises(ValueError):protocol.validate_inventory(changed,proposal)
        roots=[protocol.assignment('pilot',c['id'],i)['root_seed64'] for c in cells for i in range(200)]
        self.assertEqual(len(roots),12800);self.assertEqual(len(set(roots)),12800)

    def test_failed_preflight_cannot_freeze_or_assign_pilot_streams(self):
        with tempfile.TemporaryDirectory() as tmp:
            directory=Path(tmp);probe=directory/'probe.json';probe.write_text('{"preflight_pass":false}')
            output=directory/'manifest.json'
            result=subprocess.run(['python3',str(HERE/'freeze.py'),'--historical',str(ROOT),
                '--output',str(output),'--probe',str(probe),
                '--current-classpath',str(directory/'absent-current'),'--historical-classpath',str(directory/'absent-old')],
                capture_output=True,text=True)
            self.assertNotEqual(result.returncode,0)
            self.assertIn('preflight must pass',result.stderr)
            self.assertFalse(output.exists())
            self.assertFalse((directory/'assignments.jsonl.gz').exists())

    def test_startup_helper_is_recorded_but_not_a_second_numerical_lifetime(self):
        worker=type('Worker',(),{'pid':999,'poll':lambda self:0,'wait':lambda self,timeout:0})()
        flags=type('Flags',(),{'returncode':0,'stdout':'-XX:ActiveProcessorCount=4 -XX:MaxHeapSize=3221225472','stderr':''})()
        resources._commands.update({123:'java sbt-launch.jar --rt-ext-dir',124:'java sbt-launch.jar --detach-stdio'})
        with patch.object(resources.subprocess,'Popen',return_value=worker), \
             patch.object(resources.subprocess,'run',return_value=flags), \
             patch.object(resources,'owned_jvms',side_effect=[{}, {123:100,124:200}, {}, {124:200}]):
            result=resources.monitor(Path('/tmp/a'),['sbt'],{},None,protocol.LIMITS,float('inf'),Path('/tmp/b'))
        self.assertIsNone(result['resource_refusal'])
        self.assertEqual([s['startup_helper'] for s in result['samples']],[True,False])
        self.assertEqual(set(result['process_commands']),{'123','124'})

    def test_shutdown_waits_for_process_exit_after_socket_closes(self):
        result=type('Result',(),{'returncode':0})()
        with patch.object(resources.subprocess,'run',return_value=result), \
             patch.object(resources,'owned_jvms',side_effect=[{321:1000},{321:1000},{},{}]) as ps, \
             patch.object(resources.time,'sleep') as sleep:
            resources.shutdown(Path('/tmp/test-owned-root'),None,{})
        self.assertEqual(sleep.call_count,2)
        self.assertEqual(ps.call_count,4)

if __name__=='__main__':unittest.main()
