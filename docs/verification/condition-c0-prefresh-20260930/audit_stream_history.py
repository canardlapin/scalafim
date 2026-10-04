"""Inspect recorded C0 execution history without generating any response."""
import datetime
import hashlib
import json
import re
from pathlib import Path

OUT = Path('/private/tmp/scalafim-phrf21-next-20260930')
ROOT = Path('/private/tmp/scalafim-execution-20260929')
FRESH = (7000930201, 7000930102, 7000930103)
RETIRED = 7000930101
SEED = re.compile(r'700093(?:0201|0102|0103|0101)')
C0 = re.compile(r'ConditionC0|condition.c0|c0-(?:qualification|standalone|final|repair)|ConditionBudget', re.I)

def digest(p):
    h = hashlib.sha256()
    with p.open('rb') as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b''):
            h.update(chunk)
    return h.hexdigest()

def compact(command):
    if not isinstance(command, list):
        return str(command)[:300]
    result = []
    hide = False
    for value in command:
        if hide:
            result.append('<classpath sha256=' + hashlib.sha256(str(value).encode()).hexdigest() + '>')
            hide = False
        else:
            result.append(value)
            hide = value in ('-cp', '-classpath')
    return result

inventory, records, mentions, commands, sessions, unavailable = [], [], [], [], [], []
logs = sorted(set((ROOT / 'logs').glob('*.log')) | set(Path('/private/tmp/scalafim-phrf-push-20260930/evidence').glob('*.log')))
for p in logs:
    stat = p.stat()
    item = {'path': str(p), 'bytes': stat.st_size, 'sha256': digest(p)}
    inventory.append(item)
    with p.open(errors='replace') as f:
        for n, line in enumerate(f, 1):
            seed_hits = sorted(set(SEED.findall(line)))
            if seed_hits:
                mentions.append({'path': str(p), 'line': n, 'seeds': seed_hits, 'lineSha256': hashlib.sha256(line.encode()).hexdigest()})
            if '"kind"' not in line or '"cohort"' not in line:
                continue
            try:
                x = json.loads(line[line.index('{'):])
            except (ValueError, json.JSONDecodeError):
                continue
            if isinstance(x, dict) and x.get('cohort') == 'fresh':
                records.append({'path': str(p), 'line': n, 'kind': x.get('kind'), 'seed': x.get('seed'), 'sourceId': x.get('sourceId')})
    item['stableSizeAndMtimeDuringRead'] = (p.stat().st_size, p.stat().st_mtime_ns) == (stat.st_size, stat.st_mtime_ns)
    meta = Path(str(p) + '.meta.json')
    if meta.exists():
        try:
            x = json.loads(meta.read_text())
        except (OSError, json.JSONDecodeError) as error:
            unavailable.append({'path': str(meta), 'reason': str(error)})
            continue
        command = x.get('command', x.get('commands', []))
        if p.name.startswith('c0-') or p.parent == Path('/private/tmp/scalafim-phrf-push-20260930/evidence') or C0.search(json.dumps(command)):
            commands.append({'path': str(meta), 'sha256': digest(meta), 'command': compact(command),
                             'exit_code': x.get('exit_code'), 'started_utc': x.get('started_utc')})

# Only repository-specific sessions, starting on the two days on which these
# streams were declared. No unrelated session bodies are inspected or copied.
for day in ('29', '30'):
    for p in sorted(Path('/Users/bbuchsbaum/.codex/sessions/2026/09', day).glob('*.jsonl')):
        selected = False
        with p.open(errors='replace') as f:
            for n, line in enumerate(f, 1):
                if n > 100:
                    break
                try:
                    context = json.loads(line)
                except json.JSONDecodeError:
                    continue
                cwd = context.get('payload', {}).get('cwd', '')
                if context.get('type') in ('session_meta', 'turn_context') and (
                    cwd.startswith('/Users/bbuchsbaum/code/scala/scalafim') or
                    cwd.startswith('/private/tmp/scalafim-execution-20260929') or
                    cwd.startswith('/private/tmp/scalafim-phrf')):
                    selected = True
                    break
        if not selected:
            continue
        hits = []
        with p.open(errors='replace') as f:
            for n, line in enumerate(f, 1):
                if not SEED.search(line) and not ('fresh' in line and C0.search(line)):
                    continue
                try:
                    x = json.loads(line)
                except json.JSONDecodeError:
                    continue
                payload = x.get('payload', {})
                if x.get('type') != 'response_item' or payload.get('type') not in ('function_call', 'function_call_output', 'custom_tool_call', 'custom_tool_call_output'):
                    continue
                # Mentions in tool code/output are candidates for inspection,
                # never silently classified as completed numerical execution.
                hits.append({'line': n, 'timestamp': x.get('timestamp'), 'type': payload.get('type'),
                             'tool': payload.get('name'), 'seeds': sorted(set(SEED.findall(line))),
                             'lineSha256': hashlib.sha256(line.encode()).hexdigest()})
        sessions.append({'path': str(p), 'bytes': p.stat().st_size, 'sha256': digest(p), 'candidateToolRecords': hits})

result = {'schema': 'c0-prior-use-record-audit-v1', 'createdUtc': datetime.datetime.now(datetime.timezone.utc).isoformat(),
          'freshRoots': FRESH, 'retiredEntireStream': RETIRED,
          'scope': 'All current execution-root raw logs and Gaussian-repair logs; repo-specific Codex sessions on 2026-09-29/30. Text/tool-record audit, not global historical proof.',
          'rawLogInventory': inventory, 'rawSeedMentions': mentions, 'freshOutputRecords': records,
          'c0MetadataCommands': commands, 'repositorySessionInventory': sessions, 'unavailableMetadata': unavailable,
          'disposition': 'No response generation by this audit. Fresh entry still needs parent historical-use disposition and exact integrated source/provider freeze review.'}
OUT.mkdir(parents=True, exist_ok=True)
(OUT / 'stream-history-audit.json').write_text(json.dumps(result, indent=2) + '\n')
print(json.dumps({'rawLogs': len(inventory), 'rawSeedMentions': len(mentions), 'freshOutputRecords': len(records),
                  'c0Commands': len(commands), 'repoSessions': len(sessions),
                  'candidateToolRecords': sum(len(x['candidateToolRecords']) for x in sessions)}, indent=2))
