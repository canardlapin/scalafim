"""Index command literals in the history audit; declarations are not execution."""
import ast
import hashlib
import json
import re
from pathlib import Path

ROOT = Path('/private/tmp/scalafim-phrf21-next-20260930')
audit = json.loads((ROOT / 'stream-history-audit.json').read_text())
commands = []
for session in audit['repositorySessionInventory']:
    wanted = {h['line']: h for h in session['candidateToolRecords'] if h['type'] in ('custom_tool_call', 'function_call')}
    if not wanted:
        continue
    for n, line in enumerate(Path(session['path']).open(), 1):
        if n not in wanted:
            continue
        # Live sessions may append; the audited historical record must match.
        assert hashlib.sha256(line.encode()).hexdigest() == wanted[n]['lineSha256']
        p = json.loads(line)['payload']
        s = p.get('input', p.get('arguments', ''))
        if not isinstance(s, str):
            s = json.dumps(s)
        for m in re.finditer(r'\b["\']?cmd["\']?\s*:\s*("(?:[^"\\]|\\.)*"|\'(?:[^\'\\]|\\.)*\')', s):
            try:
                c = json.loads(m[1]) if m[1].startswith('"') else ast.literal_eval(m[1])
            except (ValueError, SyntaxError):
                continue
            if ('fresh' in c or re.search(r'700093(?:0201|0102|0103|0101)', c)) and re.search(r'C0|c0-|700093|runProspectiveStudy', c):
                hits = list(re.finditer(r'fresh|700093(?:0201|0102|0103|0101)', c))
                excerpts = [c[max(0, hit.start() - 65):hit.end() + 75].replace('\n', ' ') for hit in hits[:3]]
                commands.append({'session': session['path'], 'line': n,
                                 'commandSha256': hashlib.sha256(c.encode()).hexdigest(),
                                 'commandPrefix': c[:80].replace('\n', ' '), 'excerpts': excerpts})
(ROOT / 'session-command-index.json').write_text(json.dumps(commands, indent=2) + '\n')
print(json.dumps({'indexedCommandLiterals': len(commands), 'scope': 'Candidate literals only; reviewed declarations/read-only/tracker actions are not fresh execution receipts.'}))
