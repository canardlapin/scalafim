# sbt-warm command completion

The wrapper now appends a unique completion receipt to the same sbt command
sequence as each requested gate. Success requires both a zero client exit and
the exact receipt. Receipts live outside build targets, survive `clean`, and are
removed after each invocation. Shutdown retains its separate socket-death check.

Validation on 2026-10-11:

- The zero-exit disconnect regression failed before the change and passes after it.
- `python3 -m unittest discover -s tools/build -p test_sbt_warm.py`: 63 tests pass.
- A disposable sbt 1.11.7 project exercised successful commands, `clean`, a task
  throwing an exception, and a task abruptly halting the server. Failure and
  disconnect both stopped later requested commands; restart and shutdown passed.
- `python3 tools/build/sbt-warm arJVM/test arJS/test`: both platforms pass,
  170 tests each, with a separate completion receipt for each gate.

The live server-death probe returned a nonzero client exit; the regression test
separately covers a thin client returning zero after a disconnect and misleading
earlier success output. Neither output matching nor a second client connection
is accepted as completion evidence.
