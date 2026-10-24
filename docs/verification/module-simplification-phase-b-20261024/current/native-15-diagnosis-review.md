# Independent Native15 diagnosis review

Reviewer: acceptance_scenario_review (Astra), source2fd0b2b1117c979a1320b8f5158f34fa957bb485. One controlled, unchanged full201case replay is justified after a user quiet-desktop signal. Native15 remains failed; this does not prove external interference.

The trace records Glass-delivered key release -> CheckBox.fire -> enabled listener396 -> CohortTablePane.edit -> LatestWork.clear -> invalidate fails an outstanding promise with CancellationException. The probe after callback calls result.get, which correctly fails this workflow. clear does not itself throw: the exception stack records creation. This supports an edit superseding unfinished work, rather than spontaneous worker computation failure.

The density probe has no keyboard injection. Its own enabled-checkbox toggles occur later after acceptance; preceding drawer probe dispatches only local synthetic Tab/Escape events in a separate fork. The trace identifies neither key nor origin (human, automation, queued native input or other native-event issue) and cannot distinguish initial loading from early review.

Keep code, input handling, cancellation assertions and budgets unchanged. Revalidate resume eligibility immediately before launch; rerun every case afresh. A pass qualifies the new run and does not establish the earlier cause. If the same failure recurs during the controlled window, stop equivalent retries and capture narrowly scoped key target/code/timing and operation phase. Do not swallow cancellation or disable interaction for a pass.
