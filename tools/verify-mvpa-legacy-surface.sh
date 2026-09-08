#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/.." && pwd)"
cd "$repo_root"

if [[ "${1:---final}" != "--final" ]]; then
  printf 'usage: %s [--final]\n' "$0" >&2
  exit 2
fi

failures=0

fail() {
  printf 'ERROR: %s\n' "$1" >&2
  failures=$((failures + 1))
}

for module in modules/mvpa-fit modules/mvpa-dataset modules/mvpa-spatial; do
  if [[ -e "$module" ]]; then
    fail "retired module still exists: $module"
  fi
done

source_retired='\b(RoiPayload|Response|FeatureSetKind|FeatureSetPlan|FeatureSet|FoldPlan|FoldPartition|RoiAnalysis|FoldRequiredRoiAnalysis|DenseRoiAnalysis|OperatorRoiAnalysis|FoldRequiredDenseRoiAnalysis|FoldRequiredOperatorRoiAnalysis|RoiContext|UnfoldedRoiContext|FoldedRoiContext|RoiAnalysisResult|RoiOutcome|MvpaResult|MvpaEngine|MvpaTask|MvpaStream|PatternMatrix|PatternOperator|PatternSource|DensePatternSource|OperatorPatternSource|InMemoryPatternSource|SampleAxis|SampleIndex|RoiId|CanonicalEffectMvpaResult|NonnegativeCanonicalMvpaResult|ManovaMvpaResult)\b|scalafim\.fmri\.mvpa\.(fit|dataset|spatial)(\.|\b)'
consumer_retired='\b(RoiPayload|FeatureSetKind|FeatureSetPlan|FoldPlan|FoldPartition|RoiAnalysis|FoldRequiredRoiAnalysis|DenseRoiAnalysis|OperatorRoiAnalysis|FoldRequiredDenseRoiAnalysis|FoldRequiredOperatorRoiAnalysis|RoiContext|UnfoldedRoiContext|FoldedRoiContext|RoiAnalysisResult|RoiOutcome|MvpaResult|MvpaEngine|MvpaTask|MvpaStream|PatternMatrix|PatternOperator|PatternSource|DensePatternSource|OperatorPatternSource|InMemoryPatternSource|CanonicalEffectMvpaResult|NonnegativeCanonicalMvpaResult|ManovaMvpaResult)\b|scalafim\.fmri\.mvpa\.(Response|SampleAxis|SampleIndex|FeatureSet|RoiId)(\.|\b)|\bResponse\.(Categorical|Probabilistic|Continuous|categorical|continuous|probabilistic)\b|scalafim\.fmri\.mvpa\.(fit|dataset|spatial)(\.|\b)|scalafim-fmri-mvpa-(fit|dataset|spatial)\b'
architectural_retired='\b(CategoricalClassifier|FittedCategoricalClassifier|ClassScoreSemantics|NeuralMetricSelection|RandomizationDesign|PcaQuery|ModelDisposition|directQuery|crossRun)\b'

source_hits="$({
  rg -n \
    --glob '!**/target/**' \
    "$source_retired" \
    modules/mvpa || true
} | sed -n '1,121p')"

consumer_hits="$({
  rg -n \
    --glob '!**/target/**' \
    --glob '!docs/migrations/mvpa-zero-cruft.md' \
    --glob '!tools/verify-mvpa-legacy-surface.sh' \
    "$consumer_retired" \
    README.md build.sbt docs examples tools || true
} | sed -n '1,121p')"

if [[ -n "$source_hits$consumer_hits" ]]; then
  fail 'retired MVPA vocabulary remains outside the historical migration record'
  printf '%s\n%s\n' "$source_hits" "$consumer_hits" >&2
fi

architecture_hits="$({
  rg -n \
    --glob '!**/target/**' \
    --glob '!docs/migrations/mvpa-zero-cruft.md' \
    --glob '!tools/verify-mvpa-legacy-surface.sh' \
    "$architectural_retired" \
    modules/mvpa README.md build.sbt docs examples tools || true
} | sed -n '1,121p')"

if [[ -n "$architecture_hits" ]]; then
  fail 'superseded MVPA compiler, policy, or generator names remain'
  printf '%s\n' "$architecture_hits" >&2
fi

compatibility_hits="$({
  rg -n --glob '*.scala' \
    '^package[[:space:]]+scalafim\.fmri\.mvpa\.(next|prototype|compat|compatibility|legacy|adapter)(\.|$)' \
    modules/mvpa/shared/src modules/mvpa/jvm/src modules/mvpa/js/src 2>/dev/null || true
})"

if [[ -n "$compatibility_hits" ]]; then
  fail 'prototype, compatibility, legacy, and adapter namespaces are forbidden'
  printf '%s\n' "$compatibility_hits" >&2
fi

alder_hits="$({
  rg -l --glob '*.scala' '\balder\.' \
    modules/mvpa/shared/src/main modules/mvpa/jvm/src/main modules/mvpa/js/src/main 2>/dev/null \
    | rg -v '/scalafim/fmri/mvpa/predictive/' || true
})"

if [[ -n "$alder_hits" ]]; then
  fail 'Alder types may occur only inside the MVPA predictive boundary'
  printf '%s\n' "$alder_hits" >&2
fi

if [[ "$failures" -ne 0 ]]; then
  exit 1
fi

printf 'MVPA zero-cruft final court passed.\n'
