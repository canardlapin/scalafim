% Host-local steady-state benchmark of the original RSA Toolbox numerical calls.
% Setup and fixture generation are outside every timed loop.  This court runs
% under Octave and is diagnostic; it is not a claim about MATLAB performance.

toolboxRoot = getenv('RSA_TOOLBOX_MATLAB_ROOT');
if isempty(toolboxRoot)
    error('RSA_TOOLBOX_MATLAB_ROOT is required');
end
% Octave emits one MATLAB-compatibility warning from distanceLDC on every
% invocation.  Repeated console IO would dominate the timed call; correctness
% is checked separately by the unsuppressed reference court.
warning('off', 'all');
addpath(toolboxRoot);
addpath(fullfile(toolboxRoot, '+rsa', '+util'));

nPartitions = 8;
nEffects = 8;
nFeatures = 64;
patterns = zeros(nPartitions, nEffects, nFeatures);
for partitionIndex = 1:nPartitions
    for effectIndex = 1:nEffects
        for featureIndex = 1:nFeatures
            patterns(partitionIndex, effectIndex, featureIndex) = ...
                sin(effectIndex * 0.31 + featureIndex * 0.017) + ...
                0.02 * (partitionIndex - 1) * cos(featureIndex * 0.11);
        end
    end
end

betas = zeros(nPartitions * nEffects, nFeatures);
partition = zeros(nPartitions * nEffects, 1);
condition = zeros(nPartitions * nEffects, 1);
row = 1;
for partitionIndex = 1:nPartitions
    for effectIndex = 1:nEffects
        betas(row, :) = reshape(patterns(partitionIndex, effectIndex, :), 1, nFeatures);
        partition(row) = partitionIndex;
        condition(row) = effectIndex;
        row = row + 1;
    end
end

model = zeros(nEffects * (nEffects - 1) / 2, 1);
position = 1;
for left = 1:(nEffects - 1)
    for right = (left + 1):nEffects
        model(position) = mod(left - 1, 2) ~= mod(right - 1, 2);
        position = position + 1;
    end
end

for warmup = 1:3
    distance = rsa.distanceLDC(betas, partition, condition);
end

repeats = 9;

crossCalls = 3;
crossDurations = zeros(repeats, 1);
for repetition = 1:repeats
    started = tic();
    for callIndex = 1:crossCalls
        distance = rsa.distanceLDC(betas, partition, condition);
    end
    crossDurations(repetition) = toc(started) * 1e9 / crossCalls;
end

pearsonCalls = 100;
pearsonDurations = zeros(repeats, 1);
for repetition = 1:repeats
    started = tic();
    for callIndex = 1:pearsonCalls
        correlation = corrcoef(distance(:), model);
    end
    pearsonDurations(repetition) = toc(started) * 1e9 / pearsonCalls;
end

olsCalls = 50;
olsDurations = zeros(repeats, 1);
for repetition = 1:repeats
    started = tic();
    for callIndex = 1:olsCalls
        [slope, rSquared] = rsa.stat.fitModelOLS(model, distance, 'intercept', 1);
        intercept = mean(distance) - mean(model) * slope;
    end
    olsDurations(repetition) = toc(started) * 1e9 / olsCalls;
end

sortedCross = sort(crossDurations);
sortedPearson = sort(pearsonDurations);
sortedOls = sort(olsDurations);

result.schema = 'scalafim-mvpa-matlab-performance/v1';
result.host.runtime = version();
result.host.computer = computer();
result.reference.name = 'rsatoolbox-matlab';
result.reference.revision = '91ad43359c8a6355de1dd726e69038b7e798720a';
result.reference.warnings_suppressed_for_timing = true;
result.shapes.relational.partitions = nPartitions;
result.shapes.relational.effects = nEffects;
result.shapes.relational.features = nFeatures;
result.allocation_scope = 'unavailable in the Octave court';

result.results(1).scenario = 'crossvalidated-identity-rdm';
result.results(1).stage = 'steady-state-public-call';
result.results(1).calls_per_repeat = crossCalls;
result.results(1).repeats = repeats;
result.results(1).warmup_calls = 3;
result.results(1).median_ns_per_call = median(crossDurations);
result.results(1).p25_ns_per_call = sortedCross(3);
result.results(1).p75_ns_per_call = sortedCross(7);
result.results(1).min_ns_per_call = min(crossDurations);
result.results(1).max_ns_per_call = max(crossDurations);
result.results(1).samples_ns_per_call = crossDurations;
result.results(1).checksum = sum(distance(:));

result.results(2).scenario = 'rsa-pearson-query';
result.results(2).stage = 'steady-state-public-call';
result.results(2).calls_per_repeat = pearsonCalls;
result.results(2).repeats = repeats;
result.results(2).warmup_calls = 0;
result.results(2).median_ns_per_call = median(pearsonDurations);
result.results(2).p25_ns_per_call = sortedPearson(3);
result.results(2).p75_ns_per_call = sortedPearson(7);
result.results(2).min_ns_per_call = min(pearsonDurations);
result.results(2).max_ns_per_call = max(pearsonDurations);
result.results(2).samples_ns_per_call = pearsonDurations;
result.results(2).checksum = correlation(1, 2);

result.results(3).scenario = 'rsa-intercepted-ols-query';
result.results(3).stage = 'steady-state-public-call';
result.results(3).calls_per_repeat = olsCalls;
result.results(3).repeats = repeats;
result.results(3).warmup_calls = 0;
result.results(3).median_ns_per_call = median(olsDurations);
result.results(3).p25_ns_per_call = sortedOls(3);
result.results(3).p75_ns_per_call = sortedOls(7);
result.results(3).min_ns_per_call = min(olsDurations);
result.results(3).max_ns_per_call = max(olsDurations);
result.results(3).samples_ns_per_call = olsDurations;
result.results(3).checksum = intercept + slope;

fprintf('SCALAFIM_JSON_BEGIN\n');
fprintf('%s\n', jsonencode(result));
fprintf('SCALAFIM_JSON_END\n');
