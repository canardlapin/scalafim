% Executable original RSA Toolbox arm of the ScalaFIM MVPA court.
%
% The wrapper supplies a no-op import function under Octave because Octave
% does not implement MATLAB package imports.  The numerical functions below
% are unmodified files from the pinned rsatoolbox_matlab checkout.

toolboxRoot = getenv('RSA_TOOLBOX_MATLAB_ROOT');
fixturePath = getenv('SCALAFIM_MVPA_FIXTURE');
if isempty(toolboxRoot)
    error('RSA_TOOLBOX_MATLAB_ROOT is required');
end
if isempty(fixturePath)
    error('SCALAFIM_MVPA_FIXTURE is required');
end

addpath(toolboxRoot);
% distanceLDC and fitModelOLS refer to indicatorMatrix without a package
% qualifier after importing rsa.util in MATLAB.
addpath(fullfile(toolboxRoot, '+rsa', '+util'));

fixture = jsondecode(fileread(fixturePath));
% BEGIN SCENARIO rsatoolbox-matlab-relational-crossnobis
patterns = fixture.relational.patterns_by_run;
nRuns = size(patterns, 1);
nConditions = size(patterns, 2);
nFeatures = size(patterns, 3);

betas = zeros(nRuns * nConditions, nFeatures);
partition = zeros(nRuns * nConditions, 1);
condition = zeros(nRuns * nConditions, 1);
row = 1;
for run = 1:nRuns
    for item = 1:nConditions
        betas(row, :) = reshape(patterns(run, item, :), 1, nFeatures);
        partition(row) = run;
        condition(row) = item;
        row = row + 1;
    end
end

% distanceLDC expects noise-normalized patterns.  If K = L L', B L has
% exactly the inner product B K B' used by ScalaFIM and Python rsatoolbox.
precisionFactor = chol(fixture.relational.precision, 'lower');
whitenedBetas = betas * precisionFactor;
distance = rsa.distanceLDC(whitenedBetas, partition, condition);
% END SCENARIO rsatoolbox-matlab-relational-crossnobis

% BEGIN SCENARIO rsatoolbox-matlab-relational-rsa
models = [fixture.relational.models.two_category, fixture.relational.models.graded];
[slopes, rSquared] = rsa.stat.fitModelOLS(models, distance, 'intercept', 1);
intercept = mean(distance) - mean(models, 1) * slopes';

pearsonCategory = corrcoef(distance', models(:, 1));
pearsonGraded = corrcoef(distance', models(:, 2));
rankedDistance = rsa.util.rankTransform_equalsStayEqual(distance', 0);
rankedCategory = rsa.util.rankTransform_equalsStayEqual(models(:, 1), 0);
rankedGraded = rsa.util.rankTransform_equalsStayEqual(models(:, 2), 0);
spearmanCategory = corrcoef(rankedDistance, rankedCategory);
spearmanGraded = corrcoef(rankedDistance, rankedGraded);
% END SCENARIO rsatoolbox-matlab-relational-rsa

result.schema = 'scalafim-mvpa-rsatoolbox-matlab-result/v1';
result.fixture_sha256 = fixture.fixture_sha256;
result.reference.name = 'rsatoolbox-matlab';
result.reference.revision = '91ad43359c8a6355de1dd726e69038b7e798720a';
result.reference.runtime = version();
result.scenarios.relational.pair_order = fixture.relational.pair_order;
result.scenarios.relational.crossnobis_fixed_precision = distance;
result.scenarios.relational.rsa.pearson_two_category = pearsonCategory(1, 2);
result.scenarios.relational.rsa.spearman_two_category = spearmanCategory(1, 2);
result.scenarios.relational.rsa.pearson_graded = pearsonGraded(1, 2);
result.scenarios.relational.rsa.spearman_graded = spearmanGraded(1, 2);
result.scenarios.relational.rsa.ols_terms = {'intercept', 'two_category', 'graded'};
result.scenarios.relational.rsa.ols_coefficients = [intercept, slopes];
result.scenarios.relational.rsa.r_squared = rSquared;

fprintf('SCALAFIM_JSON_BEGIN\n');
fprintf('%s\n', jsonencode(result));
fprintf('SCALAFIM_JSON_END\n');
