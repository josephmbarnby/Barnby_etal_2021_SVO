%%
% Barnby & Dayan (2021) Inequality Aversion and Paranoia
%
% Joe Barnby j.barnby@uq.edu.au 2021
%
%% MASTER FILE %%
%% Load data

addpath('CBMCode');
Db      = readtable('Intentions_BothPhase.csv');
Dbsim   = readtable('simulatedDat.csv'); %simulated data for recovery
D3      = table2array(Db(:,contains(Db.Properties.VariableNames, {'id', 'Trial', 'Option1_PPT', 'Option1_Partner', 'Option2_PPT', 'Option2_Partner', 'Response', 'Answer', 'GuessAction', 'Correct'})));
D3sim   = table2array(Dbsim(:,contains(Dbsim.Properties.VariableNames, {'PPT', 'Trial', 'Op1SimPPT', 'Op1SimPAR', 'Op2SimPPT', 'Op2SimPAR', 'simA', 'Answer', 'Alpha', 'Beta'})));

subs = unique(D3(:,1));
subsSim = unique(D3sim(:,9));
% 697 participants, each associated with a 18 x 7 data matrix
n = 697;
nSim = 1000;
 
%--------------------------------

d = size(D3,1)/n;
dsim = size(D3sim,1)/nSim;

sb = cell(697, 1);
sb_phase2 = cell(697, 1);
sbSim = cell(697, 1);
sbDF = zeros(1000,3);

for i = 1:n
    sb{i} = D3(1+(i-1)*d:d+(i-1)*d,:);
    sb_phase2{i} = sb{i}(19:54, :);
    sb_phase2{i}(1:36, 2) = 1:36;
end
for i = 1:nSim
    sbSim{i} = D3sim(1+(i-1)*dsim:dsim+(i-1)*dsim,:);
    sbDF(i, 1:3) = sbSim{i}(1, 7:9);
end

%% Test Model

% Test with person against competitive partner
% Test bayes model
FSModel_1_Phase2([2, -2, 2, 2], sb{1}) % with info from phase 1 or zeta
FSModel_1_Phase2([2, -2, 2, 2, 1], sb{1}) % with info from phase 1
FSModel_1_Phase2_7parms([2, -2, 2, 2, 1, 2, -2], sb{1}) % with no info from phase 1
% Test heuristic model
Q_model_FS([2, -2, 0.5, 0.1, 0.1, 0.9], sb{1}) % with 6 parameters
Q_model_FS([2, -2, 0.5, 0.1, 0.1], sb{1}) % with 5 parameters
Q_model_FS([2, -2, 0.5, 0.1], sb{1}) % with 4 parameters

F = zeros(697,1);

for i=1:length(sb)
[F(i)] = FSModel_1_Phase2([2, -2, 1, 1, 1], sb{i});
end

%RW test
dataTest = [sb_phase2{1}];
dataTest(:,9) = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,1,1,1,1,1,1];
dataTest(:,10) = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1];
[FTest, QTest, simATest] = Q_model_FS([-1, 2, 4, -4], dataTest, 1, 1);

%% Create priors for CBM

v = 7.5;
prior_BFS6_null = struct('mean', zeros(6,1), 'variance', v);
prior_ChoiceCong = struct('mean', zeros(6,1), 'variance', v);
prior_ActionCong = struct('mean', zeros(6,1), 'variance', v);
prior_BFShrink = struct('mean', zeros(5,1), 'variance', v);
prior_BFSzeta   = struct('mean', zeros(5,1), 'variance', v);
prior_BFS4   = struct('mean', zeros(4,1), 'variance', v);
prior_FavBias   = struct('mean', zeros(5,1), 'variance', v);
prior_BFS2   = struct('mean', zeros(2,1), 'variance', v);
prior_RWFS4 = struct('mean', zeros(4,1), 'variance', v);
prior_RWFS6_3lrc = struct('mean', zeros(6,1), 'variance', v);
prior_RWFS5_updown = struct('mean', zeros(5,1), 'variance', v);
prior_RWFS5_congIncong = struct('mean', zeros(5,1), 'variance', v);

%% parfor loops to run CBM

% first create directories for individual output files:
mkdir('lap_subjects_BFS2_20');
mkdir('lap_subjects_BFS4_20');
mkdir('lap_subjects_BFShrink_20');
mkdir('lap_subjects_BFSzeta_20');
mkdir('lap_subjects_BFS6_20');
mkdir('lap_subjects_BFS6_ChoiceCong');
mkdir('lap_subjects_BFS6_ActionCong');
mkdir('lap_subjects_BFS5_FavBias');

mkdir('lap_subjects_RWFS4_20');
mkdir('lap_subjects_RWFS5_updown_20');
mkdir('lap_subjects_RWFS6_3lrc_20');
mkdir('lap_subjects_RWFS5_congIncong');

%% Qmodels
%standard model
parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_RWFS4_20_subj = fullfile('lap_subjects_RWFS4_20',['lap_RWFS4_20_', num2str(i), '.mat']);
cbm_lap(data_subj, @Q_model_FS, prior_RWFS4, fname_RWFS4_20_subj);
end

lap_RWFS4_20 = cell(697,1);
for n=1:697
    lap_RWFS4_20{n} = fullfile('lap_subjects_RWFS4_20',['lap_RWFS4_20_' num2str(n) '.mat']);
end

fname_BF = 'lap_RWFS4_20.mat';
cbm_lap_aggregate(lap_RWFS4_20,fname_BF);

%2 lrc
parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_RWFS5_updown_20_subj = fullfile('lap_subjects_RWFS5_updown_20',['lap_RWFS5_updown_20_', num2str(i), '.mat']);
cbm_lap(data_subj, @Q_model_FS_updown, prior_RWFS5_updown, fname_RWFS5_updown_20_subj);
end

lap_RWFS5_updown_20 = cell(697,1);
for n=1:697
    lap_RWFS5_updown_20{n} = fullfile('lap_subjects_RWFS5_updown_20',['lap_RWFS5_updown_20_' num2str(n) '.mat']);
end

fname_BF = 'lap_RWFS5_updown_20.mat';
cbm_lap_aggregate(lap_RWFS5_updown_20,fname_BF);

%3 lrc
parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_RWFS6_3lrc_20_subj = fullfile('lap_subjects_RWFS6_3lrc_20',['lap_RWFS6_3lrc_20_', num2str(i), '.mat']);
cbm_lap(data_subj, @Q_model_FS_3lrc, prior_RWFS6_3lrc, fname_RWFS6_3lrc_20_subj);
end

lap_RWFS6_3lrc_20 = cell(697,1);
for n=1:697
    lap_RWFS6_3lrc_20{n} = fullfile('lap_subjects_RWFS6_3lrc_20',['lap_RWFS6_3lrc_20_' num2str(n) '.mat']);
end

fname_BF = 'lap_RWFS6_3lrc_20.mat';
cbm_lap_aggregate(lap_RWFS6_3lrc_20,fname_BF);

%2 cong incong lrc
parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_RWFS5_congIncong_subj = fullfile('lap_subjects_RWFS5_congIncong',['lap_RWFS5_congIncong_', num2str(i), '.mat']);
cbm_lap(data_subj, @Q_model_FS_congIncong, prior_RWFS5_congIncong, fname_RWFS5_congIncong_subj);
end

lap_RWFS5_congIncong = cell(697,1);
for n=1:697
    lap_RWFS5_congIncong{n} = fullfile('lap_subjects_RWFS5_congIncong',['lap_RWFS5_congIncong_' num2str(n) '.mat']);
end

fname_BF = 'lap_RWFS5_congIncong.mat';
cbm_lap_aggregate(lap_RWFS5_congIncong,fname_BF);

%% Bayes models

%2 bayes

parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_BFS2_20_subj = fullfile('lap_subjects_BFS2_20',['lap_BFS2_20_', num2str(i), '.mat']);
cbm_lap(data_subj, @FSModel_Phase2_BetaOnly, prior_BFS2, fname_BFS2_20_subj);
end

lap_BFS2_20 = cell(697,1);
for n=1:697
    lap_BFS2_20{n} = fullfile('lap_subjects_BFS2_20',['lap_BFS2_20_' num2str(n) '.mat']);
end

fname_BF = 'lap_BFS2_20.mat';
cbm_lap_aggregate(lap_BFS2_20,fname_BF);

% 4 Bayes

parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_BFS4_20_subj = fullfile('lap_subjects_BFS4_20',['lap_BFS4_20_', num2str(i), '.mat']);
cbm_lap(data_subj, @FSModel_1_Phase2, prior_BFS4, fname_BFS4_20_subj);
end

lap_BFS4_20 = cell(697,1);
for n=1:697
    lap_BFS4_20{n} = fullfile('lap_subjects_BFS4_20',['lap_BFS4_20_' num2str(n) '.mat']);
end

fname_BF = 'lap_BFS4_20.mat';
cbm_lap_aggregate(lap_BFS4_20,fname_BF);

% 4 Bayes zeta

parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_BFSzeta_20_subj = fullfile('lap_subjects_BFSzeta_20',['lap_BFSzeta_20_', num2str(i), '.mat']);
cbm_lap(data_subj, @FSModel_1_Phase2_zeta, prior_BFSzeta, fname_BFSzeta_20_subj);
end

lap_BFSzeta_20 = cell(697,1);
for n=1:697
    lap_BFSzeta_20{n} = fullfile('lap_subjects_BFSzeta_20',['lap_BFSzeta_20_' num2str(n) '.mat']);
end

fname_BF = 'lap_BFSzeta_20.mat';
cbm_lap_aggregate(lap_BFSzeta_20,fname_BF);

% 4 Bayes shrink

parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_BFShrink_20_subj = fullfile('lap_subjects_BFShrink_20',['lap_BFShrink_20_', num2str(i), '.mat']);
cbm_lap(data_subj, @FSModel_1_Phase2_shrink, prior_BFShrink, fname_BFShrink_20_subj);
end

lap_BFShrink_20 = cell(697,1);
for n=1:697
    lap_BFShrink_20{n} = fullfile('lap_subjects_BFShrink_20',['lap_BFShrink_20_' num2str(n) '.mat']);
end

fname_BF = 'lap_BFShrink_20.mat';
cbm_lap_aggregate(lap_BFShrink_20,fname_BF);

% 6 Bayes null

parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_BFS6_20_subj = fullfile('lap_subjects_BFS6_20',['lap_BFS6_20_', num2str(i), '.mat']);
cbm_lap(data_subj, @FSModel_1_Phase2_7parms, prior_BFS6_null, fname_BFS6_20_subj);
end

lap_BFS6_20 = cell(697,1);
for n=1:697
    lap_BFS6_20{n} = fullfile('lap_subjects_BFS6_20',['lap_BFS6_20_' num2str(n) '.mat']);
end

fname_BF = 'lap_BF6_20.mat';
cbm_lap_aggregate(lap_BFS6_20,fname_BF);

%% Extra models

% 6 Bayes Choice_congruency

parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_BFS6_ChoiceCong_subj = fullfile('lap_subjects_BFS6_ChoiceCong',['lap_BFS6_ChoiceCong_', num2str(i), '.mat']);
cbm_lap(data_subj, @FSModel_1_Phase2_EPSchoice, prior_ChoiceCong, fname_BFS6_ChoiceCong_subj);
end

lap_BFS6_ChoiceCong = cell(697,1);
for n=1:697
    lap_BFS6_ChoiceCong{n} = fullfile('lap_subjects_BFS6_ChoiceCong',['lap_BFS6_ChoiceCong_' num2str(n) '.mat']);
end

fname_BF = 'lap_BF6_ChoiceCong.mat';
cbm_lap_aggregate(lap_BFS6_ChoiceCong,fname_BF);

% 6 Bayes Action_congruency

parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_BFS6_ActionCong_subj = fullfile('lap_subjects_BFS6_ActionCong',['lap_BFS6_ActionCong_', num2str(i), '.mat']);
cbm_lap(data_subj, @FSModel_1_Phase2_EPSaction, prior_ActionCong, fname_BFS6_ActionCong_subj);
end

lap_BFS6_ActionCong = cell(697,1);
for n=1:697
    lap_BFS6_ActionCong{n} = fullfile('lap_subjects_BFS6_ActionCong',['lap_BFS6_ActionCong_' num2str(n) '.mat']);
end

fname_BF = 'lap_BF6_ActionCong.mat';
cbm_lap_aggregate(lap_BFS6_ActionCong,fname_BF);

% 5 Bayes Positive bias

parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_BFS5_FavBias_subj = fullfile('lap_subjects_BFS5_FavBias',['lap_BFS5_FavBias_', num2str(i), '.mat']);
cbm_lap(data_subj, @FSModel_1_Phase2_FavBias, prior_FavBias, fname_BFS5_FavBias_subj);
end

lap_BFS5_FavBias = cell(697,1);
for n=1:697
    lap_BFS5_FavBias{n} = fullfile('lap_subjects_BFS5_FavBias',['lap_BFS5_FavBias_' num2str(n) '.mat']);
end

fname_BF = 'lap_BFS5_FavBias.mat';
cbm_lap_aggregate(lap_BFS5_FavBias,fname_BF);

%% check single model fit%

BFSZetCBM = load('lap_BFSzeta_20.mat');
BFSZetCBM.cbm.output.parameters(:,1) = (1./(1+exp(-BFSZetCBM.cbm.output.parameters(:, 1))))*15;
BFSZetCBM.cbm.output.parameters(:,3) = exp(BFSZetCBM.cbm.output.parameters(:, 3));
BFSZetCBM.cbm.output.parameters(:,4) = exp(BFSZetCBM.cbm.output.parameters(:, 4));
BFSZetCBM.cbm.output.parameters(:,5) = (1./(1+exp(-BFSZetCBM.cbm.output.parameters(:, 5))));
histogram(BFSZetCBM.cbm.output.parameters(:,3), 100); hold on; histogram(BFSZetCBM.cbm.output.parameters(:,4), 100)
histogram(BFSZetCBM.cbm.output.parameters(:,1), 100); hold on; histogram(BFSZetCBM.cbm.output.parameters(:,2), 100)
histogram(BFSZetCBM.cbm.output.parameters(:,5), 100)

BFshrinkCBM = load('lap_BFshrink_20.mat');
BFshrinkCBM.cbm.output.parameters(:,1) = (1./(1+exp(-BFshrinkCBM.cbm.output.parameters(:, 1))))*15;
BFshrinkCBM.cbm.output.parameters(:,3) = exp(BFshrinkCBM.cbm.output.parameters(:, 3));
BFshrinkCBM.cbm.output.parameters(:,4) = exp(BFshrinkCBM.cbm.output.parameters(:, 4));
BFshrinkCBM.cbm.output.parameters(:,5) = (1./(1+exp(-BFshrinkCBM.cbm.output.parameters(:, 5))));
histogram(BFshrinkCBM.cbm.output.parameters(:,3), 100); hold on; histogram(BFshrinkCBM.cbm.output.parameters(:,4), 100)
histogram(BFshrinkCBM.cbm.output.parameters(:,1), 100); hold on; histogram(BFshrinkCBM.cbm.output.parameters(:,2), 100)
histogram(BFshrinkCBM.cbm.output.parameters(:,5), 100)

BFS4CBM = load('lap_BFS4TEST.mat');
BFS4CBM.cbm.output.parameters(:,1) = 15*(1./(1+(exp(-BFS4CBM.cbm.output.parameters(:, 1)))));
BFS4CBM.cbm.output.parameters(:,3) = exp(BFS4CBM.cbm.output.parameters(:, 3));
BFS4CBM.cbm.output.parameters(:,4) = exp(BFS4CBM.cbm.output.parameters(:, 4));
histogram(BFS4CBM.cbm.output.parameters(:,3), 100); hold on; histogram(BFS4CBM.cbm.output.parameters(:,4), 100)
histogram(BFS4CBM.cbm.output.parameters(:,1), 100); hold on; histogram(BFS4CBM.cbm.output.parameters(:,2), 100)

BFS2CBM = load('lap_BFS2_20.mat');
BFS2CBM.cbm.output.parameters(:,2) = exp(BFS2CBM.cbm.output.parameters(:, 2));
histogram(BFS2CBM.cbm.output.parameters(:,1), 100); hold on; histogram(BFS2CBM.cbm.output.parameters(:,2), 100)

%% HBM CBM for bayes

models = {  @FSModel_1_Phase2_FavBias, @FSModel_1_Phase2_EPSaction, @FSModel_1_Phase2_EPSchoice, @FSModel_1_Phase2_7parms, @FSModel_1_Phase2_shrink, @FSModel_1_Phase2_zeta, @FSModel_1_Phase2, @FSModel_Phase2_BetaOnly, @Q_model_FS, @Q_model_FS_updown, @Q_model_FS_3lrc, @Q_model_FS_congIncong};

fcbm_maps = {'lap_BFS5_FavBias.mat',    'lap_BF6_ActionCong.mat',     'lap_BF6_ChoiceCong.mat',     'lap_BFS6_null.mat',       'lap_BFshrink.mat',      'lap_BFSzeta.mat',     'lap_BFS4TEST.mat',   'lap_BFS2.mat',    'lap_RWFS4.mat','lap_RWFS5_updown.mat','lap_RWFS6_3lrc.mat','lap_RWFS5_congIncong.mat'};

BFS_hbi = 'hbi_12models.mat';

cbm_hbi(sb,models,fcbm_maps,BFS_hbi);

%% load output

BFS_hbi = load('hbi_12models.mat');
cbm = BFS_hbi.cbm;
cbm.output

model_frequency      = cbm.output.model_frequency;
model_responsibility = cbm.output.responsibility;

group_mean_BFS6_ChoiceCong       = cbm.output.group_mean{3};

group_mean_BFS6_ChoiceCong(1)    = (1./(1+exp(-group_mean_BFS6_ChoiceCong(1))))*15;
group_mean_BFS6_ChoiceCong(3)    = exp(group_mean_BFS6_ChoiceCong(3));
group_mean_BFS6_ChoiceCong(4)    = exp(group_mean_BFS6_ChoiceCong(4));
group_mean_BFS6_ChoiceCong(5)    = (1./(1+exp(-group_mean_BFS6_ChoiceCong(5))));
group_mean_BFS6_ChoiceCong(6)    = (1./(1+exp(-group_mean_BFS6_ChoiceCong(6))));

BFS_hbi.cbm.output.parameters{3}(:,1) = (1./(1+exp(-BFS_hbi.cbm.output.parameters{3}(:, 1))))*15;
BFS_hbi.cbm.output.parameters{3}(:,3) = exp(BFS_hbi.cbm.output.parameters{3}(:, 3));
BFS_hbi.cbm.output.parameters{3}(:,4) = exp(BFS_hbi.cbm.output.parameters{3}(:, 4));
histogram(BFS_hbi.cbm.output.parameters{3}(:,3), 100); hold on; histogram(BFS_hbi.cbm.output.parameters{3}(:,4), 100)
histogram(BFS_hbi.cbm.output.parameters{3}(:,1), 100); hold on; histogram(BFS_hbi.cbm.output.parameters{3}(:,2), 100)

%% null HBI 
% Note: takes a long time.

fname_BFS_RW_hbi = 'hbi_8models_Alpha15TEST.mat';

cbm_hbi_null(sb,fname_BFS_RW_hbi);

%% Check null model

BFS_hbi = load('hbi_12models.mat');
cbm   = BFS_hbi.cbm;
pxp   = cbm.output.protected_exceedance_prob;
xp    = cbm.output.exceedance_prob;

group_mean_BFS4       = cbm.output.group_mean{3};
group_mean_BFS4(1)    = (1./(1+exp(-group_mean_BFS4(1))))*15;
group_mean_BFS4(3)    = exp(group_mean_BFS4(3));
group_mean_BFS4(4)    = exp(group_mean_BFS4(4));
group_mean_BFS4(5)    = (1./(1+exp(-group_mean_BFS4(5))));
group_mean_BFS4(6)    = (1./(1+exp(-group_mean_BFS4(6))));

%% model error check

%% step1: 

% ensure the model is set to output probabilities of the model
% as well as likelihood for each phase, simulated action, and reward
% contingencies

cbmReal      = load('hbi_12models.mat');
optim_parms6 = cbmReal.cbm.output.parameters;

lik1 = zeros(697,1);
lik2 = lik1;
F    = lik1;
simA = cell(697,1);
prob1= cell(697,1);
prob2= cell(697,1);
rew  = cell(697,1);
action=cell(697,1);
u1    =cell(697,1);
u2    =cell(697,1);
cong  =cell(697,1);
incong=cell(697,1);

for i=1:697
   
    [lik1(i), lik2(i), F(i), simA{i}, prob1{i}, prob2{i}, rew{i}, u1{i}, u2{i}, cong{i}, incong{i}] = FSModel_1_Phase2_modcheck(optim_parms4{6}(i, :), sb{i});
    action{i} = sb{i}(:,7);
    
end

save modelerror12mod_BFCong.mat lik1 lik2 F simA prob1 prob2 rew u1 u2 cong incong action

%% step2: 

% run laplace approximation on the generated action values.
% create a directory for individual output files:

mkdir('lap_subjects_BFS4_recovery_check1');
v = 7.5; 
prior_BFS4_Rec = struct('mean', zeros(4,1), 'variance', v);

simAtouse = load('modelerror8mod_BFS4_Alpha15CheckCheck.mat');

sb_rec_dat = cell(697, 1);
for i=1:697
sb_rec_dat{i}      = sb{i};
sb_rec_dat{i}(:,7) = simAtouse.simA{i}; 
end

parfor i = 1:697
% 1st input: data
data_subj = sb_rec_dat(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_BFS4_subj = fullfile('lap_subjects_BFS4_recovery_check1',['lap_BFS4_rec_', num2str(i), '.mat']);
cbm_lap(data_subj, @FSModel_1_Phase2, prior_BFS4_Rec, fname_BFS4_subj);
end

lap_BFS4_rec1 = cell(697,1);
for n=1:length(lap_BFS4_rec1)
    lap_BFS4_rec1{n} = fullfile('lap_subjects_BFS4_recovery_check1',['lap_BFS4_rec_' num2str(n) '.mat']);
end

fname_BF = 'lap_BFS4_recovery1.mat';
cbm_lap_aggregate(lap_BFS4_rec1,fname_BF);

CBMRECTEST = load('lap_BFS4_recovery1.mat');
histogram(exp(CBMRECTEST.cbm.output.parameters(:,3)), 100); hold on; histogram(BFS4CBM.cbm.output.parameters(:,3), 100)

%% Test how well the model captures individualistic partners

% Step 1 - treat partner decisions as if they were real decisions

sb_Partner = cell(697, 1);

for i=1:697
sb_Partner{i}           = sb{i};
sb_Partner{i}(19:54,7)  = sb{i}(19:54, 8); 
sb_Partner{i}           = sb_Partner{i}(19:54,:);
sb_Partner{i}(:, 2)     = 1:36;
end

sb_Partner18 = cell(697, 1);

for i=1:697
sb_Partner18{i}           = sb{i};
sb_Partner18{i}(19:36,7)  = sb{i}(19:36, 8); 
sb_Partner18{i}           = sb_Partner18{i}(19:36,:);
sb_Partner18{i}(:, 2)     = 1:18;
end

% Step 2 - fit the model

v = 7.5; 
prior_sb_Partner = struct('mean', zeros(2,1), 'variance', v);

% create a directory for individual output files:
mkdir('lap_subjects_sb_Partner');
mkdir('lap_subjects_sb_Partner18');

% 36 trials
parfor i = 1:697
% 1st input: data
data_subj = sb_Partner(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_sb_Partner = fullfile('lap_subjects_sb_Partner',['lap_sb_Partner_', num2str(i), '.mat']);
cbm_lap(data_subj, @FSModel_Phase1, prior_sb_Partner, fname_sb_Partner);
end

%18 trials
parfor i = 1:697
% 1st input: data
data_subj = sb_Partner18(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_sb_Partner18 = fullfile('lap_subjects_sb_Partner18',['lap_sb_Partner18_', num2str(i), '.mat']);
cbm_lap(data_subj, @FSModel_Phase1, prior_sb_Partner, fname_sb_Partner18);
end

lap_sb_Partner = cell(697,1);
for n=1:length(lap_sb_Partner)
    lap_sb_Partner{n} = fullfile('lap_subjects_sb_Partner',['lap_sb_Partner_' num2str(n) '.mat']);
end

lap_sb_Partner18 = cell(697,1);
for n=1:length(lap_sb_Partner18)
    lap_sb_Partner18{n} = fullfile('lap_subjects_sb_Partner18',['lap_sb_Partner18_' num2str(n) '.mat']);
end

fname_BF = 'lap_sb_Partner.mat';
cbm_lap_aggregate(lap_sb_Partner,fname_BF);

fname_BF18 = 'lap_sb_Partner18.mat';
cbm_lap_aggregate(lap_sb_Partner18,fname_BF18);