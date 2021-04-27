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
D3sim   = table2array(Dbsim(:,contains(Dbsim.Properties.VariableNames, {'ppt', 'Trial', 'Option1_PPT', 'Option1_Partner', 'Option2_PPT', 'Option2_Partner', 'Response', 'Answer', 'alpha', 'beta', 'alpha2', 'beta2', 'alpha_v', 'beta_v', 'zeta'})));
D3sim   = D3sim(:,[15, 7, 2, 3, 4, 5, 1,6,8, 9, 10, 11, 12, 13, 14]);

subs = unique(D3(:,1));
subsSim = unique(D3sim(:,9));
% 697 participants, each associated with a 18 x 7 data matrix
n = 697;
nSim = 1000;
 
%--------------------------------

d = size(D3,1)/n;
dsim = size(D3sim,1)/nSim;

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

for i=1:length(sb)
[F(i)] = FSModel_1_Phase2([2, -2, 1, 1, 1], sb{i});
end
%RW test
dataTest = [sb_phase2{1}];
dataTest(:,9) = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,1,1,1,1,1,1];
dataTest(:,10) = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1];
[FTest, QTest, simATest] = Q_model_FS([-1, 2, 4, -4], dataTest, 1, 1);

%% Recovery of model
v = 5; 
prior_sim = struct('mean', zeros(5,1), 'variance', v);
fname_SIM5 =  'lap_BFS5SIM.mat';
for i=1:100
sb_check{i} = sbSim{i};
end
cbm_lap(sb_check, @FSModel_1_Phase2, prior_sim, fname_SIM5)

BFS5SIM = load('lap_BFS5SIM.mat');
BFS5SIM.cbm.output.parameters(:,1) = (1./(1+exp(-BFS5SIM.cbm.output.parameters(:, 1))))*10;
BFS5SIM.cbm.output.parameters(:,3) = (1./(1+exp(-BFS5SIM.cbm.output.parameters(:, 3))))*5;
BFS5SIM.cbm.output.parameters(:,4) = (1./(1+exp(-BFS5SIM.cbm.output.parameters(:, 4))))*5;
BFS5SIM.cbm.output.parameters(:,5) = (1./(1+exp(-BFS5SIM.cbm.output.parameters(:, 5))));

for i=1:100
  check(i, 1:7) = sb_check{i}(1,9:15);  
end
scatter(check(:,2), BFS5SIM.cbm.output.parameters(:,2))

%% run models in CDM 

v = 7.5; 
prior_BFS6_null = struct('mean', zeros(6,1), 'variance', v);
prior_BFS7_null = struct('mean', zeros(7,1), 'variance', v);
prior_BFS5_shrink = struct('mean', zeros(5,1), 'variance', v);
prior_BFS5_zeta   = struct('mean', zeros(5,1), 'variance', v);
prior_BFS4   = struct('mean', zeros(4,1), 'variance', v);
prior_BFS2   = struct('mean', zeros(2,1), 'variance', v);
prior_RWFS_6 = struct('mean', zeros(6,1), 'variance', v);
prior_RWFS_5 = struct('mean', zeros(5,1), 'variance', v);
prior_RWFS_4 = struct('mean', zeros(4,1), 'variance', v);
prior_RWFS_8_3lrc = struct('mean', zeros(8,1), 'variance', v);
prior_RWFS_7_3lrc = struct('mean', zeros(7,1), 'variance', v);
prior_RWFS_6_3lrc = struct('mean', zeros(6,1), 'variance', v);
prior_RWFS_7_updown = struct('mean', zeros(7,1), 'variance', v);
prior_RWFS_6_updown = struct('mean', zeros(6,1), 'variance', v);
prior_RWFS_5_updown = struct('mean', zeros(5,1), 'variance', v);

fname_BFS6_null =  'lap_BFS6_null.mat';
fname_BFS7_null =  'lap_BFS7_null.mat';
fname_BFS5_shrink =  'lap_BFS_shrink.mat';
fname_BFS5_zeta =  'lap_BFS_zeta.mat';
fname_BFS4 =  'lap_BFS4.mat';
fname_BFS2 =  'lap_BFS2.mat';
fname_RWFS6 = 'lap_RWFS6.mat';
fname_RWFS5 = 'lap_RWFS5.mat';
fname_RWFS4 = 'lap_RWFS4.mat';
fname_RWFS8_3lrc = 'lap_RWFS8_3lrc.mat';
fname_RWFS7_3lrc = 'lap_RWFS7_3lrc.mat';
fname_RWFS6_3lrc = 'lap_RWFS6_3lrc.mat';
fname_RWFS7_updown = 'lap_RWFS7_updown.mat';
fname_RWFS6_updown = 'lap_RWFS6_updown.mat';
fname_RWFS5_updown = 'lap_RWFS5_updown.mat';

% CBM lab
%cbm_lap(sb, @FSModel_1_Phase2_7parms, prior_BFS6_null, fname_BFS6_null); 6                 %  wo zeta; alpha_m, alpha_v, beta_m, beta_v, alpha_null, beta_null % 6 parms
%cbm_lap(sb, @FSModel_1_Phase2_7parms, prior_BFS7_null, fname_BFS7_null); 7                 %  w zeta; alpha_m, alpha_v, beta_m, beta_v, alpha_null, beta_null % 7 parms
%cbm_lap(sb, @FSModel_1_Phase2_shrink, prior_BFS_shrink, fname_BFS_shrink); 5      %  wo zeta; alpha_m, alpha_v, beta_m, beta_v, shrinkage % 5 parms
%cbm_lap(sb, @FSModel_1_Phase2_zeta, prior_BFS_zeta, fname_BFS_zeta); 5            %  w zeta w mean % 5 parms
%cbm_lap(sb, @FSModel_1_Phase2, prior_BFS_4, fname_BFS4); 4                        %  wo zeta w mean % 4 parms
%cbm_lap(sb, @Q_model_FS, prior_RWFS_6, fname_RWFS7); 6                            %  alpha, beta, tau, lrc, zet, mem % 6 parms
%cbm_lap(sb, @Q_model_FS, prior_RWFS_5, fname_RWFS5); 5                            %  alpha, beta, tau, lrc, zet % 5 parms
%cbm_lap(sb, @Q_model_FS, prior_RWFS_4, fname_RWFS4); 4                            %  alpha, beta, tau, lrc % 4 parms
%cbm_lap(sb, @Q_model_FS_3lrc, prior_RWFS_8_3lrc, fname_RWFS8_3lrc); 8             %  alpha, beta, tau, lrc x 3, zet, mem % 8 parms
%cbm_lap(sb, @Q_model_FS_3lrc, prior_RWFS_7_3lrc, fname_RWFS7_3lrc); 7             %  alpha, beta, tau, lrc x 3, zet % 7 parms
%cbm_lap(sb, @Q_model_FS_3lrc, prior_RWFS_6_3lrc, fname_RWFS6_3lrc); 6             %  alpha, beta, tau, lrc x 3 % 6 parms
%cbm_lap(sb, @Q_model_FS_updown, prior_RWFS_7_updown, fname_RWFS7_updown); 7       %  alpha, beta, tau, lrc x 2, zet, mem % 7 parms
%cbm_lap(sb, @Q_model_FS_updown, prior_RWFS_6_updown, fname_RWFS6_updown); 6       %  alpha, beta, tau, lrc x 2, zet % 6 parms
%cbm_lap(sb, @Q_model_FS_updown, prior_RWFS_5_updown, fname_RWFS5_updown); 5       %  alpha, beta, tau, lrc x 2 % 5 parms


%% parfor loops

% create a directory for individual output files:
mkdir('lap_subjects_BFS2');

parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_BFS2_subj = fullfile('lap_subjects_BFS2',['lap_BFS2_', num2str(i), '.mat']);
cbm_lap(data_subj, @FSModel_Phase2_BetaOnly, prior_BFS2, fname_BFS2_subj);
end

lap_BFS2 = cell(697,1);
for n=1:length(lap_BFS2)
    lap_BFS2{n} = fullfile('lap_subjects_BFS2',['lap_BFS2_' num2str(n) '.mat']);
end

fname_BF = 'lap_BFS2.mat';
cbm_lap_aggregate(lap_BFS2,fname_BF);

%% check single model fit%

BFSZetCBM = load('lap_BFSzeta.mat');
BFSZetCBM.cbm.output.parameters(:,1) = (1./(1+exp(-BFSZetCBM.cbm.output.parameters(:, 1))))*10;
BFSZetCBM.cbm.output.parameters(:,3) = exp(BFSZetCBM.cbm.output.parameters(:, 3));
BFSZetCBM.cbm.output.parameters(:,4) = exp(BFSZetCBM.cbm.output.parameters(:, 4));
BFSZetCBM.cbm.output.parameters(:,5) = (1./(1+exp(-BFSZetCBM.cbm.output.parameters(:, 5))));
histogram(BFSZetCBM.cbm.output.parameters(:,3), 100); hold on; histogram(BFSZetCBM.cbm.output.parameters(:,4), 100)
histogram(BFSZetCBM.cbm.output.parameters(:,1), 100); hold on; histogram(BFSZetCBM.cbm.output.parameters(:,2), 100)
histogram(BFSZetCBM.cbm.output.parameters(:,5), 100)

BFshrinkCBM = load('lap_BFshrink.mat');
BFshrinkCBM.cbm.output.parameters(:,1) = (1./(1+exp(-BFshrinkCBM.cbm.output.parameters(:, 1))))*10;
BFshrinkCBM.cbm.output.parameters(:,3) = exp(BFshrinkCBM.cbm.output.parameters(:, 3));
BFshrinkCBM.cbm.output.parameters(:,4) = exp(BFshrinkCBM.cbm.output.parameters(:, 4));
BFshrinkCBM.cbm.output.parameters(:,5) = (1./(1+exp(-BFshrinkCBM.cbm.output.parameters(:, 5))));
histogram(BFshrinkCBM.cbm.output.parameters(:,3), 100); hold on; histogram(BFshrinkCBM.cbm.output.parameters(:,4), 100)
histogram(BFshrinkCBM.cbm.output.parameters(:,1), 100); hold on; histogram(BFshrinkCBM.cbm.output.parameters(:,2), 100)
histogram(BFshrinkCBM.cbm.output.parameters(:,5), 100)

BFS4CBM = load('lap_BFS4.mat');
BFS4CBM.cbm.output.parameters(:,1) = (1./(1+exp(-BFS4CBM.cbm.output.parameters(:, 1))))*10;
BFS4CBM.cbm.output.parameters(:,3) = exp(-BFS4CBM.cbm.output.parameters(:, 3));
BFS4CBM.cbm.output.parameters(:,4) = exp(-BFS4CBM.cbm.output.parameters(:, 4));
histogram(BFS4CBM.cbm.output.parameters(:,3), 100); hold on; histogram(BFS4CBM.cbm.output.parameters(:,4), 100)
histogram(BFS4CBM.cbm.output.parameters(:,1), 100); hold on; histogram(BFS4CBM.cbm.output.parameters(:,2), 100)

BFS2CBM = load('lap_BFS2.mat');
BFS2CBM.cbm.output.parameters(:,2) = exp(-BFS2CBM.cbm.output.parameters(:, 2));
histogram(BFS2CBM.cbm.output.parameters(:,1), 100); hold on; histogram(BFS2CBM.cbm.output.parameters(:,2), 100)

%% HBM CBM for bayes

models = {@FSModel_1_Phase2_7parms, @FSModel_1_Phase2_shrink, @FSModel_1_Phase2_zeta, @FSModel_1_Phase2, @FSModel_Phase2_BetaOnly, @Q_model_FS, @Q_model_FS_updown, @Q_model_FS_3lrc};

fcbm_maps = {'lap_BFS6_null.mat','lap_BFshrink.mat', 'lap_BFSzeta.mat', 'lap_BFS4.mat', 'lap_BFS2.mat','lap_RWFS4.mat', 'lap_RWFS5_updown.mat', 'lap_RWFS6_3lrc.mat'};

BFS_hbi = 'hbi_BFS_RW_8mods.mat';

cbm_hbi(sb,models,fcbm_maps,BFS_hbi);

%% load output

BFS_hbi = load('hbi_BFS_RW_8mods.mat');
cbm = BFS_hbi.cbm;
cbm.output

model_frequency      = cbm.output.model_frequency;
model_responsibility = cbm.output.responsibility;

group_mean_BFS4       = cbm.output.group_mean{4};

group_mean_BFS4(1)    = (1./(1+exp(-group_mean_BFS4(1))))*10;
group_mean_BFS4(3)    = exp(group_mean_BFS4(3));
group_mean_BFS4(4)    = exp(group_mean_BFS4(4));

BFS_hbi.cbm.output.parameters{4}(:,1) = (1./(1+exp(-BFS_hbi.cbm.output.parameters{4}(:, 1))))*10;
BFS_hbi.cbm.output.parameters{4}(:,3) = exp(BFS_hbi.cbm.output.parameters{4}(:, 3));
BFS_hbi.cbm.output.parameters{4}(:,4) = exp(BFS_hbi.cbm.output.parameters{4}(:, 4));
histogram(BFS_hbi.cbm.output.parameters{4}(:,3), 100); hold on; histogram(BFS_hbi.cbm.output.parameters{4}(:,4), 100)
histogram(BFS_hbi.cbm.output.parameters{4}(:,1), 100); hold on; histogram(BFS_hbi.cbm.output.parameters{4}(:,2), 100)

%% null HBI
fname_BFS_RW_hbi = 'hbi_BFS_RW_8mods.mat';

cbm_hbi_null(sb,fname_BFS_RW_hbi);

BFS_hbi = load('hbi_BFS_RW_8mods.mat');
cbm   = BFS_hbi.cbm;
pxp   = cbm.output.protected_exceedance_prob;
xp    = cbm.output.exceedance_prob;

group_mean_BFS4       = cbm.output.group_mean{4};
group_mean_BFS4(1)    = (1./(1+exp(-group_mean_BFS4(1))))*10;
group_mean_BFS4(3)    = exp(group_mean_BFS4(3));
group_mean_BFS4(4)    = exp(group_mean_BFS4(4));

%% model error check

%% step1: 

% ensure the model is set to output probabilities of the model
% as well as likelihood for each phase, simulated action, and reward
% contingencies

%BFS4CBM = load('lap_BFS4.mat');
optim_parms = cbm.output.parameters{4};

lik1 = zeros(697,1);
lik2 = lik1;
F    = lik1;
simA = cell(697,1);
prob1= simA;
prob2= simA;
rew  = simA;
action=simA;

for i=1:length(sb)
   
    [lik1(i), lik2(i), F(i), simA{i}, prob1{i}, prob2{i}, rew{i}] = FSModel_1_Phase2_modcheck(optim_parms(i, :), sb{i});
    action{i} = sb{i}(1:54, 7);
    
end

save modelerror.mat lik1 lik2 F simA prob1 prob2 rew action

%% step2: 
% run laplace approximation on the generated action values.
% create a directory for individual output files:

mkdir('lap_subjects_BFS4_recovery');

sb_rec_dat = cell(697, 1);
for i=1:697
sb_rec_dat{i}      = sb{1};
sb_rec_dat{i}(:,7) = simA{i}; 
end

parfor i = 1:697
% 1st input: data
data_subj = sb_rec_dat(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_BFS4_subj = fullfile('lap_subjects_BFS4_recovery',['lap_BFS4_rec_', num2str(i), '.mat']);
cbm_lap(data_subj, @FSModel_1_Phase2, prior_BFS4, fname_BFS4_subj);
end

lap_BFS4_rec = cell(697,1);
for n=1:length(lap_BFS4_rec)
    lap_BFS4_rec{n} = fullfile('lap_subjects_BFS4_recovery',['lap_BFS4_rec_' num2str(n) '.mat']);
end

fname_BF = 'lap_BFS4_recovery.mat';
cbm_lap_aggregate(lap_BFS4_rec,fname_BF);