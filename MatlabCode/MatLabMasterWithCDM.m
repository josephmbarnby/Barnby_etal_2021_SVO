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

%% run models in CDM for bayes

v = 5; 
prior_BFS_7 = struct('mean', zeros(7,1), 'variance', v);
prior_BFS_5 = struct('mean', zeros(5,1), 'variance', v);
prior_BFS_4 = struct('mean', zeros(4,1), 'variance', v);
prior_RWFS_6 = struct('mean', zeros(6,1), 'variance', v);
prior_RWFS_5 = struct('mean', zeros(5,1), 'variance', v);
prior_RWFS_4 = struct('mean', zeros(4,1), 'variance', v);
prior_RWFS_8_3lrc = struct('mean', zeros(8,1), 'variance', v);
prior_RWFS_7_3lrc = struct('mean', zeros(7,1), 'variance', v);
prior_RWFS_6_3lrc = struct('mean', zeros(6,1), 'variance', v);
prior_RWFS_7_updown = struct('mean', zeros(7,1), 'variance', v);
prior_RWFS_6_updown = struct('mean', zeros(6,1), 'variance', v);
prior_RWFS_5_updown = struct('mean', zeros(5,1), 'variance', v);

fname_BFS7 =  'lap_BFS7.mat';
fname_BFS5 =  'lap_BFS5.mat';
fname_BFS4 =  'lap_BFS4.mat';
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
cbm_lap(sb, @FSModel_1_Phase2_7parms, prior_BFS_7, fname_BFS7);                   %  w zeta w null alpha and beta % 7 parms
%cbm_lap(sb, @FSModel_1_Phase2, prior_BFS_5, fname_BFS5);                         %  w zeta w mean % 5 parms
%cbm_lap(sb, @FSModel_1_Phase2, prior_BFS_4, fname_BFS4);                         %  wo zeta w mean % 4 parms
cbm_lap(sb, @Q_model_FS, prior_RWFS_6, fname_RWFS7);                              %  alpha, beta, tau, lrc, zet, mem % 6 parms
cbm_lap(sb, @Q_model_FS, prior_RWFS_5, fname_RWFS5);                              %  alpha, beta, tau, lrc, zet % 5 parms
%cbm_lap(sb, @Q_model_FS, prior_RWFS_4, fname_RWFS4);                             %  alpha, beta, tau, lrc % 4 parms
cbm_lap(sb, @Q_model_FS_3lrc, prior_RWFS_8_3lrc, fname_RWFS8_3lrc);               %  alpha, beta, tau, lrc x 3, zet, mem % 8 parms
cbm_lap(sb, @Q_model_FS_3lrc, prior_RWFS_7_3lrc, fname_RWFS7_3lrc);               %  alpha, beta, tau, lrc x 3, zet % 7 parms
cbm_lap(sb, @Q_model_FS_3lrc, prior_RWFS_6_3lrc, fname_RWFS6_3lrc);              %  alpha, beta, tau, lrc x 3 % 6 parms
cbm_lap(sb, @Q_model_FS_updown, prior_RWFS_7_updown, fname_RWFS7_updown);         %  alpha, beta, tau, lrc x 2, zet, mem % 7 parms
cbm_lap(sb, @Q_model_FS_updown, prior_RWFS_6_updown, fname_RWFS6_updown);         %  alpha, beta, tau, lrc x 2, zet % 6 parms
cbm_lap(sb, @Q_model_FS_updown, prior_RWFS_5_updown, fname_RWFS5_updown);         %  alpha, beta, tau, lrc x 2 % 5 parms

%% check single model fit%

BFS5CBM = load('lap_BFS5.mat');
BFS5CBM.cbm.output.parameters(:,1) = (1./(1+exp(-BFS5CBM.cbm.output.parameters(:, 1))))*10;
BFS5CBM.cbm.output.parameters(:,3) = (1./(1+exp(-BFS5CBM.cbm.output.parameters(:, 3))))*5;
BFS5CBM.cbm.output.parameters(:,4) = (1./(1+exp(-BFS5CBM.cbm.output.parameters(:, 4))))*5;
BFS5CBM.cbm.output.parameters(:,5) = (1./(1+exp(-BFS5CBM.cbm.output.parameters(:, 5))));
histogram(BFS5CBM.cbm.output.parameters(:,3), 100); hold on; histogram(BFS5CBM.cbm.output.parameters(:,4), 100)
histogram(BFS5CBM.cbm.output.parameters(:,1), 100); hold on; histogram(BFS5CBM.cbm.output.parameters(:,2), 100)

%% HBM CBM for bayes

models = {@FSModel_1_Phase2, @FSModel_1_Phase2, @FSModel_Phase2Alone, @FSModel_Phase2Alone, @Q_model_FS, @Q_model_FS, @Q_model_FS};

fcbm_maps = {'lap_BFS1.mat','lap_BFS2.mat', 'lap_BFS3.mat', 'lap_BFS4.mat', 'lap_RWFS4', 'lap_RWFS3', 'lap_RWFS2'};

BFS_hbi = 'hbi_BFS_RW.mat';

cbm_hbi(sb,models,fcbm_maps,BFS_hbi);

%% load output

sigParm = @(x)5*(1/(1+exp(-x)));
sig=@(x)(1./(1+exp(-x)));

BFS_hbi = load('hbi_BFS_RW.mat');
cbm = BFS_hbi.cbm;
cbm.output

model_frequency      = cbm.output.model_frequency;

group_mean_BFS1       = cbm.output.group_mean{1}
group_mean_BFS3       = cbm.output.group_mean{3}
group_mean_BFS1(1)    = sigParm(group_mean_BFS1(1))
group_mean_BFS1(2)    = sigParm(group_mean_BFS1(2))
group_mean_BFS1(3)    = sig(group_mean_BFS1(3))
group_mean_BFS3(1)    = sigParm(group_mean_BFS3(1))
group_mean_BFS3(2)    = sigParm(group_mean_BFS3(2))
group_mean_BFS3(4)    = sigParm(group_mean_BFS3(4))
group_mean_BFS3(5)    = sig(group_mean_BFS3(5))

%% null HBI
BFS_RW_hbi = 'hbi_BFS_RW.mat';

cbm_hbi_null(sb,BFS_hbi);

BFS_hbi = load('hbi_BFS1_BFS2');
cbm   = BFS_hbi.cbm;
pxp   = cbm.output.protected_exceedance_prob
xp    = cbm.output.exceedance_prob

group_mean_BFS1       = cbm.output.group_mean{1};
group_mean_BFS1(1)    = sigParm(group_mean_BFS1(1))
group_mean_BFS1(2)    = sigParm(group_mean_BFS1(2))

%% plot
% 1st input is the file-address of the file saved by cbm_hbi
fname_hbi = 'hbi_FS1_FS2.mat';

% 2nd input: a cell input containing model names
model_names = {'FS1', 'FS2'};
% note that they corresponds to models (so pay attention to the order)

% 3rd input: another cell input containing parameter names of the winning model
param_names = {'\alpha','\beta','\tau'};
% note that '\alpha^+' is in the latex format, which generates a latin alpha

% 4th input: another cell input containing transformation function associated with each parameter of the winning model
transform = {'none','none','none'};
% note that if you use a less usual transformation function, you should pass the handle here (instead of a string)

cbm_hbi_plot(fname_hbi, model_names, param_names, transform)
% this function creates a model comparison plot (exceednace probability and model frequency) as well as 
% a plot of transformed parameters of the most frequent model.

%% individual parameter estimates

parameters_FS1 = cbm.output.parameters{1};
parameters_FS2 = cbm.output.parameters{2};
responsibility = cbm.output.responsibility
plot(responsibility(:,2)); ylim([-.1 1.1])