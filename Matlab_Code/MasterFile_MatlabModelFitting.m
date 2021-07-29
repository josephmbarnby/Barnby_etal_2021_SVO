%%
% Barnby & Dayan (2021) Inequality Aversion and Paranoia
%
% Joe Barnby j.barnby@uq.edu.au 2021
%
%% MASTER FILE %%
%% Load data

addpath('CBMCode');
Db      = readtable('Intentions_BothPhase.csv');
D3      = table2array(Db(:,contains(Db.Properties.VariableNames, {'id', 'Trial', 'Option1_PPT', 'Option1_Partner', 'Option2_PPT', 'Option2_Partner', 'Response', 'Answer', 'GuessAction', 'Correct'})));
subs = unique(D3(:,1));
% 697 participants, each associated with a 18 x 7 data matrix
n = 697;

%--------------------------------

d = size(D3,1)/n;
sb = cell(697, 1);

for i = 1:n
    sb{i} = D3(1+(i-1)*d:d+(i-1)*d,:);
end

%% Test Model

% Test with person against competitive partner
% Test bayes model
Model_2([2, -2, 2, 2], sb{1}) % with info from phase 1 or zeta
Model_2([2, -2, 2, 2, 1], sb{1}) % with info from phase 1
Model_10([2, -2, 2, 2, 1, 2, -2], sb{1}) % with no info from phase 1
% Test heuristic model
Model_13([2, -2, 0.5, 0.1, 0.1, 0.9], sb{1}) % with 6 parameters
Model_12([2, -2, 0.5, 0.1, 0.1], sb{1}) % with 5 parameters
Model_11([2, -2, 0.5, 0.1], sb{1}) % with 4 parameters

F = zeros(697,1);

for i=1:length(sb)
[F(i)] = Model_2([2, -2, 1, 1, 1], sb{i});
end

%RW test
dataTest = [sb_phase2{1}];
dataTest(:,9) = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,1,1,1,1,1,1];
dataTest(:,10) = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1];
[FTest, QTest, simATest] = Model_11([-1, 2, 4, -4], dataTest, 1, 1);

%% Create priors for CBM

v = 7.5;
prior_Model_10 = struct('mean', zeros(6,1), 'variance', v);
prior_Model_8  = struct('mean', zeros(6,1), 'variance', v);
prior_Model_9  = struct('mean', zeros(6,1), 'variance', v);
prior_Model_4  = struct('mean', zeros(5,1), 'variance', v);
prior_Model_5  = struct('mean', zeros(5,1), 'variance', v);
prior_Model_2  = struct('mean', zeros(4,1), 'variance', v);
prior_Model_3  = struct('mean', zeros(2,1), 'variance', v);
prior_Model_7  = struct('mean', zeros(5,1), 'variance', v);
prior_Model_6  = struct('mean', zeros(5,1), 'variance', v);
prior_Model_1  = struct('mean', zeros(2,1), 'variance', v);
prior_Model_11 = struct('mean', zeros(4,1), 'variance', v);
prior_Model_13 = struct('mean', zeros(6,1), 'variance', v);
prior_Model_12 = struct('mean', zeros(5,1), 'variance', v);
prior_Model_14 = struct('mean', zeros(5,1), 'variance', v);
prior_Phase1   = struct('mean', zeros(2,1), 'variance', v);

%% parfor loops to run CBM

% first create directories for individual output files:
mkdir('lap_subjects_Model1');
mkdir('lap_subjects_Model2');
mkdir('lap_subjects_Model4');
mkdir('lap_subjects_Model5');
mkdir('lap_subjects_Model10');
mkdir('lap_subjects_Model8');
mkdir('lap_subjects_Model7');
mkdir('lap_subjects_Model9');
mkdir('lap_subjects_Model6');
mkdir('lap_subjects_Phase1');
mkdir('lap_subjects_Model11');
mkdir('lap_subjects_Model12');
mkdir('lap_subjects_Model13');
mkdir('lap_subjects_Model14');

%% Qmodels
%standard model
parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_Model11_subj = fullfile('lap_subjects_Model11',['lap_Model11_', num2str(i), '.mat']);
cbm_lap(data_subj, @Model_11, prior_Model_11, fname_Model11_subj);
end

lap_Model11 = cell(697,1);
for n=1:697
    lap_Model11{n} = fullfile('lap_subjects_Model11',['lap_Model11_' num2str(n) '.mat']);
end

fname_BF = 'lap_Model11.mat';
cbm_lap_aggregate(lap_Model11,fname_BF);

%2 lrc
parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_Model12_subj = fullfile('lap_subjects_Model12',['lap_Model12_', num2str(i), '.mat']);
cbm_lap(data_subj, @Model_12, prior_Model_12, fname_Model12_subj);
end

lap_Model12 = cell(697,1);
for n=1:697
    lap_Model12{n} = fullfile('lap_subjects_Model12',['lap_Model12_' num2str(n) '.mat']);
end

fname_BF = 'lap_RWFS5_updown_20.mat';
cbm_lap_aggregate(lap_Model12,fname_BF);

%3 lrc
parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_Model13_subj = fullfile('lap_subjects_Model13',['lap_Model13_', num2str(i), '.mat']);
cbm_lap(data_subj, @Model_13, prior_Model_13, fname_Model13_subj);
end

lap_Model13 = cell(697,1);
for n=1:697
    lap_Model13{n} = fullfile('lap_subjects_Model13',['lap_Model13_' num2str(n) '.mat']);
end

fname_BF = 'lap_RWFS6_3lrc_20.mat';
cbm_lap_aggregate(lap_Model13,fname_BF);

%2 cong incong lrc
parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_Model14_subj = fullfile('lap_subjects_Model14',['lap_Model14_', num2str(i), '.mat']);
cbm_lap(data_subj, @Model_14, prior_Model_14, fname_Model14_subj);
end

lap_Model14 = cell(697,1);
for n=1:697
    lap_Model14{n} = fullfile('lap_subjects_Model14',['lap_Model14_' num2str(n) '.mat']);
end

fname_BF = 'lap_Model14.mat';
cbm_lap_aggregate(lap_Model14,fname_BF);

%% Bayes models

%2 bayes

parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_Model1_subj = fullfile('lap_subjects_Model1',['lap_Model1_', num2str(i), '.mat']);
cbm_lap(data_subj, @Model_1, prior_Model_1, fname_Model1_subj);
end

lap_Model1 = cell(697,1);
for n=1:697
    lap_Model1{n} = fullfile('lap_Model1_20',['lap_Model1_' num2str(n) '.mat']);
end

fname_BF = 'lap_BFS2_20.mat';
cbm_lap_aggregate(lap_Model1,fname_BF);

% 4 Bayes

parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_Model2_subj = fullfile('lap_subjects_Model2',['lap_Model2_', num2str(i), '.mat']);
cbm_lap(data_subj, @Model_2, prior_Model_2, fname_Model2_subj);
end

lap_Model2 = cell(697,1);
for n=1:697
    lap_Model2{n} = fullfile('lap_subjects_Model2',['lap_Model2_' num2str(n) '.mat']);
end

fname_BF = 'lap_Model2.mat';
cbm_lap_aggregate(lap_Model2,fname_BF);

% 4 Bayes zeta

parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_BFSzeta_20_subj = fullfile('lap_subjects_BFSzeta_20',['lap_BFSzeta_20_', num2str(i), '.mat']);
cbm_lap(data_subj, @FSModel_1_Phase2_zeta, prior_Model_5, fname_BFSzeta_20_subj);
end

lap_Model5 = cell(697,1);
for n=1:697
    lap_Model5{n} = fullfile('lap_subjects_Model5',['lap_Model5_' num2str(n) '.mat']);
end

fname_BF = 'lap_Model5.mat';
cbm_lap_aggregate(lap_Model5,fname_BF);

% 4 Bayes shrink

parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_Model4_subj = fullfile('lap_subjects_Model4',['lap_Model4_', num2str(i), '.mat']);
cbm_lap(data_subj, @Model_4, prior_Model_4, fname_Model4_subj);
end

lap_Model4 = cell(697,1);
for n=1:697
    lap_Model4{n} = fullfile('lap_subjects_Model4',['lap_Model4_' num2str(n) '.mat']);
end

fname_BF = 'lap_Model4.mat';
cbm_lap_aggregate(lap_Model4,fname_BF);

% 6 Bayes null

parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_Model10_subj = fullfile('lap_subjects_Model10',['lap_Model10_', num2str(i), '.mat']);
cbm_lap(data_subj, @Model_10, prior_Model_10, fname_Model10_subj);
end

lap_Model10 = cell(697,1);
for n=1:697
    lap_Model10{n} = fullfile('lap_subjects_Model10',['lap_Model10_' num2str(n) '.mat']);
end

fname_BF = 'lap_Model10.mat';
cbm_lap_aggregate(lap_Model10,fname_BF);

% 6 Bayes Choice_congruency

parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_Model8_subj = fullfile('lap_subjects_Model8',['lap_Model8_', num2str(i), '.mat']);
cbm_lap(data_subj, @Model_8, prior_Model_8, fname_Model8_subj);
end

lap_Model8 = cell(697,1);
for n=1:697
    lap_Model8{n} = fullfile('lap_subjects_Model8',['lap_Model8_' num2str(n) '.mat']);
end

fname_BF = 'lap_Model8.mat';
cbm_lap_aggregate(lap_Model8,fname_BF);

% 6 Bayes Action_congruency
parfor i = 1:697
%i = run(idx);
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_Model9_subj = fullfile('lap_subjects_Model9',['lap_Model9_', num2str(i), '.mat']);
cbm_lap(data_subj, @Model_9, prior_Model_9, fname_Model9_subj);
end

lap_Model9 = cell(697,1);
for n=1:697
    lap_Model9{n} = fullfile('lap_subjects_Model9',['lap_Model9_' num2str(n) '.mat']);
end

fname_BF = 'lap_Model9.mat';
cbm_lap_aggregate(lap_Model9,fname_BF);

% 5 Bayes Positive bias

parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_Model6_subj = fullfile('lap_subjects_Model6',['lap_Model6_', num2str(i), '.mat']);
cbm_lap(data_subj, @Model_6, prior_Model_6, fname_Model6_subj);
end

lap_Model6 = cell(697,1);
for n=1:697
    lap_Model6{n} = fullfile('lap_subjects_Model6',['lap_Model6_' num2str(n) '.mat']);
end

fname_BF = 'lap_Model6.mat';
cbm_lap_aggregate(lap_Model6,fname_BF);

% 5 Bayes Epsilon

parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_Model7_subj = fullfile('lap_subjects_Model7',['lap_Model7_', num2str(i), '.mat']);
cbm_lap(data_subj, @Model_7, prior_Model_7, fname_Model7_subj);
end

lap_Model7 = cell(697,1);
for n=1:697
    lap_Model7{n} = fullfile('lap_subjects_Model7',['lap_Model7_' num2str(n) '.mat']);
end

fname_BF = 'lap_Model7.mat';
cbm_lap_aggregate(lap_Model7,fname_BF);

% 2 Bayes Subject ignores partner

parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_Model3_subj = fullfile('lap_subjects_Model3',['lap_Model3_', num2str(i), '.mat']);
cbm_lap(data_subj, @Model_3, prior_Model_3, fname_Model3_subj);
end

lap_Model3 = cell(697,1);
for n=1:697
    lap_Model3{n} = fullfile('lap_subjects_Model3',['lap_Model3_' num2str(n) '.mat']);
end

fname_BF = 'lap_Model3.mat';
cbm_lap_aggregate(lap_Model3,fname_BF);

%% Phase 1 Only fit

parfor i = 1:697
% 1st input: data
data_subj = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_Phase1_subj = fullfile('lap_subjects_Phase1',['lap_Phase1_', num2str(i), '.mat']);
cbm_lap(data_subj, @Model_Phase1Only, prior_Phase1, fname_Phase1_subj);
end

lap_Phase1 = cell(697,1);
for n=1:697
    lap_Phase1{n} = fullfile('lap_subjects_Phase1',['lap_Phase1_' num2str(n) '.mat']);
end

fname_BF = 'lap_Phase1.mat';
cbm_lap_aggregate(lap_Phase1,fname_BF);

%% Phase 1 Only fit for partner

mkdir('lap_partner_Phase1');
v = 7.5;
prior_Phase1_partner = struct('mean', zeros(2,1), 'variance', v);

parfor i = 1:697
% 1st input: data
data_part = sb(i);
% 2nd input: function handle of model (i.e. @model_mf)
% 3rd input: a prior struct.
% 4th input: output file
fname_Phase1_partner = fullfile('lap_Phase1_partner',['lap_Phase1_partner_', num2str(i), '.mat']);
cbm_lap(data_part, @Model_Phase1PartnerOnly, prior_Phase1_partner, fname_Phase1_partner);
end

lap_Phase1_partner = cell(697,1);
for n=1:697
    lap_Phase1_partner{n} = fullfile('lap_Phase1_partner',['lap_Phase1_partner_' num2str(n) '.mat']);
end

fname_BF = 'lap_Phase1_partner.mat';
cbm_lap_aggregate(lap_Phase1_partner,fname_BF);


%% TEST HBM WORKS AND SAVES (using simple models)

models = {@Model11, @Model12};

fcbm_maps = {'lap_Model11.mat','lap_Model12.mat'};

BFS_hbi = 'hbi_TEST.mat';

cbm_hbi(sb,models,fcbm_maps,BFS_hbi);

%% HBM CBM for bayes run step 1

models = {@Model_1, @Model_2, @Model_3, @Model_4, @Model_5, @Model_6, @Model_7, @Model_8, @Model_9, @Model_10, @Model_11, @Model_12, @Model_13, @Model_14};

fcbm_maps = {'lap_Model1.mat', 'lap_Model2.mat', 'lap_Model3.mat', 'lap_Model4.mat', 'lap_Model5.mat', 'lap_Model6.mat', 'lap_Model7.mat', 'lap_Model8.mat', 'lap_Model9.mat', 'lap_Model10.mat', 'lap_Model11.mat', 'lap_Model12.mat', 'lap_Model13.mat', 'lap_Model14.mat', };

BFS_hbi = 'hbi_models1.mat';

cbm_hbi(sb,models,fcbm_maps,BFS_hbi);

%% HBM CBM for bayes run step 2

models = {@Model_2, @Model_1, @Model_8, @Model_3};

fcbm_maps = {'lap_Model2.mat','lap_Model1.mat','lap_Model8.mat','lap_Model3.mat'};
    
BFS_hbi = 'hbi_models2.mat';

cbm_hbi(sb,models,fcbm_maps,BFS_hbi);

%% HBM CBM for bayes run step 3

models = {@Model_2, @Model_1,@Model_3};

fcbm_maps = {'lap_Model2.mat','lap_Model1.mat','lap_Model3.mat'};
    
BFS_hbi = 'hbi_models3.mat';

cbm_hbi(sb,models,fcbm_maps,BFS_hbi);

%% null HBI 
% Note: takes a long time.

fname_BFS_RW_hbi = 'hbi_models3.mat';

cbm_hbi_null(sb,fname_BFS_RW_hbi);

%% model error check

%% Simulate the model with real parameters

% ensure the model is set to output probabilities of the model
% as well as likelihood for each phase, simulated action, and reward
% contingencies

cbmReal      = load('hbi_models3.mat');
optim_parms3mods = cbmReal.cbm.output.parameters;

lik1 = zeros(697,1);
lik2 = lik1;
F    = lik1;
simA = cell(697,1);
simAFix = cell(697,1);
prob1= cell(697,1);
prob2= cell(697,1);
rew  = cell(697,1);
action=cell(697,1);
u1    =cell(697,1);
u2    =cell(697,1);
cong  =cell(697,1);
alpha_m=cell(697,1);
alpha_m2=cell(697,1);
beta_m=cell(697,1);
beta_m2=cell(697,1);
pptprob1 = cell(697,1);


for i=1:697
   
    [lik1(i), lik2(i), F(i), simA{i}, simAFix{i}, prob1{i}, prob2{i}, pptprob1{i}, rew{i}, u1{i}, u2{i}, cong{i}, alpha_m{i}, beta_m{i}, alpha_m2{i}, beta_m2{i}] = Model_2_Generative(optim_parms3mods{1}(i, :), sb{i});
    action{i} = sb{i}(:,7);
    
end

save modelError_Generative.mat lik1 lik2 F simA simAFix prob1 prob2 pptprob1 rew u1 u2 cong action alpha_m beta_m alpha_m2 beta_m2

%% Recovery analysis

% run laplace approximation on the generated action values.
% create a directory for individual output files:

mkdir('lap_subjects_Model2_recovery_check1');
v = 7.5; 
prior_BFS4_Rec = struct('mean', zeros(4,1), 'variance', v);

simAtouse = load('modelError_Generative.mat');

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
fname_Model2_subj = fullfile('lap_subjects_Model2_recovery_check1',['lap_Model2_rec_', num2str(i), '.mat']);
cbm_lap(data_subj, @Model_2, prior_BFS4_Rec, fname_Model2_subj);
end

lap_Model2_rec1 = cell(697,1);
for n=1:length(lap_Model2_rec1)
    lap_Model2_rec1{n} = fullfile('lap_subjects_Model2_recovery_check1',['lap_Model2_rec_' num2str(n) '.mat']);
end

fname_BF = 'lap_Model2_recovery1.mat';
cbm_lap_aggregate(lap_Model2_rec1,fname_BF);

%% Permutation test 

% ensure the model is set to output probabilities of the model
% as well as likelihood for each phase, simulated action, and reward
% contingencies

cbmReal      = load('hbi_models3.mat');
optim_parms3mods = cbmReal.cbm.output.parameters;

synthPPT = 650;

lik1 = zeros(synthPPT,1);
lik2 = lik1;
F    = lik1;
simA = cell(synthPPT,1);
simAFix = cell(synthPPT,1);
prob1= cell(synthPPT,1);
prob2= cell(synthPPT,1);
rew  = cell(synthPPT,1);
action=cell(synthPPT,1);
u1    =cell(synthPPT,1);
u2    =cell(synthPPT,1);
cong  =cell(synthPPT,1);
alpha_m=cell(synthPPT,1);
alpha_m2=cell(synthPPT,1);
beta_m=cell(synthPPT,1);
beta_m2=cell(synthPPT,1);
pptprob1 = cell(synthPPT,1);
parms  = zeros(synthPPT, 4);

for i=1:synthPPT
    parms(i,:) = [optim_parms3mods{1}(i, 1:2) optim_parms3mods{1}(i+1, 3) optim_parms3mods{1}(i+2, 4)];
    [lik1(i), lik2(i), F(i), simA{i}, simAFix{i}, prob1{i}, prob2{i}, pptprob1{i}, rew{i}, u1{i}, u2{i}, cong{i}, alpha_m{i}, beta_m{i}, alpha_m2{i}, beta_m2{i}] = Model_2_Generative([optim_parms3mods{1}(i, 1:2) optim_parms3mods{1}(i+1, 3) optim_parms3mods{1}(i+2, 4)], sb{i});
    action{i} = sb{i}(:,7);
end

save modelerrorPermute.mat lik1 lik2 F simA simAFix prob1 prob2 pptprob1 rew u1 u2 cong action alpha_m beta_m alpha_m2 beta_m2 parms

%% Generative exploration of predicted and actual scores

%1 = [3.94  -6.87 1 1]; %Prosocial/tight variance
%2 = [3.94  -6.87 6 6]; %Prosocial/loose variance
%3 = [13.07 -0.57 1 1]; %Indiv/tight variance
%4 = [13.07 -0.57 6 6]; %Indiv/loose variance
%5 = [0.99   6.74 1 1]; %Compet/tight variance
%6 = [0.99   6.74 6 6]; %Compet/loose variance

ppts = 250;

for k=1:9
    
lik1 = zeros(ppts,1);
lik2 = lik1;
F    = lik1;
simA = cell(ppts,1);
simAFix = cell(ppts,1);
prob1= cell(ppts,1);
prob2= cell(ppts,1);
probF= cell(ppts,1);
rew  = cell(ppts,1);
u1    =cell(ppts,1);
u2    =cell(ppts,1);
cong  =cell(ppts,1);
alpha_m=cell(ppts,1);
alpha_m2=cell(ppts,1);
beta_m=cell(ppts,1);
beta_m2=cell(ppts,1);
pptprob1 = cell(ppts,1);

sdP = 0.5;
sdV = 0.2;
    
for i=1:ppts    
    
    GenParms = zeros(ppts, 4);
    if          k == 1
    GenParms(i,:) = [normrnd(3.94,  sdP) ,normrnd(-6.87,sdP), normrnd(1,sdV), normrnd(1,sdV)];
         elseif k == 2
    GenParms(i,:) = [normrnd(3.94,  sdP) ,normrnd(-6.87,sdP), normrnd(6,sdV), normrnd(6,sdV)];
         elseif k == 3
    GenParms(i,:) = [normrnd(13.07, sdP) ,normrnd(-0.57,sdP), normrnd(1,sdV), normrnd(1,sdV)];
         elseif k == 4
    GenParms(i,:) = [normrnd(13.07, sdP) ,normrnd(-0.57,sdP), normrnd(6,sdV), normrnd(6,sdV)];
         elseif k == 5
    GenParms(i,:) = [normrnd(0.99,  sdP) ,normrnd(6.74, sdP), normrnd(1,sdV), normrnd(1,sdV)];
         elseif k == 6
    GenParms(i,:) = [normrnd(0.99,  sdP) ,normrnd(6.74, sdP), normrnd(6,sdV), normrnd(6,sdV)];
         elseif k == 7
    GenParms(i,:) = [normrnd(3.94,  sdP) ,normrnd(-6.87,sdP), normrnd(2.5,sdV), normrnd(2.5,sdV)];
         elseif k == 8
    GenParms(i,:) = [normrnd(13.07, sdP) ,normrnd(-0.57,sdP), normrnd(2.5,sdV), normrnd(2.5,sdV)];
         elseif k == 9
    GenParms(i,:) = [normrnd(0.99,  sdP) ,normrnd(6.74, sdP), normrnd(2.5,sdV), normrnd(2.5,sdV)];
    else
    warning('error rand parm')
    end 
    
    [lik1(i), lik2(i), F(i), simA{i}, simAFix{i}, prob1{i}, prob2{i}, probF{i}, pptprob1{i}, rew{i}, u1{i}, u2{i}, cong{i}, alpha_m{i}, beta_m{i}, alpha_m2{i}, beta_m2{i}] = Model_2_Generative(GenParms(i,:), sb{i});
    
end
    if         k==1
    save GenParmsPTV.mat lik1 lik2 F simA simAFix prob1 prob2 pptprob1 rew u1 u2 cong alpha_m beta_m alpha_m2 beta_m2
        elseif k==2
    save GenParmsPLV.mat lik1 lik2 F simA simAFix prob1 prob2 pptprob1 rew u1 u2 cong alpha_m beta_m alpha_m2 beta_m2    
        elseif k==3
    save GenParmsITV.mat lik1 lik2 F simA simAFix prob1 prob2 pptprob1 rew u1 u2 cong alpha_m beta_m alpha_m2 beta_m2
        elseif k==4
    save GenParmsILV.mat lik1 lik2 F simA simAFix prob1 prob2 pptprob1 rew u1 u2 cong alpha_m beta_m alpha_m2 beta_m2
        elseif k==5
    save GenParmsCTV.mat lik1 lik2 F simA simAFix prob1 prob2 pptprob1 rew u1 u2 cong alpha_m beta_m alpha_m2 beta_m2
        elseif k==6
    save GenParmsCLV.mat lik1 lik2 F simA simAFix prob1 prob2 pptprob1 rew u1 u2 cong alpha_m beta_m alpha_m2 beta_m2
        elseif k==7
    save GenParmsPMV.mat lik1 lik2 F simA simAFix prob1 prob2 pptprob1 rew u1 u2 cong alpha_m beta_m alpha_m2 beta_m2
        elseif k==8
    save GenParmsIMV.mat lik1 lik2 F simA simAFix prob1 prob2 pptprob1 rew u1 u2 cong alpha_m beta_m alpha_m2 beta_m2
        elseif k==9
    save GenParmsCMV.mat lik1 lik2 F simA simAFix prob1 prob2 pptprob1 rew u1 u2 cong alpha_m beta_m alpha_m2 beta_m2
    else
    warning('error save')
    end
end

%% Archetypal SVO types

res = 15; %200
alphares1 = 0.1; %1
betares1  = alphares1*2;
va = 2; vb = va;

policy = 3; %k
alphares = 0:alphares1:res; %j
betares = -res:betares1:res; %x

sumProbF = zeros(length(alphares), length(betares), policy);

for k = 1:3
        for x = 1:length(alphares)
            for y = 1:length(betares)
                
 [lik1, lik2, F, simA, simAFix, prob1, prob2, probF, pptprob1, rew, u1, u2, cong, alpha_m, beta_m, alpha_m2, beta_m2] = Model_2_Generative([alphares(x), betares(y), va, vb], sb{k});

 sumProbF(x, y, k) = sum(probF);
 
            end
        end
end

save SumProbF3.mat sumProbF

