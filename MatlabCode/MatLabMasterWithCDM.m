%%
% Barnby & Dayan (2021) Inequality Aversion and Paranoia
%
% Joe Barnby j.barnby@uq.edu.au 2021
%
%% MASTER FILE %%

%% Load data
addpath('CBMCode');
D = readtable('Intentions_Phase1.csv');
L      = zeros(12546,7);
D2      = table2array(D(:,contains(D.Properties.VariableNames, {'id', 'Trial', 'Option1_PPT', 'Option1_Partner', 'Option2_PPT', 'Option2_Partner', 'Response'})))
subs = unique(D2(:,7));

% 697 participants, each associated with a 18 x 7 data matrix
n = 697;
 
%--------------------------------
d = size(D2,1)/n;
 
for i = 1:n
    s{i} = D2(1+(i-1)*d:d+(i-1)*d,:);
end

%functions for data transformation
sig=@(x)(1./(1+exp(-x)));
A.s=@(x)(5./(1+exp(-x)));

%% Check models

parms=randn(1,2);
subj = s{1};
F1 = FSModel_1(parms, subj);
parms=randn(1,3);
F2 = FSModel_2(parms, subj);

for i=1:697
F3(i) = FSModel_bayes(s{i}); %#ok<SAGROW>
end

%%% run models in CDM

v = 6.25; 
prior_FS_1 = struct('mean', zeros(2,1), 'variance', v);
prior_FS_2 = struct('mean', zeros(3,1), 'variance', v);

fname_FS1 = 'lap_FS1.mat';
fname_FS2 = 'lap_FS2.mat';

% CBM lab
cbm_lap(s, @FSModel_1, prior_FS_1, fname_FS1);
cbm_lap(s, @FSModel_2, prior_FS_2, fname_FS2);

% HBM CBM

models = {@FSModel_1, @FSModel_2};

fcbm_maps = {'lap_FS1.mat','lap_FS2.mat'};

fname_hbi = 'hbi_FS1_FS2.mat';

cbm_hbi(data,models,fcbm_maps,fname_hbi);

% load output
fname_hbi = load('hbi_FS1_FS2.mat');
cbm = fname_hbi.cbm;
cbm.output

model_frequency      = cbm.output.model_frequency;

group_mean_FS1       = cbm.output.group_mean{1};
group_mean_FS1.alpha = A.s(group_mean_FS1.alpha);
group_mean_FS1.tau   = sig(group_mean_FS1.tau);

group_mean_FS2       = cbm.output.group_mean{2};
group_mean_FS2.alpha = A.s(group_mean_FS2.alpha);
group_mean_FS2.tau   = sig(group_mean_FS2.tau);