% Barnby & Dayan (2021) Inequality Aversion and Paranoia
%
% Joe Barnby j.barnby@uq.edu.au 2021

%% -----------------------------------------------------------------------
%% PREPARATION

% fresh memory
%clear;

% Paths [change 'repoBase' according to local setup]
%fs = filesep;
%repoBase = [fs,'Volumes', fs, 'GoogleDrive', fs, 'My Drive', fs, 'Dropbox', fs, 'UoQ_BI', fs, 'IntentionsGameModel'];
%data = [repoBase, fs, 'Intentions_Phase1.csv']
%% -----------------------------------------------------------------------
%% Model

% how the experimenter learns online about the subject in phase 1
% and how the subject learns online about the partner in phase 2
function[F, prob, alpha_marginal, beta_marginal] = FSModel_bayes(data)

%-------------------------

%-------------------------

   % Initialise
[alpha,beta]=meshgrid(0:.125:5,-5:.25:5);
pabg=unifpdf(alpha).*unifpdf(beta);
pabg=pabg/sum(pabg(:)); % probability of beta,alpha,gamma
     
lik = 0;
prob = 0;
T = length(data(1));

for t=1:T
    
s1 = data(t, 3)/10;
o1 = data(t, 4)/10;
s2 = data(t, 5)/10;
o2 = data(t, 6)/10;
actual_choice = data(t, 2);

sig=@(x)(1./(1+exp(-x)));

val1 = alpha*s1 + beta*max(s1-o1,0) ; 
val2 = alpha*s2 + beta*max(s2-o2,0) ;

if (actual_choice==1)
    pchoose=sig(val1 - val2); % probability of 1
else
    pchoose=sig(val2 - val1); % probability of 2
end

pabg = pchoose.*pabg; % Bayes rule
lik  = lik + log(sum(pabg(:)));
prob = prob + sum(pabg(:));
pabg = pabg ./ sum(pabg(:)); %so distribution

end

alpha_marginal = squeeze(sum(pabg,[1 3])); % work out the marginals over the components
beta_marginal  = squeeze(sum(pabg,[2 3]))';
F = sum(lik + eps);
prob = sum(prob);

end

%% -----------------------------------------------------------------------

%plot(squeeze(alpha(1,:)),alpha_marginal)
%hold on;
%plot(squeeze(beta(1,:)),beta_marginal,'r')