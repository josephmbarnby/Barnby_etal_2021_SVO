% how the experimenter learns online about the subject in phase 1
% and how the subject learns online about the partner in phase 2


[alpha,beta,gamma]=meshgrid(0:.25:10,0:.25:10,0:.25:10);
pabg=gampdf(alpha,3,1).*gampdf(beta,4,1).*gampdf(gamma,5,1);
pabg=pabg/sum(pabg(:)); % probability of beta,alpha,gamma

choiceab=[5 5 ; 9 1]/10; % choices scaled to 0...1

sig=inline('1./(1+exp(-x))');

opt=choiceab(1,:);
val1 = alpha*opt(1) + beta*max(opt(1)-opt(2),0) - gamma*max(opt(2)-opt(1),0); 
opt=choiceab(2,:);
val2 = alpha*opt(1) + beta*max(opt(1)-opt(2),0) - gamma*max(opt(2)-opt(1),0);

actual_choice = 1; % what did our subject 'choose'
if (actual_choice==1)
    pchoose=sig(val1 - val2); % probability of 1
else
    pchoose=sig(val2 - val1); % probability of 2
end

newpabg = pchoose.*pabg; % Bayes rule
newpabg = newpabg ./ sum(newpabg(:)); %so distribution

alpha_marginal = squeeze(sum(newpabg,[1 3])); % work out the marginals over the components
beta_marginal = squeeze(sum(newpabg,[2 3]))';
gamma_marginal = squeeze(sum(newpabg,[1 2]))';

figure
plot(squeeze(alpha(1,:,1)),alpha_marginal)
hold on;
plot(squeeze(alpha(1,:,1)),beta_marginal,'r')
plot(squeeze(alpha(1,:,1)),gamma_marginal,'g')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% how the experimenter learns how the subject learns online about the partner in phase 2

param_alpha_prior=2.5;
param_beta_prior=3.5;
param_gamma_prior=4.5;
param_zeta=3;

[alpha,beta,gamma]=meshgrid(0:.25:10,0:.25:10,0:.25:10);
pabg=gampdf(alpha,param_alpha_prior,1).*gampdf(beta,param_beta_prior,1).*gampdf(gamma,param_gamma_prior,1);
pabg=pabg/sum(pabg(:)); % probability of beta,alpha,gamma

choiceab=[5 5 ; 9 1]/10; % choices scaled to 0...1

sig=inline('1./(1+exp(-x))');

opt=choiceab(1,:);
val1 = alpha*opt(1) + beta*max(opt(1)-opt(2),0) - gamma*max(opt(2)-opt(1),0); 
opt=choiceab(2,:);
val2 = alpha*opt(1) + beta*max(opt(1)-opt(2),0) - gamma*max(opt(2)-opt(1),0);

subject_estimate_phoose1 = sig(val1 - val2);
tmp=subject_estimate_phoose1 .* pabg;
subject_netp1 = sum(tmp(:));
subject_netp2 = 1-subject_netp1;

% adjust for over/under matching
subject_adjp1 = subject_netp1^param_zeta/(subject_netp1^param_zeta + subject_netp2^param_zeta);
subject_report = 2; % say the subject thought that the partner would go for 2
if (subject_report==1)
    ll = log(subject_adjp1); % log likelihood 
else
    ll = log(1-subject_adjp1);
end

actual_choice = 1; % what did our subject 'choose'
if (actual_choice==1)
    pchoose=sig(val1 - val2); % probability of 1
else
    pchoose=sig(val2 - val1); % probability of 2
end

newpabg = pchoose.*pabg; % Bayes rule
newpabg = newpabg ./ sum(newpabg(:)); %so distribution


