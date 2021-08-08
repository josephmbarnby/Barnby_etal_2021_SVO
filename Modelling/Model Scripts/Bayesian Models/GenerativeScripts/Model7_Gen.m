% Barnby & Dayan (2021) Inequality Aversion and Paranoia
%
% Joe Barnby j.barnby@uq.edu.au 2021

% Full Bayesian model
% Estimates:
%1  alpha and beta of participant over trials 1:18
%2  the variance of the alpha and beta priors a participant holds about
%   their partner
%3  A participant's inferred alpha and beta given their partner's decisions
%   over trials 1:36. 

%% Model

function[lik1, lik2, F, simA, simAFix, prob1, prob2, pptprob1, rew, u1, u2, congCount, alpha_marg, beta_marg, alpha_marg2, beta_marg2] = Model7_Gen(parms, data)

   % Initialise
   
   res = 15;

   %phase 1 parms
alpha_raw       = parms(1); % subjects alpha for phase 1
alpha           = 15*(1./(1+exp(-alpha_raw))); % restrict alpha to between 0 and 15
beta            = parms(2); % subjects beta for phase 1
    %phase 2 parms
alpha_v         = parms(3); % subjects prior variance of belief over their partner for alpha
beta_v          = parms(4); % subjects prior variance of belief over their partner for beta
param_alpha_v   = exp(alpha_v); % restrict the variance to above 0
param_beta_v    = exp(beta_v);  % restrict the variance to above 0

%learning rule adjustments
raw_epsc        = parms(5);
epsilon         = 1./(1+exp(-raw_epsc));

    % grid for a subjects beliefs over their partner in phase 2
    
 %generate standardised grid to form priors
[alpha_2,beta_2]=meshgrid(0:.125:res,-res:.25:res);
newpabg=normpdf(alpha_2,alpha,param_alpha_v).*normpdf(beta_2,beta,param_beta_v);
newpabg=newpabg ./ sum(newpabg(:)); 

alpha_marg = squeeze(sum(newpabg,[1 3]));
beta_marg = squeeze(sum(newpabg,[2 3]))';

    % initialised dummy values
    
lik1 = 0;   % likelihood for choices in phase 1
prob1= zeros(18, 1);
lik2 = 0;   % likelihood for guesses in phase 2
prob2= zeros(36, 1);
T1   = 18;  % trials for phase 1
T2   = 54;  % trials for phase 2
simA = zeros(54,1);
simAFix = zeros(54, 1);
u1   = zeros(18,1);
u2   = zeros(18,1);
rew  = zeros(54,4);
congCount = zeros(36,1);
pptprob1 = zeros(36,1);

    % Phase 1 choices of the participant
    
for t=1:T1
    
s1 = data(t, 3)/10;
o1 = data(t, 4)/10;
s2 = data(t, 5)/10;
o2 = data(t, 6)/10;

rew(t,1) = data(t, 3)/10;
rew(t,2) = data(t, 4)/10;
rew(t,3) = data(t, 5)/10;
rew(t,4) = data(t, 6)/10;

actual_choice = data(t, 7);

val1c = alpha*s1 + beta*max(s1-o1,0) ; 
val2c = alpha*s2 + beta*max(s2-o2,0) ;
u1(t)= val1c;
u2(t)= val2c;

pchoose1=(1./(1+exp(-(val1c - val2c)))); % probability of 1
simA(t) = randsample([1,2],1,true,[pchoose1, 1-pchoose1]);

    if (actual_choice==1)
        lik1 = lik1 + log(pchoose1); % log likelihood of 1
        prob1(t) = pchoose1;
    else
        lik1 = lik1 + log(1-pchoose1);
        prob1(t) = 1-pchoose1;
    end

end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% how the experimenter learns how the subject learns online about the partner in phase 2

     % Phase2

for t=(T1+1):T2
    
s1 = data(t, 3)/10;
o1 = data(t, 4)/10;
s2 = data(t, 5)/10;
o2 = data(t, 6)/10;

rew(t,1) = data(t, 3)/10;
rew(t,2) = data(t, 4)/10;
rew(t,3) = data(t, 5)/10;
rew(t,4) = data(t, 6)/10;

val1 = alpha_2*s1 + beta_2*max(s1-o1,0) ; 
val2 = alpha_2*s2 + beta_2*max(s2-o2,0) ;
%val1 = (alpha_2*(s1 * (s1-o1))) + beta_2*max(s1-o1,0) ; %to check indiv
%val2 = (alpha_2*(s2 * (s2-o2))) + beta_2*max(s2-o2,0) ; %to check indiv
val1ppt = alpha*s1 + beta*max(s1-o1,0) ; 
val2ppt = alpha*s2 + beta*max(s2-o2,0) ;

subject_estimate = data(t, 7); % subject choice

    if     subject_estimate == 1 && val1ppt > val2ppt
        congCount(t-18) = 1;
    elseif subject_estimate == 2 && val2ppt > val1ppt
        congCount(t-18) = 1;
    else 
        congCount(t-18) = 0;
    end

subject_estimate_pchoose1 = (1-epsilon)  * (1./(1+exp(-(val1 - val2)))) + (epsilon/2);
tmp=subject_estimate_pchoose1 .* newpabg;
subject_netp1 = sum(tmp(:));
pptprob1(t-18) = subject_netp1;

%for fixed parms
subject_estimate_pchoose1Fix = (1./(1+exp(-(val1ppt - val2ppt))));

    if (subject_estimate == 1)
        lik2 = lik2 + log(subject_netp1); % log likelihood
        prob2(t-18) = subject_netp1;
    else
        lik2 = lik2 + log(1-subject_netp1);
        prob2(t-18) = 1-subject_netp1;
    end

simA(t)    = randsample([1,2],1,true,[subject_netp1, 1-subject_netp1]);
simAFix(t) = randsample([1,2],1,true,[subject_estimate_pchoose1Fix, 1-subject_estimate_pchoose1Fix]);

actual_choice = data(t, 8); % what did our partner 'choose'

    if (actual_choice==1)
        pchoose2 = (1./(1+exp(-(val1 - val2)))); % if the partner chose 1
    else
        pchoose2 = (1./(1+exp(-(val2 - val1)))); % if the partner chose 2
    end

newpabg = pchoose2.*newpabg; % Bayes rule
newpabg = newpabg ./ sum(newpabg(:)); %normalised distribution

end

alpha_marg2 = squeeze(sum(newpabg,[1 3]));
beta_marg2 = squeeze(sum(newpabg,[2 3]))';

F  = lik1 + lik2 + eps;

end