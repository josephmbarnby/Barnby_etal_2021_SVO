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

function[F] = FSModel_1_Phase2(parms, data)

   % Initialise
   
      res = 15; % resolution of grid and alpha

   %phase 1 parms
alpha_raw       = parms(1); % subjects alpha for phase 1
alpha           = res*(1./(1+exp(-alpha_raw)));% restrict alpha to between 0 and 15
beta            = parms(2); % subjects beta for phase 1
    %phase 2 parms
alpha_v         = parms(3); % subjects prior variance of belief over their partner for alpha
beta_v          = parms(4); % subjects prior variance of belief over their partner for beta

param_alpha_v   = exp(alpha_v); % restrict the variance to above 0
param_beta_v    = exp(beta_v);  % restrict the variance to above 0

    % grid for a subjects beliefs over their partner in phase 2
    
 %generate standardised grid to form priors
[alpha_2,beta_2]=meshgrid(0:.125:res,-res:.25:res);
newpabg=normpdf(alpha_2,alpha,param_alpha_v).*normpdf(beta_2,beta,param_beta_v);
newpabg=newpabg/sum(newpabg(:)); 

    % initialised dummy values
    
lik1 = 0;   % likelihood for choices in phase 1
lik2 = 0;   % likelihood for guesses in phase 2
T1   = 18;  % trials for phase 1
T2   = 54;  % trials for phase 2

    % Phase 1 choices of the participant
    
for t=1:T1
    
s1 = data(t, 3)/10;
o1 = data(t, 4)/10;
s2 = data(t, 5)/10;
o2 = data(t, 6)/10;

actual_choice = data(t, 7);

val1 = alpha*s1 + beta*max(s1-o1,0) ; 
val2 = alpha*s2 + beta*max(s2-o2,0) ;

pchoose1=(1./(1+exp(-(val1 - val2)))); % probability of 1
    
     if (actual_choice==1)   
        lik1 = lik1 + log(pchoose1);
     else
        lik1 = lik1 + log(1-pchoose1);
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

val1 = alpha_2*s1 + beta_2*max(s1-o1,0) ; 
val2 = alpha_2*s2 + beta_2*max(s2-o2,0) ;

subject_estimate_pchoose1 = (1./(1+exp(-(val1 - val2))));
tmp=subject_estimate_pchoose1 .* newpabg;
subject_netp1 = sum(tmp(:));

subject_estimate = data(t, 7); % say the subject thought that the partner would go for 2

    if (subject_estimate==1)
        lik2 = lik2 + log(subject_netp1); % log likelihood 
    else
        lik2 = lik2 + log(1-subject_netp1);
    end

actual_choice = data(t, 8); % what did our partner 'choose'

    if (actual_choice==1)
        pchoose2=(1./(1+exp(-(val1 - val2)))); % probability of 1
    else
        pchoose2=(1./(1+exp(-(val2 - val1)))); % probability of 2
    end

newpabg = pchoose2.*newpabg; % Bayes rule
newpabg = newpabg ./ sum(newpabg(:)); %normalised distribution

end

F  = lik1 + lik2 + eps;

end