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
function[F] = FSModel_1_Phase2_shrink(parms, data)

   % Initialise

   %phase 1 parms
alpha_raw       = parms(1); % subjects alpha for phase 1
alpha           = 10*(1./(1+exp(-alpha_raw))); % restrict alpha to between 0 and 10
beta            = parms(2); % subjects beta for phase 1
    %phase 2 parms
alpha_v         = parms(3); % subjects prior variance of belief over their partner for alpha
beta_v          = parms(4); % subjects prior variance of belief over their partner for beta
%param_alpha_v   = 7.5*(1./(1+exp(-alpha_v))); % restrict the variance to above 0
%param_beta_v    = 7.5*(1./(1+exp(-beta_v)));  % restrict the variance to above 0
param_alpha_v   = exp(alpha_v); % restrict the variance to above 0
param_beta_v    = exp(beta_v);  % restrict the variance to above 0

shrink_raw      = parms(5);
shrink          = 1./(1+exp(-shrink_raw));

%if (length(parms) < 6) % over/under matching of the subject for the partner
%    zeta = 1;
%else
%    zeta_m = parms(6);
%    zeta   = (1./(1+exp(-zeta_m)));
%end

    % grid for a subjects beliefs over their partner in phase 2
    
alpha_shrink = shrink * alpha;
beta_shrink  = shrink * beta;
    
 %generate standardised grid to form priors
[alpha_2,beta_2]=meshgrid(0:.125:10,-10:.25:10);
newpabg=normpdf(alpha_2,alpha_shrink,param_alpha_v).*normpdf(beta_2,beta_shrink,param_beta_v);
newpabg=newpabg/sum(newpabg(:)); 

    % initialised dummy values
    
lik1 = 0;   % likelihood for choices in phase 1
lik2 = 0;   % likelihood for guesses in phase 2
F    = 0;   % overall log likelihood
T1   = 18;  % trials for phase 1
T2   = 54;  % trials for phase 2

%simA = zeros(54, 1);

    % Phase 1 choices of the participant
    
for t=1:T1
    
s1 = data(t, 3)/10;
o1 = data(t, 4)/10;
s2 = data(t, 5)/10;
o2 = data(t, 6)/10;

actual_choice = data(t, 7);

val1 = alpha*s1 + beta*max(s1-o1,0) ; 
val2 = alpha*s2 + beta*max(s2-o2,0) ;

if (actual_choice==1)
    pchoose1=(1./(1+exp(val1 - val2))); % probability of 1
else
    pchoose1=(1./(1+exp(val2 - val1))); % probability of 2
end

    %if (actual_choice==1) %simulated answer from participant given probability
    %    simA(t) = randsample(2,1,true,[pchoose1, 1-pchoose1]);
    %else
    %    simA(t) = randsample(2,1,true,[1-pchoose1, pchoose1]);
    %end

lik1 = lik1 + log(pchoose1);

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

subject_estimate_pchoose1 = (1./(1+exp(val1 - val2)));
tmp=subject_estimate_pchoose1 .* newpabg;
subject_netp1 = sum(tmp(:));
%subject_netp2 = 1-sum(tmp(:));

% adjust for over/under matching
subject_adjp1 = subject_netp1; %^zeta/(subject_netp1^zeta + subject_netp2^zeta);
subject_estimate = data(t, 7); % say the subject thought that the partner would go for 2

if (subject_estimate==1)
    lik2 = lik2 + log(subject_adjp1); % log likelihood 
else
    lik2 = lik2 + log(1-subject_adjp1);
end

    %if (subject_estimate==1)
    %    simA(t) = randsample(2,1,true,[subject_adjp1(:), 1-subject_adjp1(:)]);
    %else
    %    simA(t) = randsample(2,1,true,[1-subject_adjp1(:), subject_adjp1(:)]);
    %end

actual_choice = data(t, 8); % what did our partner 'choose'

if (actual_choice==1)
    pchoose2=(1./(1+exp(val1 - val2))); % probability of 1
else
    pchoose2=(1./(1+exp(val2 - val1))); % probability of 2
end

newpabg = pchoose2.*newpabg; % Bayes rule
newpabg = newpabg ./ sum(newpabg(:)); %normalised distribution

end

%alpha_marginal2 = squeeze(sum(newpabg,[1 3])); % work out the marginals over the components
%beta_marginal2  = squeeze(sum(newpabg,[2 3]))';
F  = sum(lik1) + sum(lik2) + eps;

end