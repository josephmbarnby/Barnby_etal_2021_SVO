% Barnby & Dayan (2021) Inequality Aversion and Paranoia
%
% Joe Barnby j.barnby@uq.edu.au 2021

% Full Bayesian model
% Estimates:
%1  beta of participant over trials 1:18
%2  the variance of the beta priors a participant holds about
%   their partner
%3  A participant's inferred beta given their partner's decisions
%   over trials 1:36. 

%%%%%% DATA STRUCTURE REQUIRED %%%%%%%
% colnames:
% 1 ID                    | 1....n
% 2 Trial                 | 1:18 (Phase1); 19:54 (Phase 2); adjust as appropriate
% 3 O1-Self               | 8, 10, 10...n
% 4 O1-Other              | 8, 6,  5 ...n
% 5 O2-Self               | 6, 9,  8 ...n
% 6 O2-Other              | 2, 9,  4 ...n
% 7 PPT choice/prediction | 2, 2,  1 ...n
% 8 Partner action        | 1, 2,  1 ...n

%% Model

function[F] = Model_1(parms, data)

   % Initialise
   
   res = 15;

   %phase 1 parms
beta            = parms(1); % subjects beta for phase 1
    %phase 2 parms
beta_v          = parms(2); % subjects prior variance of belief over their partner for beta
param_beta_v    = exp(beta_v);  % restrict the variance to above 0

    % grid for a subjects beliefs over their partner in phase 2
    
 %generate standardised grid to form priors
[beta_2]=meshgrid(-res:.25:res);
newpabg=normpdf(beta_2,beta,param_beta_v);
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

val1 = beta*max(s1-o1,0) ; 
val2 = beta*max(s2-o2,0) ;

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

val1 = beta_2*max(s1-o1,0) ; 
val2 = beta_2*max(s2-o2,0) ;

subject_estimate_pchoose1 = (1./(1+exp(-(val1 - val2))));
tmp=subject_estimate_pchoose1 .* newpabg;
subject_netp1 = sum(tmp(:));

subject_estimate = data(t, 7); % participant prediction

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