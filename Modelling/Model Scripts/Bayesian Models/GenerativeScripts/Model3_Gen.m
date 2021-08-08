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

function[lik1, lik2, F, simA, simAFix, prob1, prob2, pptprob1, rew, u1, u2] = Model3_Gen(parms, data)

   % Initialise
   
   %phase 1 parms
alpha_raw       = parms(1); % subjects alpha for phase 1
alpha           = 15*(1./(1+exp(-alpha_raw))); % restrict alpha to between 0 and 15
beta            = parms(2); % subjects beta for phase 1

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

val1ppt = alpha*s1 + beta*max(s1-o1,0) ; 
val2ppt = alpha*s2 + beta*max(s2-o2,0) ;

subject_netp1 = (1./(1+exp(-(val1ppt - val2ppt))));

subject_estimate = data(t, 7); % subject choice

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

end


F  = lik1 + lik2 + eps;

end