% Barnby & Dayan (2021) Inequality Aversion and Paranoia
%
% Joe Barnby j.barnby@uq.edu.au 2021

% Full Bayesian model
% Estimates:
% alpha and beta of participant over trials 1:36


%% Model

function[F] = Model_Phase1PartnerOnly(parms, data)

res = 30;

   % Initialise

   %phase 1 parms
alpha_raw       = parms(1); % subjects alpha for phase 1
alpha           = (1./(1+exp(-alpha_raw)))*res; % restrict alpha above 0
beta            = parms(2); % subjects beta for phase 1

    % initialised dummy values
    
lik1 = 0;   % likelihood for choices in phase 1

T2   = 36;  % trials for phase 1

    % Phase 1 choices of the participant
    
for t=18:(T2+18)
    
s1 = data(t, 3)/10;
o1 = data(t, 4)/10;
s2 = data(t, 5)/10;
o2 = data(t, 6)/10;

actual_choice = data(t, 8);

val1 = (alpha*s1) + (beta*max(s1-o1,0)) ; 
val2 = (alpha*s2) + (beta*max(s2-o2,0)) ;

pchoose1=(1./(1+exp(-(val1 - val2)))); % probability of 1

    if (actual_choice==1)
        lik1 = lik1 + log(pchoose1); % log likelihood of 1
    else
        lik1 = lik1 + log(1-pchoose1);
    end

end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

F = lik1 + eps;

end