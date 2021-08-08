% Barnby & Dayan (2021) Inequality Aversion and Paranoia
%
% Joe Barnby j.barnby@uq.edu.au 2021

% basic model free q learning model
% Q values represent partner policies
% Q1 = prosocial, Q2 = individualistic, Q3 = competitive
% estimates learnign rate for each Q, tau, memory, zeta (lapse)
% While participants make binary choices (1,2), this model instead
% accepts whether that choice was prosocial (1), individualistic (2), or
% competitive (3).

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
% 9 PPT Choice            | 1=Prosocial, 2=Individualistic, 3=Competitive
% 10Outcome               | 1=Correct,0=Incorrect

%% Model

function [F] = Model_13(parms,data)

nd_alpha        = parms(1);
nd_tau          = parms(3);
nd_lr_p         = parms(4);
nd_lr_i         = parms(5);
nd_lr_c         = parms(6);

alpha   = 15*(1./(1+exp(-nd_alpha)));
beta    = parms(2);
tau     = exp(nd_tau);
lr_p    = 1./(1+exp(-nd_lr_p));
lr_i    = 1./(1+exp(-nd_lr_i));
lr_c    = 1./(1+exp(-nd_lr_c));

T1  = 18;
T2  = T1 + 36;       %number of trials + 1
k   = 3;             %number of options
Q   = nan(T2+1,k);     %values of each choice each trial %1 = prosocial, 2 = individual, 3 = competitive

lik2   = 0;
lik1   = 0;

Q(T1,:) = 0.33; %initialize guesses

% Phase 1 participants choices

for t=1:T1
    
s1 = data(t, 3)/10;
o1 = data(t, 4)/10;
s2 = data(t, 5)/10;
o2 = data(t, 6)/10;

actual_choice = data(t, 7);

val1 = (alpha*s1) + (beta*max(s1-o1,0)) ; 
val2 = (alpha*s2) + (beta*max(s2-o2,0)) ;

pchoose1=(1./(1+exp(-(val1 - val2)))); % probability of 1

    if (actual_choice==1)
        lik1 = lik1 + log(pchoose1); % log likelihood of 1
    else
        lik1 = lik1 + log(1-pchoose1);
    end

end

% Phase 2 participants heuristic guesses

for t = (T1+1):T2 
    
    choice = data(t,9);
    outcome= data(t,10);

    % copy forward action values to next trial
    Q(t, :) = Q(t-1, :);
    
    pe      = outcome - Q(t,choice);
    
    % update option chosen on this trial for next trial's choice
    if (choice == 1)
    Q(t,choice) = Q(t,choice) + (lr_p * pe);  
    elseif (choice == 2)
    Q(t,choice) = Q(t,choice) + (lr_i * pe);  
    else
    Q(t,choice) = Q(t,choice) + (lr_c * pe);   
    end
    
    pr      = (exp(Q(t-1, choice)/tau)/sum(exp(Q(t-1,:)/tau)));
    lik2    = lik2 + log(pr);

end

F = lik1 + lik2 + eps;

end