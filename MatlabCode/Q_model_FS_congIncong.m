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

%% Model

function [F] = Q_model_FS_congIncong(parms,data)

res = 15;

nd_alpha        = parms(1);
nd_tau          = parms(3);
nd_lr_c         = parms(4);
nd_lr_ic        = parms(5);

alpha   = res*(1./(1+exp(-nd_alpha)));
beta    = parms(2);
tau     = exp(nd_tau);
lr_c    = 1./(1+exp(-nd_lr_c));
lr_ic   = 1./(1+exp(-nd_lr_ic));

T1  = 18;
T2  = T1 + 36 + 1;   %number of trials + 1
k   = 3;             %number of options
Q   = nan(T2,k);     %values of each choice each trial %1 = prosocial, 2 = individual, 3 = competitive

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
    
    
    val1 = (alpha*s1) + (beta*max(s1-o1,0)) ; 
    val2 = (alpha*s2) + (beta*max(s2-o2,0)) ;
    
    action = data(t-1,7);
    choice = data(t-1,9);
    outcome= data(t-1,10);

    % copy forward action values to next trial
    Q(t, :) = Q(t-1, :);
    
    pe      = outcome - Q(t,choice);
    
    % update option chosen on this trial for next trial's choice
    if     (action == 1 && val1>val2) 
        Q(t,choice) = Q(t,choice) + (lr_c * pe);   
    elseif (action == 2 && val2>val1)   
        Q(t,choice) = Q(t,choice) + (lr_c * pe);  
    else
        Q(t,choice) = Q(t,choice) + (lr_ic * pe);     
    end

    % decay non chosen options
       
    pr      = (exp(Q(t-1, choice)/tau)/sum(exp(Q(t-1,:)/tau)));
    lik2    = lik2 + log(pr);
    
end

F = lik1 + lik2 + eps;

end