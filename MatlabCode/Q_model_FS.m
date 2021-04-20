% Barnby & Dayan (2021) Inequality Aversion and Paranoia
%
% Joe Barnby j.barnby@uq.edu.au 2021

% basic model free q learning model
% Q values represent partner policies
% Q1 = prosocial, Q2 = individualistic, Q3 = competitive
% estimates learnign rate, tau, memory, zeta (lapse)
% While participants make binary choices (1,2), this model instead
% accepts whether that choice was prosocial (1), individualistic (2), or
% competitive (3).

%% Model

function [F] = Q_model_FS(parms,data)

%if (nargin < 4)
%    transform = 1;
%end
%if (nargin < 3)
%    detail = 0;
%end

%if(transform == 1)

if(length(parms) < 5)
parms(5) = 10;
nd_mem   = parms(5);
end
if(length(parms) < 6)
parms(6) = -10;
nd_zet   = parms(6);
end

nd_alpha    = parms(1);
nd_tau      = parms(3);
nd_lr       = parms(4);

%if(transform == 1)
alpha   = 10*(1./(1+exp(-nd_alpha)));
beta    = parms(2);
tau     = exp(nd_tau);
lr      = 1./(1+exp(-nd_lr));
mem     = 1./(1+exp(-nd_mem));
zet     = 1./(1+exp(-nd_zet));

%else
%alpha = parms(1);
%beta = parms(2);
%tau = parms(3);
%lr  = parms(4);
%zet = parms(5); 
%mem = parms(6);
%end

T1      = 18;
T2      = T1 + 36 + 1;   %number of trials + 1
k       = 3;             %number of options
Q       = nan(T2,k);     %values of each choice each trial %1 = prosocial, 2 = individual, 3 = competitive
Q(T1,:) = 0.33; 
%simA= T2;

lik2   = 0;
lik1   = 0;

% Phase 1 participants choices

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

% Phase 2 participants heuristic guesses

for t = (T1+1):T2 
    
    choice = data(t-1,9);
    outcome= data(t-1,10);

    % copy forward action values to next trial
    Q(t, :) = Q(t-1, :);
    
    pe      = outcome - Q(t,choice);
    
    % update option chosen on this trial for next trial's choice
    Q(t,choice) = Q(t,choice) + (lr * pe);  
    % decay non chosen options
    Q(t,setdiff(1:end, choice))= 0.33 - (mem * (0.33 - Q(t-1, setdiff(1:end, choice)))); 
    
    pr      = (exp(Q(t-1, choice)/tau)/sum(exp(Q(t-1,:)/tau)));
    pr1     = (zet/3) + (1-zet) * pr;
    lik2    = lik2 + log(pr1);
    
    %if (detail == 1)
    %    
    %prsim      = (exp(Q(t-1,:)/tau)/sum(exp(Q(t-1,:)/tau)));
    %pr1sim     = (zet/3) + ((1-zet) * prsim);
    %simA(t-1)   = randsample(3,1,true,pr1sim);

end
    
%return vector of action values for each trial
%Qs= Q;

%simA = simA;

F = lik1 + lik2 + eps;

end

