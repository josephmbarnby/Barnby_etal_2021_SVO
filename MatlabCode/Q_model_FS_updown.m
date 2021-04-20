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

function [F] = Q_model_FS_updown(parms,data)

%if (nargin < 4)
%    transform = 1;
%end
%if (nargin < 3)
%    detail = 0;
%end

if(length(parms) < 6)
parms(6) = 10;
nd_mem   = parms(6);
end
if(length(parms) < 7)
parms(7) = -10;
nd_zet   = parms(7);
end

nd_alpha        = parms(1);
nd_tau          = parms(3);
nd_lr_u         = parms(4);
nd_lr_d         = parms(5);

%if(transform == 1)
alpha   = 10*(1./(1+exp(-nd_alpha)));
beta    = parms(2);
tau     = exp(nd_tau);
lr_u    = 1./(1+exp(-nd_lr_u));
lr_d    = 1./(1+exp(-nd_lr_d));
mem     = 1./(1+exp(-nd_mem));
zet     = 1./(1+exp(-nd_zet));

%else
%alpha   = parms(1);
%beta    = parms(2);
%tau     = parms(3);
%lr_u    = parms(4);
%lr_d    = parms(5);
%zet     = parms(6); 
%mem     = parms(7);
%end

T1  = 18;
T2  = T1 + 36 + 1;   %number of trials + 1
k   = 3;             %number of options
Q   = nan(T2,k);     %values of each choice each trial %1 = prosocial, 2 = individual, 3 = competitive
%simA= T2;

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
    if (pe > 0)
    Q(t,choice) = Q(t,choice) + (lr_u * pe);   
    else
    Q(t,choice) = Q(t,choice) + (lr_d * pe);   
    end
    % decay non chosen options
    
    Q(t,setdiff(1:end, choice))= 0.33 - (mem * (0.33 - Q(t-1, setdiff(1:end, choice)))); 
    
    pr      = (exp(Q(t-1, choice)/tau)/sum(exp(Q(t-1,:)/tau)));
    pr1     = (zet/3) + ((1-zet) * pr);
    lik2    = lik2 + log(pr1);
    
    %if (detail == 1)
    %    
    %prsim      = (exp(Q(t-1,:)/tau)/sum(exp(Q(t-1,:)/tau)));
    %pr1sim     = (zet/3) + ((1-zet) * prsim);
    %simA(t-1)   = randsample(3,1,true,pr1sim);
    %
    %end
    
end

%return vector of action values for each trial
%Qs= Q;
F = lik1 + lik2 + eps;
%simA = simA;

end