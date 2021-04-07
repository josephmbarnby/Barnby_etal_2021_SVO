% Barnby & Dayan (2021) Inequality Aversion and Paranoia
%
% Joe Barnby j.barnby@uq.edu.au 2021

%% Model

function[F] = FSModel_2(params, data)

    %
    % INPUTS:
    %   data = data matrix with [6 x N] columns: choice option outcomes for self and
    %   other each trial (1-4), response of the participant (5), trial
    %   column detnoting the number of trials (6)
    %
    % OUTPUTS:
    %   F = loglikelihood of the choice given the parameters.

%-------------------------
A.s=@(x)(5./(1+exp(-x)));
soft=@(x, y, tau)(1./(1+exp(-tau*(x-y))));
sig=@(x)(1./(1+exp(-x)));

    % parameters
    
raw_alpha   = params(1);
alpha       = A.s(raw_alpha);
beta        = params(2);
raw_tau     = params(3);
tau         = sig(raw_tau);

    % Initialise
    
lik = 0;

%-------------------------

T = length(data(1));

for t=1:T
s1 = data(t, 3)/10;
o1 = data(t, 4)/10;
s2 = data(t, 5)/10;
o2 = data(t, 6)/10;
actual_choice = data(t, 2);
 
    %Value for each option
    val1 = alpha * s1 + beta * max(s1 - o1,0);
    val2 = alpha * s2 + beta * max(s2 - o2,0);
    
 if (actual_choice==1)
    pchoose=soft(val1,val2,tau); % probability of 1
else
    pchoose=soft(val2,val1,tau); % probability of 2
 end   
 
 lik = lik + log(pchoose);
 
end

F = sum(lik+eps);

end