% Barnby & Dayan (2021) Inequality Aversion and Paranoia
%
% Joe Barnby j.barnby@uq.edu.au 2021

% Full Bayesian model
% Estimates:
%1  alpha (1) and beta (2) of participant over trials 1:18
%2  A participant's inferred alpha and beta given their own decisions
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

function[F] = Model_3(parms, data)

   % Initialise
   
      res = 15; % resolution of grid and alpha

   %phase 1 & 2 parms
alpha_raw       = parms(1); % subjects alpha for phase 1
alpha           = res*(1./(1+exp(-alpha_raw)));% restrict alpha to between 0 and res
beta            = parms(2); % subjects beta for phase 1

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

val1ppt = alpha*s1 + beta*max(s1-o1,0) ; %subject uses their own alpha and beta instead of the partner's
val2ppt = alpha*s2 + beta*max(s2-o2,0) ;

subject_netp1 = (1./(1+exp(-(val1ppt - val2ppt))));

subject_estimate = data(t, 7); % say the subject thought that the partner would go for 2

    if (subject_estimate==1)
        lik2 = lik2 + log(subject_netp1); % log likelihood 
    else
        lik2 = lik2 + log(1-subject_netp1);
    end

end

F  = lik1 + lik2 + eps;

end