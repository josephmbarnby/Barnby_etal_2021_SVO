
# Intentions two stage Bayesian Probabilistic Estimation ------------------

Intentions_PhaseOne_Peter <- function(par, data, detail = T){
  # how the experimenter learns online about the subject in phase 1
  
  alpha_m = par[1]
  beta_m  = par[2]
  alpha_sd = par[3]
  beta_sd  = par[4]
  
  tn             = length(data[,'Trial'])+1
  res = 81
  pab_norm       = rep(NA, tn)
  actual_choice <- rep(NA, tn)
  
  alpha <- matrix(seq(from = -10, to = 10, by = 0.25), ncol = res, nrow = res, byrow = T)
  beta  <- t(matrix(seq(from = -10, to = 10, by = 0.25), ncol = res, nrow = res, byrow = T))
  
  if (length(par) < 6){
    alpha_p <- alpha
    beta_p  <- beta  
  } else {
    alpha_p <- par[5]
    beta_p  <- par[6]
  }
  
  pab   <- list()
  ll    <- rep(NA, tn)
  simA  <- rep(NA, tn)
  simab <- matrix(NA, nrow = tn, ncol = 4)
  
  #initialise priors
  pab[[1]]    <- dnorm(alpha, alpha_m, alpha_sd) * 
                 dnorm(beta,  beta_m, beta_sd) 
  pab_norm[1] <- sum(as.vector(pab[[1]]))
  ll[1]       <- NA
  pab[[1]]    <- pab[[1]] / pab_norm[1] # set prior normalisation

  #iterate over priors
  for (t in 2:tn){
    
    choiceab <- matrix(c(data[t-1,'Option1_PPT'], 
                         data[t-1,'Option1_Partner'], 
                         data[t-1,'Option2_PPT'], 
                         data[t-1,'Option2_Partner']),
                       nrow = 2, ncol = 2)
    
    opt1  = choiceab[,1]/10
    opt2  = choiceab[,2]/10
    
    #Value for each option
    val1 = alpha_p * opt1[1] + beta_p * max(opt1[1]-opt1[2],0) 
    val2 = alpha_p * opt2[1] + beta_p * max(opt2[1]-opt2[2],0)
    
    actual_choice[t]   = data[t-1, 'Response']
    sigmoid            = function(x){1/(1+exp(-x))}
    
    if (actual_choice[t] == 1){
      pchoose    = sigmoid(val1 - val2); # probability of 1
    } else {
      pchoose    = sigmoid(val2 - val1); # probability of 2

    }
    
    pab[[t]]    = pchoose*pab[[t-1]] # Bayes rule
    pab_norm[t] = sum(as.vector(pab[[t]]))
    ll[t]       = log(pab_norm[t])
    pab[[t]]    = pab[[t]]/pab_norm[t]

  
    if (detail == T){
      
    subject_estimate_pchoose1 = sigmoid(val1 - val2);
    tmp                       = subject_estimate_pchoose1 * pab[[t]];
    subject_netp1             = sum(as.vector(tmp));
    subject_netp2             = 1-subject_netp1;
    
    simA[t]      <- sample(c(1,2), 1, prob = c(subject_netp1, 1-subject_netp1))
    simab[t,1:4] <- c(choiceab[,1], choiceab[,2])
    
    } else {
      
    return(sum(ll[2:19]))
      
    }
  }
      
      alpha_marginal  = rowSums(pab[[tn]]) # work out the marginals over the components,
      beta_marginal   = colSums(pab[[tn]])
      
    output <- data.frame(  
    
    alpha_marginal  = alpha_marginal,
    beta_marginal   = beta_marginal,
    Action          = c(actual_choice, rep(NA, res-tn)),
    simA            = c(simA, rep(NA, res-tn)),
    ll              = c(ll, rep(NA, res-tn)),
    Op1SimPPT       = c(simab[,1], rep(NA, res-tn)),
    Op1SimPAR       = c(simab[,2], rep(NA, res-tn)),
    Op2SimPPT       = c(simab[,3], rep(NA, res-tn)),
    Op2SimPAR       = c(simab[,4], rep(NA, res-tn)),
    sumll           = rep(sum(ll[2:19]), res)
)   
      return(output)
}  


Intentions_PhaseTwo_Peter <- function(par = 0.5, pri, data, detail = T){

  param_zeta        = par[1];
  
  param_alpha_shape <- pri[1]
  param_alpha_scale <- pri[2]
  param_beta_shape  <- pri[3]
  param_beta_scale  <- pri[4]
  param_gamma_shape <- pri[5]
  param_gamma_scale <- pri[6]
  
  res = 61

  alpha <- array(matrix(seq(from = 0, to = 15, by = 0.25), ncol = res, nrow = res, byrow = T), 
                 dim = c(res, res, res), 
                 dimnames = list(rep('alpha',res),rep('beta',res),rep('gamma', res)))
  beta  <- array(t(matrix(seq(from = 0, to = 15, by = 0.25), ncol = res, nrow = res, byrow = T)), 
                 dim = c(res, res, res), 
                 dimnames = list(rep('alpha',res),rep('beta',res),rep('gamma', res)))
  gamma <- array(matrix(1, ncol = res, nrow = res), 
                 dim = c(res, res, res))
  for(i in 1:res) gamma[,,i] <- gamma[,,i] * i
  gamma <- array(gamma, dim = c(res, res, res), dimnames = list(rep('alpha',res),rep('beta',res),rep('gamma', res)))
  
  sigmoid   = function(x){1/(1+exp(-x))}

  pabg <- dgamma(alpha, param_alpha_shape, param_alpha_scale) * 
          dgamma(beta,  param_beta_shape,  param_beta_scale) * 
          dgamma(gamma, param_gamma_shape, param_gamma_scale);
  
  pabg[which(!is.finite(pabg))] <- 0
  
  pabg <- pabg / sum(as.vector(pabg)); 
  pabg <- pabg^(1/upi)
  pabg <- pabg / sum(as.vector(pabg)); 
  
  ll <- rep(NA, 36)
  tn = 36
  
  for (t in 1:tn){
    
    choiceab <- matrix(c(data[t,'Option1_PPT'], 
                         data[t,'Option1_Partner'], 
                         data[t,'Option2_PPT'], 
                         data[t,'Option2_Partner']),
                       nrow = 2, ncol = 2)
    opt1  = choiceab[,1]/10
    opt2  = choiceab[,2]/10
    
    #Value for each option
    val1 = alpha*opt1[1] + beta*max(opt1[1]-opt1[2],0) - gamma*max(opt1[2]-opt1[1],0); 
    val2 = alpha*opt2[1] + beta*max(opt2[1]-opt2[2],0) - gamma*max(opt2[2]-opt2[1],0);
    
    actual_choice_ppt = data[t, 'Response']
    actual_choice_par = data[t, 'Answer']
    
    subject_estimate_pchoose1 = sigmoid(val1 - val2);
    tmp                       = subject_estimate_pchoose1 * pabg;
    subject_netp1             = sum(as.vector(tmp));
    subject_netp2             = 1-subject_netp1;
    
    # adjust for over/under matching
    subject_adjp1           = subject_netp1^param_zeta/(subject_netp1^param_zeta + subject_netp2^param_zeta);
    subject_report          = actual_choice_ppt; # say the subject thought that the partner would go for 2
    
    if (subject_report==1){
      ll[t] = log(subject_adjp1); # log likelihood 
    }else{
      ll[t] = log(1-subject_adjp1);
    }
    
    if (actual_choice_par == 1){
      pchoose=sigmoid(val1 - val2); # probability of 1
    } else {
      pchoose=sigmoid(val2 - val1); # probability of 2
    }
    
    if(t==1) {newpabg = pchoose*pabg} else {newpabg = pchoose*newpabg}; # Bayes rule
    newpabg = newpabg/sum(as.vector(newpabg)); # so distribution
    
  }

  if (detail == F){
    return(sum(ll))
  } else {
    
    output <- data.frame(  
      alpha_prior     = rowSums(pabg),# work out the marginals over the components,
      beta_prior      = rowSums(pabg, dim = 2) %>% colSums(),
      gamma_prior     = colSums(pabg, dim = 2),
      alpha_marginal  = rowSums(newpabg),# work out the marginals over the components,
      beta_marginal   = rowSums(newpabg, dim = 2) %>% colSums(),
      gamma_marginal  = colSums(newpabg, dim = 2)
      
    )  
    return(output)
  }  
}


