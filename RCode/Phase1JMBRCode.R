
# Intentions two stage Bayesian Probabilistic Estimation ------------------

Intentions_PhaseOne_Peter <- function(par, data, detail = T){
  # how the experimenter learns online about the subject in phase 1
  library(EnvStats)
  
  if (par < 6) {par <- c(0,1,0,1,4,1)}
  
  alpha_shape_prior <- par[1]
  alpha_scale_prior <- par[2]
  beta_shape_prior  <- par[3]
  beta_scale_prior  <- par[4]
  gamma_shape_prior <- par[5]
  gamma_scale_prior <- par[6]
  
  res = 81
  
  alpha <- array(  matrix(seq(from = -10, to = 10, by = 0.25), ncol = res, nrow = res, byrow = T), 
                   dim = c(res, res, res), 
                   dimnames = list(rep('alpha',res),rep('beta',res),rep('gamma', res)))
  beta  <- array(t(matrix(seq(from = -10, to = 10, by = 0.25), ncol = res, nrow = res, byrow = T)), 
                 dim = c(res, res, res), 
                 dimnames = list(rep('alpha',res),rep('beta',res),rep('gamma', res)))
  gamma <- array(  matrix(-10, ncol = res, nrow = res), 
                   dim = c(res, res, res))
  for(i in 1:res) {x <- seq(from = -10, to = 10, by = 0.25); gamma[,,i] <- x[i]}
  gamma <- array(gamma, dim = c(res, res, res), dimnames = list(rep('alpha',res),rep('beta',res),rep('gamma', res)))
  
  pabg <- dnorm(alpha, alpha_shape_prior, alpha_scale_prior) * 
          dnorm(beta,  beta_shape_prior,  beta_scale_prior) * 
          dnorm(gamma, gamma_shape_prior, gamma_scale_prior);
  
  pabg <- pabg / sum(as.vector(pabg)); 
  
  ll            <- rep(NA, 18)
  actual_choice <- rep(NA, 18)
  tn = length(data[,'Trial'])
  simA          <- ll
  
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
    
    actual_choice[t] = data[t, 'Response']
    sigmoid          = function(x){1/(1+exp(-x))}
    
    if (actual_choice[t] == 1){
      pchoose =sigmoid(val1 - val2); # probability of 1
    } else {
      pchoose =sigmoid(val2 - val1); # probability of 2
    }
    
    pabg                      = pchoose*pabg # Bayes rule
    pabg                      = pabg/sum(as.vector(pabg))
    
  }
    
    if (detail == T){
    #loop over each trial given the model (pabg) and calculate the likelihood of each choice given the model.

    for (t in 1:tn){
      
      alpha_marginal  = rowSums(pabg) # work out the marginals over the components,
      beta_marginal   = rowSums(pabg, dim = 2) %>% colSums()
      gamma_marginal  = colSums(pabg, dim = 2)
      
      choiceab <- matrix(c(data[t,'Option1_PPT'], 
                           data[t,'Option1_Partner'], 
                           data[t,'Option2_PPT'], 
                           data[t,'Option2_Partner']),
                           nrow = 2, ncol = 2)
      
      opt1  = choiceab[,1]/10
      opt2  = choiceab[,2]/10
      
      #Value for each option
      val1 = alpha_marginal*opt1[1] + beta_marginal*max(opt1[1]-opt1[2],0) - gamma_marginal*max(opt1[2]-opt1[1],0); 
      val2 = alpha_marginal*opt2[1] + beta_marginal*max(opt2[1]-opt2[2],0) - gamma_marginal*max(opt2[2]-opt2[1],0);
      
      actual_choice[t] = data[t, 'Response']
      
      subject_estimate_pchoose1 = sigmoid(val1 - val2);
      tmp                       = subject_estimate_pchoose1 * pabg;
      subject_netp1             = sum(as.vector(tmp));
      subject_netp2             = 1-subject_netp1;
      
      if (actual_choice[t]==1){
        ll[t] = log(subject_netp1); # log likelihood 
        simA[t] <- sample(c(1,2), 1, prob = c(subject_netp1, 1-subject_netp1))
      }else{
        ll[t] = log(1-subject_netp1);
        simA[t] <- sample(c(2,1), 1, prob = c(subject_netp1, 1-subject_netp1))
      }
      
      
      
    }
      
    }

    if (detail == F){
      

      
    return(sum(ll))
      
    } else {
      
      alpha_marginal  = rowSums(pabg) # work out the marginals over the components,
      beta_marginal   = rowSums(pabg, dim = 2) %>% colSums()
      gamma_marginal  = colSums(pabg, dim = 2)
      
    output <- data.frame(  
    
    alpha_marginal  = alpha_marginal,
    beta_marginal   = beta_marginal,
    gamma_marginal  = gamma_marginal,
    simA            = c(simA, rep(NA, res-tn)),
    Action          = c(actual_choice, rep(NA, res-tn)),
    ll              = c(ll, rep(NA, res-tn)),
    sumll           = rep(sum(ll), res)
)   
      return(output)
    }  
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


