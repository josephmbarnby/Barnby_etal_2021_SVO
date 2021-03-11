
Intentions_Phase1 <- function(par, data, detail=T){
  
  alpha1  = par[1] #alpha quantifies the subjective benefit of individual return
  beta1   = par[2] #'guilt' (if it is negative, the subjective worth is
  #reduced for a subject if she earns more than her partner)
  gamma1  = par[3] #'envy'(if it is negative, the subjective worth is reduced 
  #for a subject if the partner earns more than her)
  
  #initialise vectors for storing information
  t_n    = 18
  ll     <- rep(0, 18)
  action <- rep(0, 18)
  valuei <- rep(0, 18)
  valuea <- rep(0, 18)
  simA   <- rep(0, 18)
  
  for (t in 1:t_n){
    
    actual_choice  <-  as.numeric(data[t,'Response'])
    
    choiceab       <- matrix(c(data[t,'Option1_PPT'], 
                               data[t,'Option1_Partner'], 
                               data[t,'Option2_PPT'], 
                               data[t,'Option2_Partner']),
                               nrow = 2, ncol = 2)
    
    if(actual_choice == 1){
    optI           <- choiceab[,1]/10
    optA           <- choiceab[,2]/10
    } else {
    optI           <- choiceab[,2]/10
    optA           <- choiceab[,1]/10 
    }
    
    #Here is the inequality aversion equation for self and the alternative option
    
    valuei[t] <- (alpha1 * optI[1]) + (beta1 * max(optI[1] - optI[2],0)) + (gamma1 * max(optI[2] - optI[1],0))
    valuea[t] <- (alpha1 * optA[1]) + (beta1 * max(optA[1] - optA[2],0)) + (gamma1 * max(optA[2] - optA[1],0))
    
    softValue <- c(valuea[t], valuei[t])
    
    #probability
    l       <- exp(valuei[t])/sum(exp(softValue)) #from Story et al., 2015
    ll[t]   <- log(l)
    
    Pi = c(l, 1-l)
    
    if(detail == T){
      
      if(actual_choice == 1){
        simA[t]                   <- sample(c(1, 2), 1, prob = Pi)  # simulated action (to avoid unreferencing)
      } else {
        simA[t]                   <- sample(c(2, 1), 1, prob = Pi) 
      }
    }
  }
  
  #Save outcomes of learning loops into an output
  if (detail == F) {
    
    return(sum(ll))
    
  } else {
    
    output <- matrix(
      NA,
      nrow = 18,
      ncol = 6
    )
    
    colnames(output) <- c("Trial", "Action",
                          "ll", "Value_I", "Value_A",
                          'simAction')
    
    output[,1:2] <- matrix(c(data[,'Trial'], data[,'Response']), nrow = 18, ncol = 2)
    output[,3:5] <- matrix(c(ll, valuei, valuea), nrow = 18, ncol = 3)
    output[,6]   <- simA
    
    
    return(output)
    
  }
  
}

Intentions_PhaseOne_Peter <- function(data, detail = T){
  # how the experimenter learns online about the subject in phase 1

  alpha <- array(matrix(seq(from = 0, to = 10, by = 0.25), ncol = 41, nrow = 41, byrow = T), dim = c(41, 41, 41), dimnames = list(rep('alpha',41),rep('beta',41),rep('gamma', 41)))
  beta  <- array(t(matrix(seq(from = 0, to = 10, by = 0.25), ncol = 41, nrow = 41, byrow = T)), dim = c(41, 41, 41), dimnames = list(rep('alpha',41),rep('beta',41),rep('gamma', 41)))
  gamma <- array(matrix(1, ncol = 41, nrow = 41), dim = c(41, 41, 41))
  for(i in 1:41) gamma[,,i] <- gamma[,,i] * i
  gamma <- array(gamma, dim = c(41, 41, 41), dimnames = list(rep('alpha',41),rep('beta',41),rep('gamma', 41)))
  
  pabg <- dgamma(alpha, 3, 1) * dgamma(beta, 4, 1) * dgamma(gamma, 5, 1)
  pabg <- pabg / sum(as.vector(pabg)); 
  
  ll <- rep(NA, 18)
  tn = 18
  
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
    
    actual_choice = data[t, 'Response']
    softmax       = function(x){1/(1+exp(-x))}
    
    if (actual_choice == 1){
      pchoose =softmax(val1 - val2); # probability of 1
      pchooseA=softmax(val2 - val1); # probability of 2
    } else {
      pchoose =softmax(val2 - val1); # probability of 2
      pchooseA=softmax(val1 - val2); # probability of 1
    }
    
    if(t==1) {newpabg = pchoose*pabg} else {newpabg = pchoose*newpabg}; # Bayes rule
    newpabg = newpabg/sum(as.vector(newpabg)); # so distribution

    if(t==1) {newpabgA = pchooseA*pabg} else {newpabgA = pchooseA*newpabgA};
    newpabgA = newpabgA/sum(as.vector(newpabgA)); # so distribution
    
    ll[t] <- median(log(pchoose))
    
  }
  
  #alpha_marginal = squeeze(sum(newpabg,[1 3])); # work out the marginals over the components
  #beta_marginal  = squeeze(sum(newpabg,[2 3]'));
  #gamma_marginal = squeeze(sum(newpabg,[1 2]));
    if (detail == F){
    return(sum(ll))
    }else{
      
      alpha_marginal  = rowSums(newpabg) # work out the marginals over the components,
      beta_marginal   = rowSums(newpabg, dim = 2) %>% colSums()
      gamma_marginal  = colSums(newpabg) %>% colSums()
      
      for(i in 1:length(alpha_marginal)){
        alpha_marginal[i] <- ifelse(alpha_marginal[i] == 0, 1e-10, alpha_marginal[i])
         beta_marginal[i] <- ifelse( beta_marginal[i] == 0, 1e-10,  beta_marginal[i])
        gamma_marginal[i] <- ifelse(gamma_marginal[i] == 0, 1e-10, gamma_marginal[i])
      }
      
    output <- data.frame(  
    alpha_marginalP = rowSums(pabg),# prior
    alpha_marginal  = alpha_marginal,# work out the marginals over the components,
    alpha_shape     = fitdistr(as.numeric(alpha_marginal), "gamma", start=list(shape=1, scale=1))$estimate[1],
    alpha_scale     = fitdistr(as.numeric(alpha_marginal), "gamma", start=list(shape=1, scale=1))$estimate[2],
    beta_marginal   = beta_marginal,
    beta_shape      = fitdistr(as.numeric(beta_marginal), "gamma", start=list(shape=1, scale=1))$estimate[1],
    beta_scale      = fitdistr(as.numeric(beta_marginal), "gamma", start=list(shape=1, scale=1))$estimate[2],
    gamma_marginal  = gamma_marginal,
    gamma_shape     = fitdistr(as.numeric(gamma_marginal), "gamma", start=list(shape=1, scale=1))$estimate[1],
    gamma_scale     = fitdistr(as.numeric(gamma_marginal), "gamma", start=list(shape=1, scale=1))$estimate[2]
)   
      return(output)
    }  
  
}
