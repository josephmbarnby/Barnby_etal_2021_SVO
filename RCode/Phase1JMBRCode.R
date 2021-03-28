

# Heuristic model ---------------------------------------------------------

Intentions_PhaseOne_Heuristic <- function(par, data, detail = T){
  # how the experimenter learns online about the subject in phase 1
  #Heuristic model that doesn't rely on probabilistic distributions
  
  parm_alpha <- par[1] # how much value the participant places on their own subjective reward
  parm_beta  <- par[2] # how inequality averse a participant is (when they receive more than their partner)
  
  parm_alpha2 <- parm_alpha
  parm_beta2  <- parm_beta
  
  tn             = length(data[,'Trial']) # for trial repetition loop
  actual_choice <- rep(NA, tn) # to save choices made by participant
  ll    <- rep(NA, tn) # store ll
  simA  <- rep(NA, tn) # store simulated action
  simab <- matrix(NA, nrow = tn, ncol = 4) # store values associated with simulated action
  
  for (t in 1:tn){
    
    choiceab <- matrix(c(data[t,'Option1_PPT'],  # retrieve possible values from data
                         data[t,'Option1_Partner'], 
                         data[t,'Option2_PPT'], 
                         data[t,'Option2_Partner']),
                       nrow = 2, ncol = 2)
    
    actual_choice[t]   = data[t, 'Response']      # save actual response
    
    if (actual_choice[t] == 1){
    optA  = choiceab[,1]/10 # split option pairs between outcomes for option 1 and option 2
    optR  = choiceab[,2]/10
    } else {
    optR  = choiceab[,1]/10
    optA  = choiceab[,2]/10
    }
    
    #Value for each option
    valA = parm_alpha  * optA[1] + parm_beta  * max(optA[1] - optA[2],0) #calculate values for each given parms
    valR = parm_alpha2 * optR[1] + parm_beta2 * max(optR[1] - optR[2],0) #calculate values for each given parms
    
    sigmoid            = function(x){1/(1+exp(-x))} # sigmoid function for probability
    
    pchoose <- sigmoid(valA-valR)
    
    ll[t] <- log(pchoose) # log probability
    
    if (detail == T){ # simulated actions given probability
      
      if (actual_choice[t] == 1){
      simA[t] <- sample(c(1,2), 1, prob = c(pchoose, 1-pchoose))
      } else {
      simA[t] <- sample(c(2,1), 1, prob = c(pchoose, 1-pchoose))
      }
      
    }
    
  }
  
    if (detail == F){
      return(sum(ll))
    }
  
  output <- data.frame(  
    
    Action          = actual_choice,
    simA            = simA,
    ll              = ll,
    Op1SimPPT       = c(NA,data[,'Option1_PPT']),
    Op1SimPAR       = c(NA,data[,'Option1_Partner']),
    Op2SimPPT       = c(NA,data[,'Option2_PPT']),
    Op2SimPAR       = c(NA,data[,'Option2_Partner']),
    sumll           = sum(ll[2:19])
    
  )   
  
  return(output)
  
}


# Heuristic Wrapper -------------------------------------------------------

wrapper_PhaseOne_Heuristic <- function(par, data, scbeta0=-1,check=0){
  
  parM <- as.numeric(par); # in case it's inputed in another format
  parN <- length(parM)
  maxp <- c(10, 10)  # upper boundaries for params  MAY NEED ADJUSTMENT
  minp <- c(-10, -10) # lower boundaries for params  MAY NEED ADJUSTMENT
  
  if ((scbeta0[1] < 0) && !is.na(scbeta0)){ 
    # i.e. a number, but not a valid scaled distr. param.,
    # which means 'use default, weak regularizing priors'
    #  For:     alpha, beta
    scale1 <- c(1.1, 1.1)
    scale2 <- c(1.1, 1.1)
    mp     <- c(-10,-10)
    Mp     <- c(10, 10)
    scbeta0 <- t(matrix(c(scale1,scale2,mp,Mp),2,4))
    if(check){
      colnames(scbeta0) <- c('alpha', 'beta')
      rownames(scbeta0) <- c('scale1','scale2','min','max')
    }
  }
  
  # Cacl. the log prior for MAP purposes etc, all calc'd in short form:
  if (sum(par < minp) || sum(par > maxp)) {  # i.e. if at least one param out of range
    mRWPrior <- Inf
  } else {
    mRWPrior <- 0;
    if (length(scbeta0)>1){  # legit prior must have 3*parN elements or so!
      # dbetasc follows vectorization of dbeta, so can do all the elements of parM 
      # at once, and als get the log-prior straight away: 
      mRWPrior <-  - sum(dbetasc( parM, 
                                  scbeta0[1,1:parN],scbeta0[2,1:parN],
                                  scbeta0[3,1:parN],scbeta0[4,1:parN], log=TRUE)); 
    }
  }
  
  if (mRWPrior == Inf){  # If we are in an a priori prohibited parameter region
    # do not attempt to calculate the likelihood - it will be nonsense anyway.
    return(Inf); 
  } else {
    return(mRWPrior - Intentions_PhaseOne_Heuristic(par,data, detail = F))
  }
  
  
} # end of wrapper_RW


# Intentions two stage Bayesian Probabilistic Estimation ------------------
#Simulations
Phase1Sim <- function(par, data){
  #set up vectors
  tn       <- nrow(data)
  simA     <- rep(NA, tn)
  pchoose1 <- simA
  pchoose  <- simA
  alpha    <- par[1]
  beta     <- par[2]
  
  for (t in 1:tn){
  choiceab <- matrix(c(data[t,'Option1_PPT'], 
                       data[t,'Option1_Partner'], 
                       data[t,'Option2_PPT'], 
                       data[t,'Option2_Partner']),
                     nrow = 2, ncol = 2)
  opt1  = choiceab[,1]/10
  opt2  = choiceab[,2]/10
  val1 = alpha * opt1[1] + beta * max(opt1[1]-opt1[2],0) # fehr-schmidt equation using either alpha as a matrix, or
  val2 = alpha * opt2[1] + beta * max(opt2[1]-opt2[2],0) # alpha as a specific value (see lines 97-103)
  sigmoid                = function(x){1/(1+exp(-x))}  # sigmoid
  pchoose1[t]            = sigmoid(val1 - val2); # probability of 1
  simA[t]    <- sample(c(1,2), 1, prob = c(pchoose1[t], 1-pchoose1[t]))
  pchoose[t] <- ifelse(pchoose1[t] > 1-pchoose1[t], pchoose1[t], 1-pchoose1[t])
  }

  synd <- data.frame(
  simA            = simA,
  prob            = pchoose,
  Op1SimPPT       = data[,'Option1_PPT'], 
  Op1SimPAR       = data[,'Option1_Partner'],
  Op2SimPPT       = data[,'Option2_PPT'], 
  Op2SimPAR       = data[,'Option2_Partner']
  
  )
  return(synd)
}
#Actual Phase1
Intentions_PhaseOne_Bayes <- function(par, data, detail = T, plot = F){
  # how the experimenter learns online about the subject in phase 1
  
  tn             = nrow(data)+1 # trial number
  res            = 81                       # resolution of probability matrix
  pab_norm       = rep(NA, tn)              # empty vector to save normalising constant
  actual_choice <- rep(NA, tn)              # empty vector to save choice made by ppt
  ll    <- rep(NA, tn)                      # vector for loglik
  simA  <- rep(NA, tn)                      # vector for simulated action given probability of each choice
  simab <- matrix(NA, nrow = tn, ncol = 4)  # matrix for values given simulated outcome
  
  alpha <-   matrix(seq(from = -10, to = 10, by = 0.25), ncol = res, nrow = res, byrow = T)  # parameter sequence for probability distributions to move across for alpha
  beta  <- t(matrix(seq(from = -10, to = 10, by = 0.25), ncol = res, nrow = res, byrow = T)) # parameter sequence for probability distributions to move across for beta
  
  #initialise priors
  
  pab         <- dunif(alpha, -10, 10) *  #  *itialise joint probability distribution p(ab) with flat priors
                 dunif(beta,  -10, 10) 
  pab_norm[1] <- sum(as.vector(pab))       # initialise normalising constant
  ll[1]       <- NA
  pab         <- pab / pab_norm[1] # set prior normalisation
  
  if(plot == T){
  heatmap( t(pab),
           Rowv=NA, Colv=NA, col = topo.colors(512), 
           scale="none", margins=c(5,8),asp=1,
           labRow=colnames(pab[[1]]),
           labCol=rownames(pab[[1]]), 
           main = paste('\n Prior beliefs'),
           xlab='Beta',ylab='Alpha')
  }
  
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
    val1 = alpha * opt1[1] + beta * max(opt1[1]-opt1[2],0) # fehr-schmidt equation using either alpha as a matrix, or
    val2 = alpha * opt2[1] + beta * max(opt2[1]-opt2[2],0) # alpha as a specific value (see lines 97-103)
    
    actual_choice[t]   = data[t-1, 'Response']       # store actual choice
    sigmoid            = function(x){1/(1+exp(-x))}  # sigmoid
    
    if (actual_choice[t] == 1){
      pchoose    = sigmoid(val1 - val2); # probability of 1
    } else {
      pchoose    = sigmoid(val2 - val1); # probability of 2
    }
    
    pab         <- pchoose*pab         # Bayes rule
    pab_norm[t] <- sum(as.vector(pab)) # normalising constant
    ll[t]       <- log(pab_norm[t])    # log probability
    pab         <- pab/pab_norm[t]     # normalise for next iteration
    
    if(plot == T){
    heatmap( t(pab),
             Rowv=NA, Colv=NA, col = topo.colors(512), 
             scale="none", margins=c(5,8),asp=1,
             labRow=colnames(pab),
             labCol=rownames(pab), 
             main = paste('\n Prior beliefs'),
             xlab='Beta',ylab='Alpha')
    }
    
    if (detail == T){
      
    simA[t]      <- ifelse(actual_choice[t] == 1,
                           sample(c(1,2), 1, prob = c(pab_norm[t], 1-pab_norm[t])), # simulate outcome given p(ab)
                           sample(c(2,1), 1, prob = c(pab_norm[t], 1-pab_norm[t]))
                           )
    
    simab[t,1:4] <- c(choiceab[,1], choiceab[,2])
    
    } 
  }
  
    if (detail == F ) {
        return(sum(ll[2:19]))
    }  
      
    alpha_marginal  = colSums(pab) # work out the marginals over the components at the last p(ab)
    beta_marginal   = rowSums(pab)
    x <- which.max(alpha_marginal) # calculate the row with the modal value
    y <- which.max(beta_marginal )
    
    prior      <- seq(-10, 10, length.out = res) # create parameter space
    alpha_parm <- prior[x]                       # extract value at max probability (stored at line 157)
    beta_parm  <- prior[y]

    output <- data.frame(  
    
    alpha_marginal  = alpha_marginal,
    beta_marginal   = beta_marginal,
    MAP_A           = rep(alpha_parm,res),
    MAP_B           = rep(beta_parm,res),
    Action          = c(actual_choice, rep(NA, (res-tn))),
    simA            = c(simA, rep(NA, res-tn)),
    pab_norm        = c(NA, pab_norm[2:nrow(data)], rep(NA, res-tn+1)),
    ll              = c(ll, rep(NA, res-tn)),
    Op1SimPPT       = c(simab[,1], rep(NA, (res-tn))),
    Op1SimPAR       = c(simab[,2], rep(NA, (res-tn))),
    Op2SimPPT       = c(simab[,3], rep(NA, (res-tn))),
    Op2SimPAR       = c(simab[,4], rep(NA, (res-tn))),
    sumll           = rep(sum(ll[2:nrow(data)]), res)
)   
      return(output)
}  

Intentions_PhaseTwo_Bayes <- function(par, pri, data, detail = T){

  param_alpha_prior = par[1]
  param_beta_prior  = par[2]
  param_zeta        = par[3]
  param_upi         = par[4]
  
  tn             = length(data[,'Trial'])+1
  res = 81
  pab_norm       = rep(NA, tn)
  actual_choice <- rep(NA, tn)
  
  alpha <- matrix(seq(from = -10, to = 10, by = 0.25), ncol = res, nrow = res, byrow = T)
  beta  <- t(matrix(seq(from = -10, to = 10, by = 0.25), ncol = res, nrow = res, byrow = T))
  sigmoid   = function(x){1/(1+exp(-x))}
  
  pab   <- list()
  ll    <- rep(NA, tn)
  simA  <- rep(NA, tn)
  simab <- matrix(NA, nrow = tn, ncol = 4)
  
  #initialise priors
  pab[[1]]    <- dnorm(alpha, 0, 2) * 
                 dnorm(beta,  0, 2) 
  pab_norm[1] <- sum(as.vector(pab[[1]]))
  ll [1]      <- NA
  pab[[1]]    <- pab[[1]] / pab_norm[1] # set prior normalisation
  
  pab[[1]] <- pab[[1]] / sum(as.vector(pab[[1]])); 
  pab[[1]] <- pab[[1]]^(1/param_upi)
  pab[[1]] <- pab[[1]] / sum(as.vector(pab[[1]])); 
  
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
    val1 = param_alpha_prior*opt1[1] + param_beta_prior*max(opt1[1]-opt1[2],0)
    val2 = param_alpha_prior*opt2[1] + param_beta_prior*max(opt2[1]-opt2[2],0)
    
    actual_choice_ppt = data[t, 'Response']
    actual_choice_par = data[t, 'Answer']
    
    subject_estimate_pchoose1 = sigmoid(val1 - val2);
    tmp                       = subject_estimate_pchoose1 * pabg[[t-1]];
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
    
    pab[[t]]    <- pchoose*pab[[t-1]]; # Bayes rule
    pab_norm[t] <- sum(as.vector(pab[[t]]))
    pab[[t]]    <- pab[[t]]/pab_norm[t]; # so distribution
    pab[[t]]    <- pab[[t]]^(1/param_upi) #policy inverse precision
    pab_norm[t] <- sum(as.vector(pab[[t]]))
    pab[[t]]    <- pab[[t]]/pab_norm[t]; # so distribution
  
    
  }

  if (detail == F){
    return(sum(ll))
  } else {
    
    output <- data.frame(  
      sumll           = sum(ll),
      alpha_prior     = colSums(pab[[1]]),# work out the marginals over the components,
      beta_prior      = rowSums(pab[[1]]),
      alpha_marginal  = colSums(pab[[37]]),# work out the marginals over the components,
      beta_marginal   = rowSums(pab[[37]])
      
    )  
    return(output)
  }  
}


