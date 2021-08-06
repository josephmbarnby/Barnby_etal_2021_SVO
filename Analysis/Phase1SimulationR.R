Phase1Sim <- function(par, data){

  #set up vectors
  tn       <- 18
  simA     <- rep(NA, tn)
  pchoose1 <- simA
  pchoose  <- simA
  ll       <- simA
  alpha    <- par[1]
  beta     <- par[2]
  Answer   <- simA

  for (t in 1:tn){

    s1 = data[t, 2]/10;
    o1 = data[t, 3]/10;
    s2 = data[t, 4]/10;
    o2 = data[t, 5]/10;

    val1 = alpha * s1 + beta * max(s1-o1,0) # fehr-schmidt equation using either alpha as a matrix, or
    val2 = alpha * s2 + beta * max(s2-o2,0) # alpha as a specific value (see lines 97-103)

    sigmoid                = function(x){1/(1+exp(-x))}  # sigmoid
    pchoose1[t]            = sigmoid(val1 - val2); # probability of 1
    simA[t]                <- sample(c(1,2), 1, prob = c(pchoose1[t], 1-pchoose1[t]))

    answer = data[t,'Answer']
    if(answer == 1)
      pchoose[t] = pchoose1[t]
    else
      pchoose[t] = 1-pchoose1[t]

  }


  synd <- data.frame(
    simA            = simA,
    prob            = pchoose,
    Op1SimPPT       = data[,'Option1_PPT'],
    Op1SimPAR       = data[,'Option1_Partner'],
    Op2SimPPT       = data[,'Option2_PPT'],
    Op2SimPAR       = data[,'Option2_Partner'],
    Answer          = data[,'Answer']
  )
  return(synd)
}

Phase1Fit <- function(par, data){

  #set up vectors
  tn       <- 18
  ll       <- 0
  alpha    <- par[1]
  beta     <- par[2]

  for (t in 1:tn){

    s1 = data[t, 3]/10;
    o1 = data[t, 4]/10;
    s2 = data[t, 5]/10;
    o2 = data[t, 6]/10;

    val1 = (alpha * s1) + (beta * max(s1-o1,0)) # fehr-schmidt equation using either alpha as a matrix, or
    val2 = (alpha * s2) + (beta * max(s2-o2,0)) # alpha as a specific value (see lines 97-103)

    action = data[t,'Answer']

    pchoose1               = 1/(1+exp(-(val1 - val2))); # probability of 1

    if(action == 1){
      ll = ll + log(pchoose1)
    } else {
      ll = ll + log(1-pchoose1)
    }

  }

  return(ll)

}

Phase1Wrapper <- function(ParM, datAr, scbeta0=NA,details=0){

  parM <- as.vector(ParM); # in case it's inputed in another format
  parn <- length(parM)

  if ((scbeta0[1] < 0) && !is.na(scbeta0)){
    # i.e. a number, but not a valid scaled distr. param.,
    # which means 'use default, weak regularizing priors'
    scbeta0 <- matrix(c(1.2,3.6,    0, 200,
                        2,  2  , -200, 200
                        ), 4, 2)
    if(details){
      colnames(scbeta0) <- c('alpha', 'beta')
      rownames(scbeta0) <- c('ashape','bshape','min','max')
    }
  }

  # Cacl. the log prior for MAP purposes etc, all calc'd in short form:
  mSLPrior <- 0;
  if (length(scbeta0)>1){  # legit prior must have 24 elements or so!
    mSLPrior <- mSLPrior - sum(dbetasc( parM,
                                        scbeta0[1,1:parn],scbeta0[2,1:parn],
                                        scbeta0[3,1:parn],scbeta0[4,1:parn], log=TRUE));
  }

  if (!details){
    if (mSLPrior == Inf){  # If we are in an a priori prohibited parameter region
      # do not attempt to calculate the likelihood - it will be nonsense anyway.
      return(Inf);
    } else {
      return(mSLPrior - Phase1Fit(ParM,datAr))
    }
  } else {
    res = list();
    res[[2]] <- scbeta0;
    res[[3]] <- ParM;        res[[4]] <- datAr;
    if (mSLPrior == Inf){
      res[[1]] <- Inf
    } else {
      res[[1]] <- mSLPrior - Phase1Fit(ParM,datAr);
    }
    names(res) <- c('sumL','scbeta0','par','dat')
    return(res)
  }


} # end of msLPhisi1a

Model11_Sim <- function(par, data){

  #set up vectors
  tn       <- 18
  tn2      <- 54
  simA     <- rep(NA, tn2)
  pchoose1 <- c(simA, NA)
  pchoose  <- c(simA, NA)
  ll       <- c(simA, NA)
  alpha    <- par[1]
  beta     <- par[2]
  tau      <- par[3]
  lrc      <- par[4]
  Answer   <- c(simA, NA)
  Q <- matrix(NA, 55, 4)
  Q[18,] <- c(0.33, 0.33, 0.33, 0)

  for (t in 1:tn){

    s1 = data[t, 2]/10;
    o1 = data[t, 3]/10;
    s2 = data[t, 4]/10;
    o2 = data[t, 5]/10;

    val1 = alpha * s1 + beta * max(s1-o1,0) # fehr-schmidt equation using either alpha as a matrix, or
    val2 = alpha * s2 + beta * max(s2-o2,0) # alpha as a specific value (see lines 97-103)

    sigmoid                = function(x){1/(1+exp(-x))}  # sigmoid
    pchoose1[t]            = sigmoid(val1 - val2); # probability of 1
    simA[t]                <- sample(c(1,2), 1, prob = c(pchoose1[t], 1-pchoose1[t]))

    answer = data[t,'Answer']
    if(answer == 1)
      pchoose[t] = pchoose1[t]
    else
      pchoose[t] = 1-pchoose1[t]

  }

  for (t in (tn+1):(tn2+1)) {

  choice = as.numeric(data[t-1,'GuessAction']);
  outcome= as.numeric(data[t-1,'Answer']);

  # copy forward action values to next trial
  Q[t,] = Q[t-1,]

  pe    = outcome - Q[t,choice]

  # update option chosen on this trial for next trial's choice
    Q[t,choice] = Q[t,choice] + (lrc * pe);

    prob      = pGibbs(Q[t-1,1:3],tau)
    simA[t]   = sample(c(1, 2, 3), 1, prob = prob)
    pchoose[t]= max(prob)

  }

  synd <- data.frame(
    simA            = simA,
    prob            = pchoose,
    Op1SimPPT       = c(NA, data[,'Option1_PPT']),
    Op1SimPAR       = c(NA, data[,'Option1_Partner']),
    Op2SimPPT       = c(NA, data[,'Option2_PPT']),
    Op2SimPAR       = c(NA, data[,'Option2_Partner']),
    Answer          = c(NA, data[,'Answer'])
  )
  return(synd)
}

Model12_Sim <- function(par, data){

  #set up vectors
  tn       <- 18
  tn2      <- 54
  simA     <- rep(NA, tn2)
  pchoose1 <- c(simA, NA)
  pchoose  <- c(simA, NA)
  ll       <- c(simA, NA)
  alpha    <- par[1]
  beta     <- par[2]
  tau      <- par[3]
  lrc_u    <- par[4]
  lrc_d    <- par[5]
  Answer   <- c(simA, NA)
  Q <- matrix(NA, 55, 4)
  Q[18,] <- c(0.33, 0.33, 0.33, 0)

  for (t in 1:tn){

    s1 = data[t, 2]/10;
    o1 = data[t, 3]/10;
    s2 = data[t, 4]/10;
    o2 = data[t, 5]/10;

    val1 = alpha * s1 + beta * max(s1-o1,0) # fehr-schmidt equation using either alpha as a matrix, or
    val2 = alpha * s2 + beta * max(s2-o2,0) # alpha as a specific value (see lines 97-103)

    sigmoid                = function(x){1/(1+exp(-x))}  # sigmoid
    pchoose1[t]            = sigmoid(val1 - val2); # probability of 1
    simA[t]                <- sample(c(1,2), 1, prob = c(pchoose1[t], 1-pchoose1[t]))

    answer = data[t,'Answer']
    if(answer == 1)
      pchoose[t] = pchoose1[t]
    else
      pchoose[t] = 1-pchoose1[t]

  }

  for (t in (tn+1):(tn2+1)) {

    choice = as.numeric(data[t-1,'GuessAction']);
    outcome= as.numeric(data[t-1,'Answer']);

    # copy forward action values to next trial
    Q[t,] = Q[t-1,]

    pe    = outcome - Q[t,choice]

    # update option chosen on this trial for next trial's choice
    if(pe > 0){
    Q[t,choice] = Q[t,choice] + (lrc_u * pe);
    }else{
    Q[t,choice] = Q[t,choice] + (lrc_d * pe);
    }

    prob      = pGibbs(Q[t-1,1:3],tau)
    simA[t]   = sample(c(1, 2, 3), 1, prob = prob)
    pchoose[t]= max(prob)

  }

  synd <- data.frame(
    simA            = simA,
    prob            = pchoose,
    Op1SimPPT       = c(NA, data[,'Option1_PPT']),
    Op1SimPAR       = c(NA, data[,'Option1_Partner']),
    Op2SimPPT       = c(NA, data[,'Option2_PPT']),
    Op2SimPAR       = c(NA, data[,'Option2_Partner']),
    Answer          = c(NA, data[,'Answer'])
  )
  return(synd)
}

Model13_Sim <- function(par, data){

  #set up vectors
  tn       <- 18
  tn2      <- 54
  simA     <- rep(NA, tn2)
  pchoose1 <- c(simA, NA)
  pchoose  <- c(simA, NA)
  ll       <- c(simA, NA)
  alpha    <- par[1]
  beta     <- par[2]
  tau      <- par[3]
  lrc_p    <- par[4]
  lrc_i    <- par[5]
  lrc_c    <- par[6]
  Answer   <- c(simA, NA)
  Q <- matrix(NA, 55, 4)
  Q[18,] <- c(0.33, 0.33, 0.33, 0)

  for (t in 1:tn){

    s1 = data[t, 2]/10;
    o1 = data[t, 3]/10;
    s2 = data[t, 4]/10;
    o2 = data[t, 5]/10;

    val1 = alpha * s1 + beta * max(s1-o1,0) # fehr-schmidt equation using either alpha as a matrix, or
    val2 = alpha * s2 + beta * max(s2-o2,0) # alpha as a specific value (see lines 97-103)

    sigmoid                = function(x){1/(1+exp(-x))}  # sigmoid
    pchoose1[t]            = sigmoid(val1 - val2); # probability of 1
    simA[t]                <- sample(c(1,2), 1, prob = c(pchoose1[t], 1-pchoose1[t]))

    answer = data[t,'Answer']
    if(answer == 1)
      pchoose[t] = pchoose1[t]
    else
      pchoose[t] = 1-pchoose1[t]

  }

  for (t in (tn+1):(tn2+1)) {

    choice = as.numeric(data[t-1,'GuessAction']);
    outcome= as.numeric(data[t-1,'Answer']);

    # copy forward action values to next trial
    Q[t,] = Q[t-1,]

    pe    = outcome - Q[t,choice]

    # update option chosen on this trial for next trial's choice
    if(choice == 1){
      Q[t,choice] = Q[t,choice] + (lrc_p * pe);
    }else if (choice == 2){
      Q[t,choice] = Q[t,choice] + (lrc_i * pe);
    }else if (choice == 3){
      Q[t,choice] = Q[t,choice] + (lrc_c * pe);
    }

    prob      = pGibbs(Q[t-1,1:3],tau)
    simA[t]   = sample(c(1, 2, 3), 1, prob = prob)
    pchoose[t]= max(prob)

  }

  synd <- data.frame(
    simA            = simA,
    prob            = pchoose,
    Op1SimPPT       = c(NA, data[,'Option1_PPT']),
    Op1SimPAR       = c(NA, data[,'Option1_Partner']),
    Op2SimPPT       = c(NA, data[,'Option2_PPT']),
    Op2SimPAR       = c(NA, data[,'Option2_Partner']),
    Answer          = c(NA, data[,'Answer'])
  )
  return(synd)
}

Model14_Sim <- function(par, data){

  #set up vectors
  tn       <- 18
  tn2      <- 54
  simA     <- rep(NA, tn2)
  pchoose1 <- c(simA, NA)
  pchoose  <- c(simA, NA)
  ll       <- c(simA, NA)
  alpha    <- par[1]
  beta     <- par[2]
  tau      <- par[3]
  lrc_p    <- par[4]
  lrc_i    <- par[5]
  lrc_c    <- par[6]
  Answer   <- c(simA, NA)
  Q <- matrix(NA, 55, 4)
  Q[18,] <- c(0.33, 0.33, 0.33, 0)

  for (t in 1:tn){

    s1 = data[t, 2]/10;
    o1 = data[t, 3]/10;
    s2 = data[t, 4]/10;
    o2 = data[t, 5]/10;

    val1 = alpha * s1 + beta * max(s1-o1,0) # fehr-schmidt equation using either alpha as a matrix, or
    val2 = alpha * s2 + beta * max(s2-o2,0) # alpha as a specific value (see lines 97-103)

    sigmoid                = function(x){1/(1+exp(-x))}  # sigmoid
    pchoose1[t]            = sigmoid(val1 - val2); # probability of 1
    simA[t]                <- sample(c(1,2), 1, prob = c(pchoose1[t], 1-pchoose1[t]))

    answer = data[t,'Answer']
    if(answer == 1)
      pchoose[t] = pchoose1[t]
    else
      pchoose[t] = 1-pchoose1[t]

  }

  for (t in (tn+1):(tn2+1)) {

    s1 = data[t, 2]/10;
    o1 = data[t, 3]/10;
    s2 = data[t, 4]/10;
    o2 = data[t, 5]/10;

    val1 = alpha * s1 + beta * max(s1-o1,0) # fehr-schmidt equation using either alpha as a matrix, or
    val2 = alpha * s2 + beta * max(s2-o2,0) # alpha as a specific value (see lines 97-103)

    choice = as.numeric(data[t-1,'GuessAction']);
    outcome= as.numeric(data[t-1,'Answer']);

    # copy forward action values to next trial
    Q[t,] = Q[t-1,]

    pe    = outcome - Q[t,choice]

    # update option chosen on this trial for next trial's choice
    if(choice == 1){
      Q[t,choice] = Q[t,choice] + (lrc_p * pe);
    }else if (choice == 2){
      Q[t,choice] = Q[t,choice] + (lrc_i * pe);
    }else if (choice == 3){
      Q[t,choice] = Q[t,choice] + (lrc_c * pe);
    }

    prob      = pGibbs(Q[t-1,1:3],tau)
    simA[t]   = sample(c(1, 2, 3), 1, prob = prob)
    pchoose[t]= max(prob)

  }

  synd <- data.frame(
    simA            = simA,
    prob            = pchoose,
    Op1SimPPT       = c(NA, data[,'Option1_PPT']),
    Op1SimPAR       = c(NA, data[,'Option1_Partner']),
    Op2SimPPT       = c(NA, data[,'Option2_PPT']),
    Op2SimPAR       = c(NA, data[,'Option2_Partner']),
    Answer          = c(NA, data[,'Answer'])
  )
  return(synd)
}

