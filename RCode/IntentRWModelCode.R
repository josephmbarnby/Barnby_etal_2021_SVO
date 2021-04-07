# Rescorla-Wagner and related models for reversal learning.
# Joe Barnby 
# J.barnby@uq.edu.au | joe.barnby@kcl.ac.uk

# Simple RW model ---------------------------------------------------------

# some utilities useful for RW and related modelling:
try(source("/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/gen_ut.R"))

### test line  and plotting inputted data:
#  test <- reversal_RW(c(1,0.2,0.5),toyD); plot(test[,6],t='l',ylim=c(-5.1,10.1),ylab='Q values',xlab='trial',main='Inputed data: purple=action, gold=outcome'); lines(test[,7],col='blue'); lines(test[,2],t='p',col='purple'); lines(test[,3],t='p',col='gold4'); lines(test[,8],col='green'); View(test)
### test line for simulated data:
#  test <- reversal_RW(c(1,0.2,0.5),toyD); plot(test[,13],t='l',ylim=c(-5.1,10.1),ylab='Q values',xlab='trial',main='Simulated data: purple=action, gold=outcome \n black,blue,green=Q1,Q2,Q3'); lines(test[,14],col='blue'); lines(test[,16],t='p',col='purple'); lines(test[,15],t='l',col='green4'); lines(test[,17],t='p',col='gold3'); View(test)
### thest for win-stay lose-shift:
#  test <- reversal_RW(c(0.01,0.999,0.1,0,0.8,0.001),wslsD); plot(test[,6],t='l',ylim=c(-5.1,10.1),ylab='Q values',xlab='trial',main='wStayLshift data: purple=action, gold=outcome, pink=lapses'); lines(test[,7],col='blue'); lines(test[,2],t='p',col='purple'); lines(test[,3],t='p',col='gold4'); lines(test[,8],col='green');  abline(v=12,col='pink3'); abline(v=27,col='pink3'); abline(v=36,col='pink3'); abline(v=46,col='pink3'); abline(v=51,col='pink3'); abline(v=30.5,col='black',lwd=1)

Intentions_RW <- function(par, data, partner, detail = T) {
  
  # argument checks :
  par <- as.numeric(par); 
  if (length(par) <3){ par[3] <- 1e-6; }; # if no Pierce-Hall salience parameter, set it to almost 0
  if (length(par) <4){ par[4] <- 1-1e-6; }; # if no memory parameter provided, set it to almost 1.
  if (length(par) <5){ par[5] <- 1e-6; }; # if no lapse param (zet) provided, set it to almost 0.
  # unchosen options.
  
  #Trial Number
  t_n <- 37
  
  #Parameters
  tau   <-  par[1] # temperature  
  lrc   <-  par[2] # learning rate
  sal   <-  par[3] # salience (Pierce-hall modification)
  mem   <-  par[4] # memory
  zet   <-  par[5] # lapse parameter
  
  #Salience matrix to load salience parameter calculations
  salience <- matrix(
    NA,
    nrow = 37, 
    ncol = 4) #empty matrix for salience loop
  colnames(salience) <- c("Q1", "Q2", "Q3", "Trial")
  
  salience[1, 1:4] <- c(rep(1, 3),0)
  salience[2:37,4] <- 1:36
  
  #Q matrix to store Q values
  q <- matrix(
    rep(
      rep(0, 37),
      4), 
    nrow = 37, 
    ncol = 4) #empty matrix for loop
  colnames(q) <- c("Q1", "Q2", "Q3", "Trial")
  
  q[1,] <- c(0.33, 0.33, 0.33, 0) #set priors based on average of 1 and 0
  q[,4] <- 0:(t_n-1)
  
  #Loglikelihood initialise
  l <- rep(NA, 36) # initialise log likelihood
  
  #Pe initialise
  pe <- rep(NA, 36) # initialise pe
  
  #PeSim initialise
  peSim <- rep(NA, 36) # initialise peSim
  
  #win switching vector
  ws <- rep(NA, 37)
  
  #lose stay vector
  ls <- rep(NA, 37)
  
  #reward vector
  r    <- rep(NA, 37)
  r[1] <- 0 #initialise reward
  
  #action vector
  action <- rep(NA, 37)
  action[1]<- 0 #initialise action
  
  #Learning vector
  learning <- rep(NA, 37)
  
  if (detail == T){   # make space for the gory detail variables
    #Simulated behavioural data initialised parameters
    simD <- l
    simP <- l
    simQ <- q
    simR <- l;     # will hold simulated outcome
    simS <- salience
    simL <- learning
    retp <- matrix(
      c(
        0.666, 0.166, 0.166, 0.166, 0.666, 0.166, 0.166, 0.166, 0.666
      ),
      3,3)   # prob of decision per partner
  }
  
  # Main loop over trials, for learning, 
  # reversal, and likelihood estimates
  for (t in 2:t_n) { 
    
    ## Learning block with RW equation ##
    
    #Prediction error calculation  
    action[t]   <- data[t-1, 2] #store which action was taken
    r[t]        <- data[t-1, 3] #store the reward achieved
    pe[t]       <- r[t] - q[t-1, action[t]] #calculate the pe based on prior expectation
    
    #Salience and learning estimates
    salience[t, action[t]]          <- (sal * abs(pe[t])) + ((1-sal) * salience[t-1,action[t]]) #Salience modifier of card
    salience[t, c(-action[t], -4)]  <- salience[t-1, c(-action[t], -4)] #update other rows with t-1 salience
    learning[t]                     <- lrc * salience[t,action[t]] #learning rate based on lrc and salience modifier
    
    if(learning[t] > 1) {
      learning[t] <- 0.99999
    }
    if(learning[t] < 0) {
      learning[t] <- 0.00001
    }
    
    #Q updating
    q[t,1:3]                        <- q[t-1, 1:3] #update trial t of Q with priors
    q[t,action[t]]                  <- q[t,action[t]] + (learning[t] * pe[t]) #update Q with learning plus salience
    q[t, c(-action[t], -4)]         <- 0.5 - (mem * (0.5 - q[t-1, c(-action[t], -4)])) #decay non chosen option by value of mem
    
    #likelihood equation
    Pmotiv          <- pGibbs(q[t-1,1:3],tau,action[t]); # (exp(q[t-1, action]/tau))/sum(exp(q[t-1,1:3]/tau)) #softmax function
    Pr              <- (zet/3) + ((1-zet) * Pmotiv)
    l[t-1]          <- log(Pr)
    
    #calculate win-switch behaviour
    
    if (action[t] != action[t-1] & r[t-1] == 1){
      ws[t] <- 1
    } else {
      ws[t] <- 0
    }
    
    #calculate lose-stay behaviour
    if (action[t] == action[t-1] & r[t-1] == 0){
      ls[t] <- 1
    } else {
      ls[t] <- 0
    }
    
    #Generate simulated data in parralell
    if (detail == T) {
      
      simPmotiv              <- pGibbs(simQ[t-1,1:3],tau); #(exp(simQ[t-1, 1:3]/tau))/sum(exp(simQ[t-1,1:3]/tau))
      Pi                     <- (zet/3) + ((1-zet) * simPmotiv)
      simA                   <- sample(c(1, 2, 3), 1, prob = Pi)  # simulated action (to avoid unreferencing)
      simD[t]                <- simA                              # ... store it as decision
      simP[t]                <- Pi[simA]                          # to compare w. experimental in due
      
      if(partner == 'Prosocial')    {simR[t] <- sample(c(0.66,0.16,0.16),1,prob=retp[simA,])}
      if(partner == 'Individualist'){simR[t] <- sample(c(0.16,0.66,0.16),1,prob=retp[simA,])}
      if(partner == 'Competative')  {simR[t] <- sample(c(0.16,0.16,0.66),1,prob=retp[simA,])}
      
      # simulated outcome
     
      peSim[t]               <- simR[t] - simQ[t-1,simA]
      
      simS[t, simA]          <- (sal * abs(peSim[t])) + ((1-sal) * simS[t-1, simA]) #Salience modifier of card
      simS[t, c(-simA, -4)]  <- simS[t-1, c(-simA, -4)] #update other rows with t-1 salience
      simL[t]                <- lrc * simS[t,simA] #learning rate based on lrc and salience modifier
      
      if(simL[t] > 1) {
        simL[t] <- 0.99999
      }
      if(simL[t] < 0) {
        simL[t] <- 0.00001
      }
      
      simQ[t,1:3]              <- simQ[t-1, 1:3] #set priors for Q using t-1
      simQ[t,simA]             <- simQ[t,simA] + simL[t] * peSim[t] #update Q at trial t
      simQ[t, c(-simA, -4)]    <- 0.5 - (mem * (0.5 - simQ[t-1, c(-simA, -4)])) #decay non chosen option by value of mem
      
    }
    
  }
  
  #Save outcomes of learning loops into an output
  if (detail == F) {
    
    return(sum(l))
    
  } else {
    
    output <- matrix(
      NA,
      nrow = 37,
      ncol = 3 + 2 + 3 + 2 + 2 + 6
    )
    
    colnames(output) <- c("Trial", "Action", "Outcome", 
                          "PE", "Learning",
                          "Q1", "Q2", "Q3", 
                          "ll", "PEsim", 
                          "ws", "ls",
                          "simQ1","simQ2","simQ3","simD", "simR", "simP")
    
    output[2:37, 1:3] <- as.matrix(data[,1:3])
    output[, 4]       <- pe
    output[2:37, 5]   <- learning[2:37]
    output[, 6:8]     <- q[,1:3] #Q learning
    output[2:37, 9]   <- l       #loglikelihood
    output[, 10]      <- peSim
    output[, 11]      <- ws      #win switch
    output[, 12]      <- ls      #lose-stay
    output[, 13:18]   <- cbind(simQ[,1:3],simD, simR, simP) #simulated outcomes
    
    return(output)
    
  }
  
} # end of function

wrapper_Intentions_RW <- function(par, data, partner, scbeta0=-1,check=0){
  
  parM <- as.numeric(par); # in case it's inputed in another format
  parN <- length(parM)
  maxp <- c(20,rep(1,(parN-1)))  # upper boundaries for params  MAY NEED ADJUSTMENT
  minp <- rep(0,parN)            # lower boundaries for params  MAY NEED ADJUSTMENT
  
  if ((scbeta0[1] < 0) && !is.na(scbeta0)){ 
    # i.e. a number, but not a valid scaled distr. param.,
    # which means 'use default, weak regularizing priors'
    #  For:     tau lrc sal mem zet
    scale1 <- c(1.1,1.5,1.25,1.1,1.05);
    scale2 <- c(10 ,2.5,1.25,1.1,2.95);
    mp     <- rep(0,5);
    Mp     <- c(20,rep(1,4))
    scbeta0 <- t(matrix(c(scale1,scale2,mp,Mp),5,4))
    if(check){
      colnames(scbeta0) <- c('tau','lrc','sal','mem','zeta')
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
    return(mRWPrior - Intentions_RW(par,data, partner,detail = F))
  }
  
  
} # end of wrapper_RW

wrapper_Intentions_WSLS <- function(par, data, partner, scbeta0=-1,check=0){

  parM <- as.numeric(par); # in case it's inputed in another format
  parM <- c(0.001, 0.999, parM) # add fixed parameters
  parN <- length(parM)-2
  maxp <- rep(1,parN)  # upper boundaries for params  MAY NEED ADJUSTMENT
  minp <- rep(0,parN)            # lower boundaries for params  MAY NEED ADJUSTMENT
  
  if ((scbeta0[1] < 0) && !is.na(scbeta0)){ 
    # i.e. a number, but not a valid scaled distr. param.,
    # which means 'use default, weak regularizing priors'
    #  For:     tau lrc sal 
    scale1 <- c(1.1,1.25,1.05);
    scale2 <- c(1.1,1.25,2.95);
    mp     <- rep(0,3);
    Mp     <- rep(1,3);
    scbeta0 <- t(matrix(c(scale1,scale2,mp,Mp),3,4))
    if(check){
      colnames(scbeta0) <- c('tau', 'lrc', 'sal')
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
      mRWPrior <-  - sum(dbetasc( parM[3], 
                                  scbeta0[1,3],scbeta0[2,3],
                                  scbeta0[3,3],scbeta0[4,3], log=TRUE)); 
    }
  }
  
  if (mRWPrior == Inf){  # If we are in an a priori prohibited parameter region
    # do not attempt to calculate the likelihood - it will be nonsense anyway.
    return(Inf); 
  } else {
    return(mRWPrior - Intentions_RW(parM,data, partner,detail = F))
  }
  
  
} # end of wrapper_RW
