#Intentions Game Master file
#Joe Barnby
#Joe.barnby@kcl.ac.uk | j.barnby@uq.edu.au

#rm(list=ls(all=T))

library(easystats)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(doParallel)
library(foreach)

source("/Volumes/GoogleDrive/My Drive/Dropbox/Dev/Functions.R")
source("/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/IntentRWModelCode.R")
source("/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/IntentionsAltModelsCode.R")
source("/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/gen_ut.R")

#load data ####

Intentions <- read.csv("/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/IntentionsClean.csv", na.strings = c("", "NA"))
Intentions_comp <- read.csv('/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/Intentions_Competitive.csv',na.strings = c("", "NA"))
Intentions_indi <- read.csv('/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/Intentions_Individualist.csv',na.strings = c("", "NA"))
Intentions_pros <- read.csv('/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/Intentions_Prosocial.csv',na.strings = c("", "NA"))

sortIndividualDF_choice <- function(x, PartnerType) {
  
  x %>%
    dplyr::select(Participant.Public.ID, Response, display, Correct, Option1_PPT, Option1_Partner, Option2_PPT, Option2_Partner, ANSWER, Type1, Type2, Difference1, Difference2) %>%
    filter(display == 'playerChoice',
           Response!= is.na(Response)) %>%
    rename(ID = Participant.Public.ID,
           Diff1 = Difference1,
           Diff2 = Difference2,
           Answer = ANSWER) %>%
    group_by(ID) %>%
    mutate(Trial = 1:18,
           Choice = ifelse(Response == "Option 1", Type1, Type2),
           Answer = ifelse(Answer == "Option 1", 1, 2),
           Response = ifelse(Response == "Option 1", 1, 2),
           ChoiceAction = recode(Choice, Prosocial = 1, Individual = 2, Competative = 3)) %>%
    dplyr::select(ID, Trial, Response, display, Correct, Answer, Option1_PPT, Option1_Partner, Option2_PPT, Option2_Partner, Type1, Type2, Diff1, Diff2, Choice, ChoiceAction)
}

sortIndividualDF <- function(x, PartnerType) {
  
  x %>%
    dplyr::select(Participant.Public.ID, Response, display, Correct, ANSWER, Option1_PPT, Option1_Partner, Option2_PPT, Option2_Partner,Type1, Type2, Difference1, Difference2) %>%
    filter(display == 'playerGuess',
           Response!= is.na(Response)) %>%
    rename(ID = Participant.Public.ID,
           Diff1 = Difference1,
           Diff2 = Difference2,
           Answer = ANSWER) %>%
    group_by(ID) %>%
    mutate(Trial = 1:36,
           Guess = ifelse(Response == "Option 1", Type1, Type2),
           Answer = ifelse(Answer == "Option 1", 1, 2),
           Real_Answer = ifelse(Answer == "Option 1", 1, 2),
           Response = ifelse(Response == "Option 1", 1, 2),
           Partner = PartnerType,
           GuessAction = recode(Guess, Prosocial = 1, Individual = 2, Competative = 3)) %>%
    dplyr::select(ID, Trial, Partner, Response, display, Correct, Answer, Option1_PPT, Option1_Partner, Option2_PPT, Option2_Partner, Type1, Type2, Diff1, Diff2, Guess, GuessAction)
  
}

Intentions_compG <- sortIndividualDF(x = Intentions_comp, 'Competitive')
Intentions_indiG <- sortIndividualDF(x = Intentions_indi, 'Individualist')
Intentions_prosG <- sortIndividualDF(x = Intentions_pros, 'Prosocial')
Intentions_compC <- sortIndividualDF_choice(x = Intentions_comp)
Intentions_indiC <- sortIndividualDF_choice(x = Intentions_indi)
Intentions_prosC <- sortIndividualDF_choice(x = Intentions_pros)

Intentions_pts <- rbind(Intentions_compG, Intentions_indiG, Intentions_prosG) %>% arrange(ID)
Intentions_pts_choice <- rbind(Intentions_compC, Intentions_indiC, Intentions_prosC) %>% arrange(ID) %>% na.omit()

Intentions_guess <- plyr::join(Intentions %>% 
                                 filter(Game == "Guess") %>%
                                 dplyr::select(-Response, -Real_Answer), 
                               Intentions_pts %>% 
                                 dplyr::select(ID, Trial, Response, Answer,
                                               Option1_PPT, Option1_Partner, Option2_PPT, Option2_Partner,
                                               Type1, Type2, Diff1, Diff2, Guess, GuessAction), 
                               by = c("ID", "Trial"))
Intentions_choice <- plyr::join(Intentions %>% filter(Game == "Choose") %>% 
                                  dplyr::select(-Response), 
                                Intentions_pts_choice %>% 
                                  dplyr::select(ID, Trial, Response,Type1, Type2, 
                                                Option1_PPT, Option1_Partner, Option2_PPT, Option2_Partner, 
                                                Diff1, Diff2, Choice, ChoiceAction), 
                                by = c("ID", "Trial"))

Intentions_choice[which(is.na(Intentions_choice$Trial)),]
Intentions_choice <- Intentions_choice[which(Intentions_choice$ID != '5edf50e72ef80a1fe0267aeb'),]
Intentions_list_C <- split(Intentions_choice %>% arrange(ID), f = Intentions_choice$ID)
Intentions_guess <- Intentions_guess[which(Intentions_guess$ID != '5edf50e72ef80a1fe0267aeb'),]
Intentions_list_G <- split(Intentions_guess %>% arrange(ID), f = Intentions_guess$ID)

Intentions_choice <- transform(Intentions_choice,id=as.numeric(factor(Intentions_choice$ID)))
Intentions_guess <- transform(Intentions_guess,id=as.numeric(factor(Intentions_guess$ID)))

write.csv(Intentions_choice, '/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/Intentions_Phase1.csv')
write.csv(Intentions_guess, '/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/Intentions_Phase2.csv')

## List of CSV files for other software

Intentions_list_C <- split(Intentions_choice, Intentions_choice$ID)
Intentions_list_G <- split(Intentions_guess, Intentions_guess$ID)


registerDoParallel(cores = 6) #choose how many cores in your processor to use for the rest of the script


# Reduce local minima -----------------------------------------------------

SANNPhase1_H <- foreach(pt=1:length(Intentions_list_C), .combine = rbind) %dopar% { # multicore function for efficiency
  
  tryP                              <- as.numeric(c(0.5, 0.5));
  data                              <- Intentions_list_C[[pt]]
  
  fitAttempt                        <- NA; # clear the decks
  try(fitAttempt                    <- optim(fn = wrapper_PhaseOne_Heuristic, #change to WSLS for last 4 parm fit
                                             par = tryP, 
                                             data = data,
                                             scbeta0 = -1,
                                             method = 'SANN'
  ))
  
  loglik                            <- NA; 
  try( loglik                       <- -wrapper_PhaseOne_Heuristic(fitAttempt$par, 
                                                                   data, 
                                                                   scbeta = NA));
  
  data.frame(
    ID     = Intentions_list_C[[pt]][1,'ID'],
    alpha  = fitAttempt$par[1],
    beta   = fitAttempt$par[2],
    lp     = -fitAttempt$value,
    ll     = loglik,
    Persec = Intentions_list_C[[pt]][1,'Persec'],
    ICAR   = Intentions_list_C[[pt]][1,'ICARTot']
  )
}

# Optimise Parameters -----------------------------------------------------

OptimPhase1 <- foreach(pt=1:length(Intentions_list_C), .combine = rbind) %dopar% { # multicore function for efficiency
  
  tryP                              <- c(1,1) #SANNPhase1[pt,2:4];
  data                              <- Intentions_list_C[[pt]]
  
  try(fitAttempt                    <- optim(fn = wrapper_PhaseOne_Heuristic, 
                                             par = tryP, 
                                             data = data,
                                             scbeta0 = -1)
  )
  
  loglik                            <- NA; 
  try( loglik                       <- Intentions_PhaseOne_Heuristic(fitAttempt$par, 
                                                          data, 
                                                          detail = F));
  data.frame(
    ID     = Intentions_list_C[[pt]][1,'ID'],
    alpha  = fitAttempt$par[1],
    beta   = fitAttempt$par[2],
    lp     = -fitAttempt$value,
    ll     = loglik,
    Persec = Intentions_list_C[[pt]][1,'Persec'],
    ICAR   = Intentions_list_C[[pt]][1,'ICARTot']
  )
}

ggplot(OptimPhase1 %>% 
         pivot_longer(2:3, names_to = 'Parameter', values_to = 'Metric') %>%
         mutate(Persec = ifelse(Persec > 3.66, "High", "Low")))+
  geom_jitter (aes(Parameter, Metric, color = Persec))+
  geom_boxplot(aes(Parameter, Metric, fill = Persec), color = 'black', outlier.shape = NA)+
  ggpubr::stat_compare_means(aes(Parameter, Metric, group = Persec))+
  tidybayes::theme_tidybayes() +
  
ggplot(OptimPhase1)+
  geom_density(aes(-ll))


# Recovery Heuristic ------------------------------------------------------

simulatedHeur <- list()
testDat  <- Intentions_list_C[[1]] %>% dplyr::select(Trial, Option1_PPT, Option1_Partner, Option2_PPT, Option2_Partner, Response)

mysamp <- function(n, m, s, lwr, upr, nnorm) {
  samp <- rnorm(nnorm, m, s)
  samp <- samp[samp >= lwr & samp <= upr]
  if (length(samp) >= n) {
    return(sample(samp, n))
  }  
  stop(simpleError("Not enough values to sample from. Try increasing nnorm."))
}

simulatedHeur[[i]] <-  foreach(i = 1:1000, .combine = rbind) %dopar% {
  
  genpar <- c(mysamp(1, 0, 3, -10, 10, 1000), mysamp(1, 0, 3, -10, 10, 1000))
  data = as.data.frame(Intentions_list_C[[1]][,c('Response', 'Option1_PPT', 'Option1_Partner', 'Option2_PPT', 'Option2_Partner', 'Trial')])
  synD <- Phase1Sim(genpar, rbind(data)) #generate synthetic data
  synD$Trial <- 1:18
  tn = length(synD$Trial)+1
  colnames(synD) <- c('Response', 'prob1', 'Option1_PPT', 'Option1_Partner', 'Option2_PPT', 'Option2_Partner', 'Trial')
  
  fitAttempt <- optim(fn = wrapper_PhaseOne_Heuristic, 
                      par = tryP, 
                      data = synD,
                      scbeta0 = -1)
  loglik     <- NA; 
  try( loglik<- Intentions_PhaseOne_Heuristic(fitAttempt$par, 
                                              synD, 
                                              detail = F));  
  data.frame(
    alphaM    = genpar[1],
    betaM     = genpar[2],
    alphaMrec = fitAttempt$par[1],
    betaMrec  = fitAttempt$par[2],
    ll        = loglik,
    pt        = i
  )
}

simulatedHeur_DF <- do.call(rbind, simulatedHeur)

MEM <- ggplot(simulatedHeur_DF) + 
  geom_jitter(aes(alphaM, alphaMrec), color = 'pink', alpha = 0.5)+
  geom_smooth(aes(alphaM, alphaMrec), color = 'red', method = 'lm')+
  geom_abline(intercept = 0, slope = 1,  alpha = 0.5)+
  coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10))+
  labs(subtitle = expression(paste(alpha)), x = 'Fit Parameter', y = 'Recovered Parameter')+
  tidybayes::theme_tidybayes()+theme(title = element_text(size = 18))+
ggplot(simulatedHeur_DF) + 
  geom_jitter(aes(betaM, betaMrec), color = 'light blue', alpha = 0.5)+
  geom_smooth(aes(betaM, betaMrec), color = 'blue', method = 'lm')+
  geom_abline(intercept = 0, slope = 1,  alpha = 0.5)+
  coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10))+
  labs(subtitle = expression(paste(beta)), x = 'Fit Parameter', y = 'Recovered Parameter')+
  tidybayes::theme_tidybayes()+theme(title = element_text(size = 18))+
  
  patchwork::plot_layout(ncol = 2) + patchwork::plot_annotation(title = 'Mixed Effects Model')


# Mixed Effect Model HBI --------------------------------------------------
library(R2OpenBUGS)
library(rjags)
library(coda)
library(MCMCvis)
library(rstan)
library(rstanarm)
library(hBayesDM)

rstan_options(auto_write = T)
options(mc.cores = 6)

#Test the model with 10 random synthetic samples
dat <- list()
n = 50
t = 18
alpha = 2
beta  = -2
for (i in 1:n){
dat[[i]] <- Phase1Sim(c(alpha, beta), rbind(Intentions_list_C[[1]], Intentions_list_C[[1]]))
}
dat <- do.call(rbind, dat)
testDat <- list(N = n, T = t, C = 1, Tsubj = rep(t, n))
testDat$s1     <- matrix(data = dat$Op1SimPPT, nrow = testDat$N, ncol = testDat$T, byrow = T)
testDat$o1     <- matrix(data = dat$Op1SimPAR, nrow = testDat$N, ncol = testDat$T, byrow = T)
testDat$s2     <- matrix(data = dat$Op2SimPPT, nrow = testDat$N, ncol = testDat$T, byrow = T)
testDat$o2     <- matrix(data = dat$Op2SimPAR, nrow = testDat$N, ncol = testDat$T, byrow = T)
testDat$choice <- matrix(data = dat$simA,      nrow = testDat$N, ncol = testDat$T, byrow = T)
testDat$choice <- ifelse(testDat$choice == 1, 0, 1)

stanMod   <- '/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/StanFSIAmodel.stan'
nIter     <- 4000
nWarmup   <- floor(nIter/2)
nThin     <- 1
nChains   <- 4

fittest <- stan(
  file = stanMod,
  data = testDat,
  chains = nChains,
  warmup = nWarmup,
  iter = nIter,            
  seed = 13346563,
  init = 'random'
)  

plot(fittest, type="trace", fontSize=11, par = c('alpha', 'beta', 'IT'))
bayesplot::mcmc_trace(x = fittest, pars = c('alpha', 'beta', 'IT'))
print(fittest)
plot(fittest, type="trace", fontSize=11, par = c('log_lik'))

drawTest <- rstan::extract(fittest)
matTest  <- as.data.frame(fittest)
print(names(drawTest))
print(names(matTest))

colSums(drawTest$log_lik)/6000

ggplot(matTest %>% pivot_longer(`alpha[1]`:`alpha[20]`, names_to = 'PPT', values_to = 'Draws'))+
  geom_density(aes(Draws, group = PPT), color = 'grey')
ggplot(matTest %>% pivot_longer(`log_lik[1]`:`log_lik[20]`, names_to = 'log_lik', values_to = 'samples'))+
  geom_density(aes(samples, group = log_lik), color = 'grey')
ggplot(matTest %>% pivot_longer(`lp__[1]`:`lp__[20]`, names_to = 'log_lik', values_to = 'samples'))+
  geom_density(aes(samples, group = log_lik), color = 'grey')

#Fit the model with the full data 

RealDat <- list(N = 697, T = 18, C = 1, Tsubj = rep(18, 697))
RealDat$s1     <- matrix(data = Intentions_choice$Option1_PPT,     nrow = RealDat$N, ncol = RealDat$T, byrow = T)
RealDat$o1     <- matrix(data = Intentions_choice$Option1_Partner, nrow = RealDat$N, ncol = RealDat$T, byrow = T)
RealDat$s2     <- matrix(data = Intentions_choice$Option2_PPT,     nrow = RealDat$N, ncol = RealDat$T, byrow = T)
RealDat$o2     <- matrix(data = Intentions_choice$Option2_Partner, nrow = RealDat$N, ncol = RealDat$T, byrow = T)
RealDat$choice <- matrix(data = Intentions_choice$Response,        nrow = RealDat$N, ncol = RealDat$T, byrow = T)
RealDat$choice <- ifelse(RealDat$choice == 1, 0, 1)

stanMod   <- '/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/StanFSIAmodel.stan'
nIter     <- 4000
nWarmup   <- floor(nIter/2)
nThin     <- 1
nChains   <- 4

fit1 <- stan(
  file = stanMod,
  data = RealDat,
  chains = nChains,
  warmup = nWarmup,
  iter = nIter,            
  seed = 13346563,
  init = 'random'
)

plot(fit1, type="trace", fontSize=11, par = c('mu_alpha', 'mu_beta', 'mu_IT'))
bayesplot::mcmc_trace(x = fit1, pars = c('mu_alpha', 'mu_beta', 'mu_IT'))
summary(fit1)
plot(fit1, type="trace", fontSize=11, par = c('log_lik'))

drawTest1 <- rstan::extract(fit1)
matTest1  <- as.data.frame(fit1)
print(names(drawTest1))
print(colnames(matTest1))
ggplot(matTest1 %>% 
         dplyr::select(`beta[1]`:`beta[697]`)%>% 
         pivot_longer(`beta[1]`:`beta[697]`, names_to = 'PPT', values_to = 'Draws'))+
  geom_density(aes(Draws, group = PPT), color = 'grey')
ggplot(matTest1 %>% 
         dplyr::select(`mu_p[1]`:`mu_p[3]`) %>%
         pivot_longer(`mu_p[1]`:`mu_p[3]`, names_to = 'PPT', values_to = 'Draws'))+
  geom_density(aes(Draws, group = PPT, color = PPT))
  
# Recovery Bayes -------------------------------------------------------------

res      <- 81; #resolution of posterior
testDat  <- Intentions_list_C[[1]] %>% dplyr::select(Trial, Option1_PPT, Option1_Partner, Option2_PPT, Option2_Partner, Response)
#simulatedI18 <- list()
#simulatedI36 <- list()
#simulatedI54 <- list()
#simulatedI72 <- list()

simulatedI18[[i]] <-  foreach(i = 1:1000, .combine = rbind) %dopar% {
  
  genpar <- c(mysamp(1, 5, 3, 0, 10, 1000), mysamp(1, 0, 3, -10, 10, 1000))
  data = as.data.frame(Intentions_list_C[[1]][,c('Response', 'Option1_PPT', 'Option1_Partner', 'Option2_PPT', 'Option2_Partner', 'Trial')])
  synD <- Phase1Sim(genpar, rbind(data)) #generate synthetic data
  synD$Trial <- 1:18
  tn = length(synD$Trial)+1
  colnames(synD) <- c('Response', 'prob1', 'Option1_PPT', 'Option1_Partner', 'Option2_PPT', 'Option2_Partner', 'Trial')

  output <- Intentions_PhaseOne_Bayes(genpar, data = synD, detail = T, plot = F)

  data.frame(
    prior     = seq(-10, 10, length.out = res),
    alphaM    = rep(genpar[1], res),
    betaM     = rep(genpar[2], res),
    alphaMrec = output$alpha_marginal,
    betaMrec  = output$beta_marginal,
    MAPArec   = output$MAP_A,
    MAPBrec   = output$MAP_B,
    pchooseFit= c(NA, synD$prob, rep(NA, res-tn)),
    pt        = rep(i,res),
    sumll     = output$sumll,
    prob      = output$pab_norm
  )
}

simulated_DF18    <- do.call(rbind, simulatedI18);simulated_DF18$DF <- '18'
simulated_DF36    <- do.call(rbind, simulatedI36);simulated_DF36$DF <- '36'
simulated_DF54    <- do.call(rbind, simulatedI54);simulated_DF54$DF <- '54'
simulated_DF72    <- do.call(rbind, simulatedI72);simulated_DF72$DF <- '72'
simulated_DF      <- rbind(simulated_DF18, simulated_DF36, simulated_DF54, simulated_DF72)

BM <- ggplot(simulated_DF18) + 
  geom_jitter(aes(alphaM, MAPArec), color = 'pink', alpha = 0.1)+
  geom_smooth(aes(alphaM, MAPArec), color = 'red', method = 'lm')+
  geom_abline(intercept = 0, slope = 1,  alpha = 0.5)+
  coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10))+
  #facet_wrap(~ DF, ncol = 4)+
  labs(subtitle = expression(paste(alpha)), x = 'Fit Parameter', y = 'Recovered Parameter')+
  tidybayes::theme_tidybayes()+theme(title = element_text(size = 18))+
ggplot(simulated_DF18) + 
  geom_jitter(aes(betaM, MAPBrec), color = 'light blue', alpha = 0.1)+
  geom_smooth(aes(betaM, MAPBrec), color = 'blue', method = 'lm')+
  geom_abline(intercept = 0, slope = 1,  alpha = 0.5)+
  coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10))+
  #facet_wrap(~ DF, ncol = 4)+
  labs(subtitle = expression(paste(beta)), x = 'Fit Parameter', y = 'Recovered Parameter')+
  tidybayes::theme_tidybayes()+theme(title = element_text(size = 18))+
  
  patchwork::plot_layout(ncol = 2) + patchwork::plot_annotation(title = "Bayesian Probabilistic Model")


# Model Comparison --------------------------------------------------------

LL <- ggplot()+
  geom_density( data = simulated_DF18, aes(sumll))+
  geom_vline(xintercept = log(0.5)*18)+
  coord_cartesian(xlim = c(-15, 0))+
ggplot()+
  geom_density( data = simulatedHeur_DF, aes(ll))+
  geom_vline(xintercept = log(0.5)*18)+
  coord_cartesian(xlim = c(-15, 0))+
  patchwork::plot_layout(ncol = 1) & tidybayes::theme_tidybayes()
  
(BM / MEM) | LL

# Test Phase 1 Peter Model ------------------------------------------------
  
registerDoParallel(cores = 6)
ptN = length(Intentions_list_C)
#Test data 

Phase1DF <- foreach(pt=1:ptN, .combine = rbind) %dopar% {
  
  res = 81
  tryP                              <- c(0, 0, 2, 2)
  data                              <- Intentions_list_C[[pt]]

  try(output                        <- Intentions_PhaseOne_Peter(
                                               par = tryP,
                                               data = data,
                                               detail = T))
  data.frame(
    ID            = rep(Intentions_list_C[[pt]][1,'ID'], res),
    prior         = seq(from = -10, to = 10, by = 0.25),
    alpha_M       = output$alpha_marginal,
    beta_M        = output$beta_marginal,
    Action        = output$Action,
    simA          = output$simA,
    ll            = output$ll,
    sumll         = output$sumll,
    pab_norm      = output$pab_norm,
    Persec        = rep(Intentions_list_C[[pt]][1,'Persec'], res)
  )
} 

Phase1DF %>% 
  group_by(ID) %>% 
  mutate(
         Persec_level = ifelse(Persec > 3.66, 'High', 'Low')) %>%
  pivot_longer(3:4, names_to = 'Parameter', values_to = 'Value')  %>% 
  ggplot() + 
  geom_vline(xintercept = 0)+
  stat_summary(aes(prior, Value, color = Parameter), geom = 'line')+ 
  stat_summary(aes(prior, Value*900, color = Parameter), geom = 'line')+
  geom_bar(aes(prior, Value, fill = Parameter ), stat = 'identity', alpha = 0.5)+
  scale_fill_discrete( name = "Parameter", labels = c(expression(alpha), expression(beta)))+
  scale_color_discrete(name = "Parameter", labels = c(expression(alpha), expression(beta)))+
  labs(x = 'Group Level Posterior Distributions', y = "Parameter Density")+
  tidybayes::theme_tidybayes()+
  theme(legend.text = element_text(size = 12), legend.position = c(0.8, 0.8)) +
  
Phase1DF %>%
  dplyr::select(sumll, ID) %>%
  ggplot() +
  geom_density(aes(sumll))+
  geom_vline(xintercept = log(0.5)*18)

# Test Phase 2 Peter Experiment -------------------------------------------

Phase1Test_list <- split(Phase1Test %>% arrange(ID), Phase1Test$ID)
ptN = 20
Phase2Test <- foreach(pt=1:ptN, .combine = rbind) %dopar% {
  
  res = 61
  
  tryP                              <- 0.5
  data                              <- Intentions_list_G[[pt]]
  
  try(fitAttempt                     <- optim(Intentions_PhaseTwo_Peter,
                                             par = tryP,
                                             data = data,
                                             pri = pri,
                                             detail = F, 
                                             method = 'Brent', 
                                             lower = 0, 
                                             upper = 1))
  
  output                            <- NA; 
  try( output                       <- Intentions_PhaseTwo_Peter(
                                        par = fitAttempt$par,
                                        pri = pri,
                                        data = data,
                                        detail = T))
  
  pars_alpha                        <- try(egamma(output$alpha_marginal))
  pars_beta                         <- try(egamma(output$beta_marginal))
  pars_gamma                        <- try(egamma(output$gamma_marginal))
  
  data.frame(
    ID        = rep(Intentions_list_C[[pt]][1,'ID'],res),
    alpha_M   = output$alpha_marginal,
    beta_M    = output$beta_marginal,
    gamma_M   = output$gamma_marginal,
    zeta      = rep(fitAttempt$par[1],res),
    alpha_sha = rep(pars_alpha$parameters[1],res),
    alpha_sca = rep(pars_alpha$parameters[2],res),
    beta_sha  = rep(pars_beta$parameters[1],res),
    beta_sca  = rep(pars_beta$parameters[2],res),
    gamma_sha = rep(pars_gamma$parameters[1],res),
    gamma_sca = rep(pars_gamma$parameters[2],res),
    #simA      = output$simA,
    #Action    = output$Action,
    Persec    = rep(Intentions_list_G[[pt]][1,'Persec'], res),
    Policy    = rep(Intentions_list_G[[pt]][1,'PartnerPolicy'], res)
    
  )
}


# Combine and visualise ---------------------------------------------------

Phase1Test$Phase <- 1; Phase1Test$Policy<- NA; Phase1Test$zeta <- NA
Phase2Test$Phase <- 2; 
Phase1Test <- Phase1Test %>% dplyr::select(names(Phase2Test))
IDs <- intersect(Phase1Test$ID, Phase2Test$ID)

phase_complete <- rbind(Phase1Test[Phase1Test$ID %in% IDs, ], Phase2Test)

phase_complete %>% 
  group_by(ID) %>% 
  mutate(prior = rep(seq(from = 0, to = 15, by = 0.25),2),
         Persec_level = ifelse(Persec > 3.66, 'High', 'Low')) %>% 
  ungroup()%>%
  ggplot() + 
  stat_summary(aes(prior, alpha_M, fill = as.factor(Persec_level)), geom = 'ribbon', alpha = 0.1)+
  stat_summary(aes(prior, alpha_M, color =as.factor(Persec_level)), geom = 'line')+
  scale_color_brewer(palette = 'Dark2')+
  facet_wrap(~Phase)+
  labs(x = 'Group Level Posterior Distributions')+
  tidybayes::theme_tidybayes()


# Save Image --------------------------------------------------------------

save.image(file = '/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/IntentionsGameAnalysis.RData')

