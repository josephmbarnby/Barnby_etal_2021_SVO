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

Intentions <- read.csv("/Volumes/GoogleDrive/My Drive/Dropbox/PhD/MOBS/MOBS2/ONLINE DATA/IntentionsClean.csv", na.strings = c("", "NA"))
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

Intentions_choice <- transform(Intentions_choice %>% arrange(ID),id=as.numeric(factor(Intentions_choice$ID)))
Intentions_guess <- transform(Intentions_guess %>% arrange(ID),id=as.numeric(factor(Intentions_guess$ID)))
Intentions_both_matlab  <- rbind(Intentions_choice %>% 
                                 dplyr::select(1, 3, 7, 35, 38:41, 46, 5, 45) %>% 
                                 rename(Answer = Real_Answer,
                                        GuessAction = ChoiceAction),
                                 Intentions_guess %>%
                                 dplyr::select(1, 3, 35, 34, 36:39, 46, 5, 45) %>%
                                   group_by(ID) %>%
                                   mutate(Trial = 19:54)
                                 ) %>% arrange(ID) %>% mutate(Answer = ifelse(Trial == 1:18, NA, Answer)) %>%
  dplyr::select(9, 2, 5:8, 4, 3, 11, 10)

write.csv(Intentions_both_matlab, '/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/Intentions_BothPhase.csv')
write.csv(Intentions_choice, '/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/Intentions_Phase1.csv')
write.csv(Intentions_guess, '/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/Intentions_Phase2.csv')

## List of CSV files for other software

Intentions_list_C <- split(Intentions_choice, Intentions_choice$ID)
Intentions_list_G <- split(Intentions_guess, Intentions_guess$ID)


registerDoParallel(cores = 6) #choose how many cores in your processor to use for the rest of the script

# Simulated some data for matlab functions ------------------------------------------------------

simulatedHeur <- list()
simulatedDat  <- list()
testDat  <- Intentions_list_C[[1]] %>% dplyr::select(Trial, Option1_PPT, Option1_Partner, Option2_PPT, Option2_Partner, Response)

mysamp <- function(n, m, s, lwr, upr, nnorm) {
  samp <- rnorm(nnorm, m, s)
  samp <- samp[samp >= lwr & samp <= upr]
  if (length(samp) >= n) {
    return(sample(samp, n))
  }  
  stop(simpleError("Not enough values to sample from. Try increasing nnorm."))
}

simulatedDat1 <-  foreach(i = 1:1000, .combine = rbind) %dopar% {
genpar <- c(mysamp(1, 0, 5, 0, 15, 1000), mysamp(1, 0, 5, -10, 10, 1000))
data = as.data.frame(Intentions_list_G[[3]][1:18,c('Response', 'Option1_PPT', 'Option1_Partner', 'Option2_PPT', 'Option2_Partner', 'Trial', 'Answer')]) 
synD <- Phase1Sim(genpar, rbind(data)) #generate synthetic data
synD <- synD %>% mutate(prob2 = 1-prob,
                        Trial = 1:18,
                        Alpha = genpar[1],
                        Beta  = genpar[2],
                        PPT   = i,
                        Answer = as.numeric(Answer),
                        Correct = ifelse(simA == Answer, 1, 0),
                        SumCor  = sum(Correct)) %>%
  dplyr::select(simA, prob, prob2, 3:9, 11:16)
sapply(synD, as.numeric)
data.frame(
  synD
)
}

simulatedDat1

ggplot(simulatedDat1)+
  geom_jitter( aes(Alpha, SumCor), alpha = 0.1)+
  geom_smooth( aes(Alpha, SumCor))+
  coord_cartesian(ylim = c(0,18))+
  ggpubr::stat_cor(aes(Alpha, SumCor))

simulatedDat1 <- simulatedDat1 %>% dplyr::select(10, 11, 4:7, 1, 2, 3, 8, 9, 12, 13, 14, 15, 16)
write.csv(simulatedDat1, '/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/simulatedDat.csv')

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


                      # Go into MatLab scripts for computational #
                      # work completed using the CBM functions  ##


# Matlab data load --------------------------------------------

# load data from matlab
library(R.matlab)
RecData <- readMat('/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/hbi_12models.mat')
RecData_indiv <- readMat('/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/lap_BF6_ChoiceCong.mat')

# MAKE SURE TO CALCULATE HOW OFTEN CONGRUENT CHOICES LED TO IMPROVED PERFORMANCE#
# Total sum of congruent options ~ total score #
# Total sum of congruent options ~ total score * ParterPolicy #
# Total sum of congruent options ~ Persec + ICAR + Age + Sex #
# Stat_summary of congruent choices made for each trial * partnerpolicy #

RecData.frequency  <- RecData$cbm[,,1]$output[,,1]$model.frequency
RecData.exceed     <- RecData$cbm[,,1]$output[,,1]$exceedance.prob
RecData.p.exceed   <- t(c(0,0,1,0,0,0,0,0,0,0,0,0))#RecData$cbm[,,1]$output[,,1]$protected.exceedance.prob
RecData.parameters <- RecData$cbm[,,1]$output[,,1]$parameters
RecData.groupmeans <- RecData$cbm[,,1]$output[,,1]$group.mean
RecData.grouperror <- RecData$cbm[,,1]$output[,,1]$group.hierarchical.errorbar

RecData.groupmeans[[3]][[1]] <- as.data.frame(RecData.groupmeans[[3]][[1]])
RecData.frequency            <- as.data.frame(RecData.frequency)
RecData.exceed               <- as.data.frame(RecData.exceed)
RecData.p.exceed             <- as.data.frame(RecData.p.exceed)
colnames(RecData.frequency)  <- c('B_Fav', 'B_Action', 'B_Choice', 'B_6', 'B_shrink', 'B_zeta', 'B_4', 'B_2', 'RW', 'RW_3lrc', 'RW_2lrc', 'RW_Cong')
colnames(RecData.exceed)     <- c('B_Fav', 'B_Action', 'B_Choice', 'B_6', 'B_shrink', 'B_zeta', 'B_4', 'B_2', 'RW', 'RW_3lrc', 'RW_2lrc', 'RW_Cong')
colnames(RecData.p.exceed)   <- c('B_Fav', 'B_Action', 'B_Choice', 'B_6', 'B_shrink', 'B_zeta', 'B_4', 'B_2', 'RW', 'RW_3lrc', 'RW_2lrc', 'RW_Cong')
colnames(RecData.groupmeans[[3]][[1]]) <- c('alpha_m', 'beta_m', 'alpha_v', 'beta_v', 'episilon_c', 'epsilon_i')
colnames(RecData.parameters[[3]][[1]]) <- c('alpha_m', 'beta_m', 'alpha_v', 'beta_v', 'episilon_c', 'epsilon_i')
colnames(RecData.grouperror[[3]][[1]]) <- c('alpha_m', 'beta_m', 'alpha_v', 'beta_v', 'episilon_c', 'epsilon_i')
# Transform between 0 and positive integer
RecData.groupmeans[[3]][[1]][,1] <- 15*(1/(1+exp(-RecData.groupmeans[[3]][[1]][,1])))
RecData.groupmeans[[3]][[1]][,3] <- exp(RecData.groupmeans[[3]][[1]][,3])
RecData.groupmeans[[3]][[1]][,4] <- exp(RecData.groupmeans[[3]][[1]][,4])
RecData.groupmeans[[3]][[1]][,5] <- 1/(1+exp(-RecData.groupmeans[[3]][[1]][,5]))
RecData.groupmeans[[3]][[1]][,6] <- 1/(1+exp(-RecData.groupmeans[[3]][[1]][,6]))

RecData.parameters[[3]][[1]][,1] <- 15*(1/(1+exp(-RecData.parameters[[3]][[1]][,1])))
RecData.parameters[[3]][[1]][,3] <- exp(RecData.parameters[[3]][[1]][,3])
RecData.parameters[[3]][[1]][,4] <- exp(RecData.parameters[[3]][[1]][,4])
RecData.parameters[[3]][[1]][,5] <- 1/(1+exp(-RecData.parameters[[3]][[1]][,5]))
RecData.parameters[[3]][[1]][,6] <- 1/(1+exp(-RecData.parameters[[3]][[1]][,6]))

#augment data 
groupplot <- RecData.groupmeans[[3]][[1]] %>% 
                        dplyr::select(alpha_m, alpha_v, beta_m, beta_v, epsilon_c, epsilon_i) %>%
                        pivot_longer(1:6, names_to = 'parameter', values_to = 'value') %>%
                        mutate(sd = RecData.grouperror[[3]][[1]] %>%
                                 as.data.frame() %>%
                                 dplyr::select(alpha_m, alpha_v, beta_m, beta_v, epsilon_c, epsilon_i) %>%
                                 t(), 
                               type = c(rep('alpha', 2), rep('beta', 2), rep('epsilon', 2)),
                               metric = c('Mean', 'Var', 'Mean', 'Var', 'Mean', 'Var')) %>%
                        dplyr::rename(sd = 3)
RecData.frequency %>%
  pivot_longer(1:2, names_to = 'model', values_to = 'metric') %>%
  mutate(type = 'Freqency') -> freq
RecData.exceed %>%
  pivot_longer(1:2, names_to = 'model', values_to = 'metric') %>%
  mutate(type = 'Ex. Probability') -> exprob
RecData.p.exceed %>%
  pivot_longer(1:2, names_to = 'model', values_to = 'metric') %>%
  mutate(type = 'Ex. Probability (Protected)') -> p.exprob
Rec.data.modelcomp <- rbind(freq, exprob, p.exprob)

groupplot.parms1 <- RecData.parameters[[3]][[1]]%>% 
  as.data.frame() %>%
  mutate(id = 1:697) %>%
  dplyr::select(alpha_m, alpha_v, beta_m, beta_v, id) %>%
  pivot_longer(1:4, names_to = 'parameter', values_to = 'value') %>%
  mutate(type = ifelse(parameter %in% c('alpha_m', 'beta_m', 'epsilon_c'), 'Mean', 'Var'),
         parameter = ifelse(parameter %in% c('alpha_m', 'alpha_v'), 'alpha', 
                            ifelse(parameter %in% c('epsilon_c', 'epsilon_i'), 'epsilon', 'beta')))

groupplot.parms2 <- RecData.parameters[[3]][[1]]%>% 
  as.data.frame() %>%
  mutate(id = 1:697) 

Intentions_splice <- Intentions_guess %>% dplyr::select(
  id, PartnerPolicy, Control, Sum, Game, Persec, ICARTot, ComLadder, Age, HI, SI, Agency, Sex
) %>%
  distinct()

Intentions_splice <- plyr::join(Intentions_splice, Intentions_choice[,c(12,46)] %>% rename(Control2 = 'Control'), by = 'id')
Intentions_splice <- Intentions_splice %>%mutate(Control = Control + Control2)%>%select(1:13)

indivParms <- plyr::join(Intentions_splice %>% mutate(PartnerPolicy = ifelse(PartnerPolicy == "Competative", "Competitive", PartnerPolicy)), groupplot.parms1, by= 'id') %>% 
  distinct()
indivParmsB <- plyr::join(Intentions_splice %>% mutate(PartnerPolicy = ifelse(PartnerPolicy == "Competative", "Competitive", PartnerPolicy)), groupplot.parms2, by= 'id') %>%
  distinct()

write.csv(indivParmsB, '/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/IntentionsModelClean.csv')

# Core Models -------------------------------------------------------------
library(lme4)
#Total correct score by parameters and covariates
model.compare(lm(scale(Sum) ~ scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) + scale(epsilon_c) + scale(epsilon_i)+
                   scale(Persec) + scale(ICARTot) + scale(Age) + Control, 
                   data = indivParmsB,
                   na.action = na.fail))

#Persecutory Ideation by parameters and covariates
model.compare(lm(scale(Persec) ~ scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) + scale(epsilon_c) + scale(epsilon_i)+
                   scale(ICARTot) + scale(Age) + Control, 
                   data = indivParmsB,
                   na.action = na.fail))

#HI by parameters and covariates
model.compare(lm(scale(HI) ~ scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) + scale(epsilon_c) + scale(epsilon_i)+
                   scale(Persec) + scale(ICARTot) + scale(Age) + Control, 
                 data = indivParmsB,
                 na.action = na.fail))

#SI by parameters and covariates
model.compare(lm(scale(SI) ~ scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) + scale(epsilon_c) + scale(epsilon_i)+
                   scale(Persec) + scale(ICARTot) + scale(Age) + Control, 
                 data = indivParmsB,
                 na.action = na.fail))

#For each sum
data1 = indivParmsB %>% filter(PartnerPolicy=='Competitive')
model.compare(lm(scale(Sum) ~ scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) + scale(epsilon_c) + scale(epsilon_i)+
                   scale(Persec) + scale(ICARTot) + scale(Age) + Control, 
                 data = data1,
                 na.action = na.fail))
data2 = indivParmsB %>% filter(PartnerPolicy=='Individualist')
model.compare(lm(scale(Sum) ~ scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) + scale(epsilon_c) + scale(epsilon_i)+
                   scale(Persec) + scale(ICARTot) + scale(Age) + Control, 
                 data = data2,
                 na.action = na.fail))
data3 = indivParmsB %>% filter(PartnerPolicy=='Prosocial')
model.compare(lm(scale(Sum) ~ scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) + scale(epsilon_c) + scale(epsilon_i)+
                   scale(Persec) + scale(ICARTot) + scale(Age) + Control, 
                 data = data3,
                 na.action = na.fail))

pAll <- read.csv("/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/RegressionOutput.csv")
pAll <- pAll %>% mutate(Significant = ifelse(p.value < 0.05, 'True', 'False')) %>% filter(Type == 'Sum')

pA <- ggplot(pAll %>% filter(X %in% c('scale(alpha_m)', 'scale(alpha_v)', 'scale(beta_m)', 'scale(beta_v)')))+
  geom_bar(aes(X, Estimate, fill = Policy, alpha = Significant), color = 'black', stat ='identity', position = 'dodge')+
  geom_errorbar(aes(X, Estimate,group = Policy, ymin= conf.low, ymax=conf.high), 
                color = 'black', stat ='identity', position = 'dodge')+
  labs(title = 'Total Correct Answers',
       subtitle = '(Controlling for Age, Sex, ICAR Score, Persecutory Ideation, Task Comprehension)',
       y = expression(paste(beta, ' weight | 95% Confidence Interval'))
       )+
  scale_x_discrete(labels = c(#'Intercept', 'Control', 'Age', 
                              expression(alpha[m]), expression(alpha[v]),expression(beta[m]),expression(beta[v])
                              #'ICAR Score', 'Persec'
                              ))+
  scale_fill_brewer(name = 'Partner Policy', palette = 'Dark2')+
  coord_cartesian(ylim = c(-1, 1))+
  theme_minimal()+
  theme(legend.position = 'none',
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16),
        panel.grid.major.x = element_blank())


#For each HI
model.compare(lm(scale(HI) ~ scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) + scale(Persec) + 
                         scale(ICARTot) + scale(Age) + Control,
                       data = data1,
                       na.action = na.fail))
model.compare(lm(scale(HI) ~ scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) + scale(Persec) + 
                         scale(ICARTot) + scale(Age) + Control,
                       data = data2,
                       na.action = na.fail))
model.compare(lm(scale(HI) ~ scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) + scale(Persec) + 
                         scale(ICARTot) + scale(Age) + Control,
                       data = data3,
                       na.action = na.fail))

pAllb <- read.csv("/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/RegressionOutput.csv")
pAllb <- pAllb %>% 
  mutate(Significant = ifelse(p.value < 0.05, 'True', 'False')) %>% 
  filter(Type == 'HI') %>%
  add_row(X = 'scale(alpha_v)', Estimate = 0, conf.low = 0, conf.high = 0, Policy = 'Competitive', Significant = 'False') %>%
  add_row(X = 'scale(alpha_m)', Estimate = 0, conf.low = 0, conf.high = 0, Policy = 'Individualist', Significant = 'False')

pB <- ggplot(pAllb %>% filter(X %in% c('scale(alpha_m)', 'scale(alpha_v)', 'scale(beta_m)', 'scale(beta_v)')))+
  geom_bar(aes(X, Estimate, fill = Policy, alpha = Significant), color = 'black', stat ='identity', position = 'dodge')+
  geom_errorbar(aes(X, Estimate,group = Policy, ymin= conf.low, ymax=conf.high), 
                color = 'black', stat ='identity', position = 'dodge')+
  labs(title = 'Harmful Intent Attributions',
       y = expression(paste(beta, ' weight | 95% Confidence Interval'))
  )+
  scale_x_discrete(labels = c(#'Intercept', 'Control', 'Age', 
    expression(alpha[m]), expression(alpha[v]),expression(beta[m]),expression(beta[v])
    #'ICAR Score', 'Persec'
  ))+
  scale_fill_brewer(name = 'Partner Policy', palette = 'Dark2')+
  coord_cartesian(ylim = c(-1, 1))+
  theme_minimal()+
  theme(legend.position = c(0.5, 0.2),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16),
        panel.grid.major.x = element_blank())

#For each SI
p1c <- model.compare(lm(scale(SI) ~ scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) + scale(Persec) + 
                          scale(ICARTot) + scale(Age) + Control,
                        data = data1,
                        na.action = na.fail))
p2c <- model.compare(lm(scale(SI) ~ scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) + scale(Persec) + 
                          scale(ICARTot) + scale(Age) + Control,
                        data = data2,
                        na.action = na.fail))
p3c <- model.compare(lm(scale(SI) ~ scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) + scale(Persec) + 
                          scale(ICARTot) + scale(Age) + Control,
                        data = data3,
                        na.action = na.fail))

pAllc <- read.csv("/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/RegressionOutput.csv")
pAllc <- pAllc %>% 
  mutate(Significant = ifelse(p.value < 0.05, 'True', 'False')) %>% 
  filter(Type == 'SI' )%>%
  add_row(X = 'scale(beta_v)', Estimate = 0, conf.low = 0, conf.high = 0, Policy = 'Individualist', Significant = 'False')

pC <- ggplot(pAllc %>% filter(X %in% c('scale(alpha_m)', 'scale(alpha_v)', 'scale(beta_m)', 'scale(beta_v)')))+
  geom_bar(aes(X, Estimate, fill = Policy, alpha = Significant), color = 'black', stat ='identity', position = 'dodge')+
  geom_errorbar(aes(X, Estimate,group = Policy, ymin= conf.low, ymax=conf.high), 
                color = 'black', stat ='identity', position = 'dodge')+
  labs(title = 'Self Interest Attributions',
       y = expression(paste(beta, ' weight | 95% Confidence Interval'))
  )+
  scale_x_discrete(labels = c(#'Intercept', 'Control', 'Age', 
    expression(alpha[m]), expression(alpha[v]),expression(beta[m]),expression(beta[v])
    #'ICAR Score', 'Persec'
  ))+
  scale_fill_brewer(name = 'Partner Policy', palette = 'Dark2')+
  coord_cartesian(ylim = c(-1, 1))+
  theme_minimal()+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16),
        panel.grid.major.x = element_blank())

((pA + theme(legend.position = 'none')) | (pB+ theme(axis.title.y = element_blank())) | (pC+ theme(axis.title.y = element_blank(),
                                                           legend.position = 'none'))) & plot_annotation(tag_levels = "A")

  
# Visualisation -----------------------------------------------------------


# Model statistics

P1 <- ggplot(groupplot) + 
  geom_point(aes(parameter, value), color = c('#0097A7', '#80DEEA', '#303F9F', '#9575CD', '#2ECC71', '#138D75'),size =5, show.legend = F) +
  geom_pointrange(aes(parameter, value, ymin= value-sd, ymax=value+sd))+
  geom_label(aes(parameter, value),
             label = c('alpha [m]', 'alpha [v]', 'beta [m]', 'beta [v]', 'epsilon [con]', 'epsilon [incon]'),
             color = c('black', 'black', 'white', 'white', 'black', 'black'),
             fill = c('#0097A7', '#80DEEA', '#303F9F', '#9575CD', '#2ECC71', '#138D75'),
             size = 6,
             parse = T,
             label.size = 1,
             fontface = 'bold',
             nudge_x = 0.4)+
  coord_cartesian(clip = "off")+
  theme_minimal()+
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_blank(),
        strip.text.x = element_blank())

P2a <- groupplot.parms2 %>% 
  pivot_longer(5:6, names_to = 'Metric', values_to = 'Value') %>% 
  ggplot() + 
  geom_vline(xintercept = 0, alpha = 0.5)+
  ggridges::geom_density_ridges(aes(Value, Metric, fill = Metric), alpha = 0.7, show.legend = F,
                                jittered_points = TRUE,
                                position = position_points_jitter(height = 0, yoffset = -0.05),
                                point_shape = '|', point_size = 3, point_alpha = 1)+
  scale_fill_manual(values = c('#2ECC71', '#138D75')) +
  scale_discrete_manual(aesthetics = "point_color", values = c('#2ECC71', '#138D75')) +
  scale_y_discrete(labels=c(
    'epsilson_con' = expression(paste(epsilon[con])),
    'epsilson_incon' = expression(paste(epsilon[incon]))
  )) +
  theme_minimal()+
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        strip.text.x = element_blank())

P2b <- groupplot.parms2 %>% 
  pivot_longer(1:4, names_to = 'Metric', values_to = 'Value') %>% 
  ggplot() + 
  geom_vline(xintercept = 0, alpha = 0.5)+
  ggridges::geom_density_ridges(aes(Value, Metric, fill = Metric, point_color = Metric), alpha = 0.7, show.legend = F,
                                jittered_points = TRUE,
                                position = position_points_jitter(height = 0, yoffset = -0.05),
                                point_shape = '|', point_size = 3, point_alpha = 1)+
  scale_fill_manual(values = c('#0097A7', '#80DEEA', '#303F9F', '#9575CD')) +
  scale_discrete_manual(aesthetics = "point_color", values = c('#0097A7', '#80DEEA', '#303F9F', '#9575CD')) +
  scale_y_discrete(labels=c(
    'alpha_m' = expression(paste(alpha[m])),
    'alpha_v' = expression(paste(alpha[v])),
    'beta_m' = expression(paste(beta[m])),
    'beta_v' = expression(paste(beta[v]))
  )) +
  theme_minimal()+
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        strip.text.x = element_blank())

design2 <- "
            AAAA
            BBBB
            BBBB
"
P2a/P2b & plot_layout(design = design2)
  
#P2 <- ggplot(groupplot.parms1 %>% 
#           filter(parameter == 'alpha')) + 
#  geom_density(aes(value, fill = type), alpha = 0.75) +
#  scale_fill_manual(values = c('#0097A7', '#80DEEA'), 
#                    labels = c(expression(alpha[m]), expression(alpha[v])))+
#  expand_limits(x=0)+
#  theme_minimal() +
#  theme(legend.position = c(0.5, 0.8),
#        axis.title = element_blank(),
#        panel.grid.major.x = element_blank(),
#        panel.grid.minor.x = element_blank(),
#        legend.title = element_blank(),
#        plot.title = element_text(size = 16),
#        axis.text = element_text(size = 16),
#        strip.text.x = element_text(size = 18, face = 'bold'),
#        legend.text = element_text(size = 14))
  
P3 <- ggplot(Rec.data.modelcomp)+
  geom_bar(aes(model, metric, fill = type), stat = 'identity', color="black", width = .4, position = 'dodge') +
  scale_fill_brewer(palette = 'Set1')+
  scale_x_discrete(labels=c(
    'B_Fav' = expression(paste(alpha[m], "", alpha[v], "\n", beta[m], "",beta[v], "\n", kappa)),
    'B_Action' = expression(paste(alpha[m], "", alpha[v], "\n", beta[m], "",beta[v], "\n", rho[con], "", rho[incon])),
    'B_Choice' = expression(paste(alpha[m], "", alpha[v], "\n", beta[m], "",beta[v], "\n", epsilon[con], "", epsilon[incon])),
    'B_2' = expression(paste(beta[m], "",beta[v])),
    'B_4' = expression(paste(alpha[m], "", alpha[v], "\n", beta[m], "",beta[v])),
    'B_6' = expression(paste(alpha[m], alpha[v], "\n",beta[m], "",beta[v], "\n",alpha[m]^2, "",beta[m]^2)), 
    'B_shrink' = expression(paste(alpha[m], "",alpha[v],"\n", beta[m], "",beta[v], "\n",omega)),
    'B_zeta' = expression(paste(alpha[m], "",alpha[v], "\n",beta[m],"", beta[v],"\n", zeta)),
    'RW' = expression(paste(alpha[m],"", beta[m],"\n", tau,"\n", lambda)),
    'RW_2lrc' = expression(paste(alpha[m],"", beta[m], "\n",tau,"\n", lambda[pos],"", lambda[neg])),
    'RW_3lrc' = expression(paste(alpha[m],"", beta[m], "\n",tau,"\n", lambda[P], "",lambda[I],"", lambda[C])),
    'RW_Cong' = expression(paste(alpha[m],"", beta[m], "\n",tau,"\n", lambda[con], "",lambda[incon],""))
  ))+
  theme_minimal()+
  theme(axis.title = element_blank(),
        legend.position = c(0.7, 0.7),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 12, angle = 45),
        strip.text.x = element_text(size = 18, face = 'bold'))
  
#P4 <- ggplot(groupplot.parms1 %>% 
#           filter(parameter =='beta')) + 
#  geom_density(aes(value, fill = type), alpha = 0.75) +
#  scale_fill_manual(values = c('#303F9F', '#9575CD'), 
#                    labels = c(expression(beta[m]), expression(beta[v])))+
#  theme_minimal() +
#  theme(legend.position = c(0.5, 0.8),
#        axis.title = element_blank(),
#        legend.title = element_blank(),
#        panel.grid.major.x = element_blank(),
#        panel.grid.minor.x = element_blank(),
#        plot.title = element_text(size = 16),
#        axis.text = element_text(size = 16),
#        strip.text.x = element_text(size = 18, face = 'bold'),
#        legend.text = element_text(size = 14))
#
#P4 <- ggplot(groupplot.parms1 %>% 
#               filter(parameter =='epsilon')) + 
#  geom_density(aes(value, fill = type), alpha = 0.75) +
#  scale_fill_manual(values = c('#03A9F4', '#FF7043'), 
#                    labels = c(expression(epsilon[c]), expression(epsilon[i])))+
#  theme_minimal() +
#  theme(legend.position = c(0.5, 0.8),
#        axis.title = element_blank(),
#        legend.title = element_blank(),
#        panel.grid.major.x = element_blank(),
#        panel.grid.minor.x = element_blank(),
#        plot.title = element_text(size = 16),
#        axis.text = element_text(size = 16),
#        strip.text.x = element_text(size = 18, face = 'bold'),
#        legend.text = element_text(size = 14))

(P1/P3) | (P2 / P4) 

ggplot(indivParms) +
  geom_point(aes(value, Sum, color = PartnerPolicy), alpha = 0.1) +
  geom_smooth(aes(value, Sum, color = PartnerPolicy), method = 'lm') +
  coord_cartesian(ylim = c(10, 36))+
  scale_color_brewer(palette = 'Set1', name = 'Partner Policy')+
  facet_wrap(parameter ~ type, 
             scales = 'free_x',
             labeller = label_parsed)+
  ggpubr::stat_cor(aes(value, Sum, color = PartnerPolicy), method = 'spearman', label.y.npc = 0.5, label.x.npc = 'centre', show.legend = F)+
  labs(title = 'Individual Parameter Estimates by Total Correct Guesses in Phase 2',
       y = 'Total Correct Guesses',
       x = 'Parameter Value')+
  theme_minimal() +
  theme(legend.position = c(0.1, 0.1), 
        strip.text.x = element_text(face = 'bold', size = 14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size =14),
        legend.text = element_text(size = 14))

parmCor <- cor(RecData.parameters[[4]][[1]], method = 'spearman')
ggcorrplot::ggcorrplot(parmCor, type = 'upper', lab = T)

ggplot(indivParms) +
  geom_point(aes(Persec, value, color = type), alpha = 0.1) +
  geom_smooth(aes(Persec, value, color = type), method = 'lm') +
  scale_color_brewer(palette = 'Dark2', direction = -1)+
  facet_wrap(parameter ~. , 
             scale = 'free_y',
             labeller = label_parsed)+
  ggpubr::stat_cor(aes(Persec, value, color = type), method = 'spearman', label.y.npc = 0.2, label.x.npc = 'centre', show.legend = F)+
  theme_minimal() +
  theme(legend.position = c(0.25, 0.5), 
        strip.text.x = element_text(face = 'bold', size = 14))+
  labs(title = 'Persecutory Ideation by Individual Parameter Estimates')

ggplot(indivParms) +
  geom_point(aes(Persec, value, color =  type), alpha = 0.1) +
  geom_smooth(aes(Persec, value, color = type), method = 'lm') +
  scale_color_brewer(palette = 'Dark2', direction = -1)+
  ggpubr::stat_cor(aes(Persec, value, color = type), method = 'spearman', label.y.npc = 0.2, label.x.npc = 'centre', show.legend = F)+
  theme_minimal() +
  facet_wrap(parameter ~ type, labeller = label_parsed)+
  theme(legend.position = c(0.25, 0.7), 
        strip.text.x = element_text(face = 'bold', size = 14))+
  labs(title = 'Persecutory Ideation by Individual Parameter Estimates')

ggplot(indivParms %>% 
         pivot_longer(10:11, names_to = 'attribute', values_to = 'metric') %>%
         filter(metric <= 100)) +
  geom_point(aes( value,metric,  color = attribute), alpha = 0.1) +
  geom_smooth(aes(value,metric, color = attribute), method = 'lm') +
  scale_color_brewer(palette = 'Dark2', direction = -1, name = 'Attribute')+
  coord_cartesian(ylim = c(0,100))+
  facet_wrap(parameter ~ type, 
             scale = 'free_x', 
             ncol = 4, 
             labeller = label_parsed)+
  ggpubr::stat_cor(aes(value,metric,color = attribute), method = 'spearman', label.y.npc = 0.2, label.x.npc = 'left', show.legend = F)+
  labs(title = 'Attributions by Individual Parameter Estimates',
       y = 'Value')+
  theme_minimal() +
  theme(legend.position = c(0.25, 0.1),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.direction = 'horizontal',
        legend.title = element_text(size =14),
        legend.text = element_text(size = 14),
        strip.text.x = element_text(face = 'bold', size = 14))

# Model Error Check -------------------------------------------------------

RecError <- readMat('/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/modelerror8mod_BFS4_Alpha15CheckCheck.mat')

loglikstats <- data.frame(lik1 = RecError$lik1,
                          lik2 = RecError$lik2,
                          ll   = RecError$F,
                          id   = 1:697)

controlling <- plyr::join(loglikstats, indivParms %>% mutate(PartnerPolicy == ifelse(PartnerPolicy == 'Competative', 'Competitive', PartnerPolicy)), by = 'id')


#error3 <- ggplot(p.plots %>% join(controlling, by = 'id') %>% mutate(Probability = ifelse(Probability < 0.5, 1-Probability, Probability)))+
#  geom_density(aes(Probability, color = PartnerPolicy), alpha = 0.1)+
#  #facet_wrap(~parameter, scales = 'free_x')+
#  labs(title = 'Individual Parameter Estimates x Probability on Each Trial')+
#  theme_minimal()

probabilities <- matrix(NA, nrow = 697, ncol = 54)
probabilities.1 <- matrix(NA, nrow =  697, ncol = 18)
probabilities.2 <- matrix(NA, nrow =  697, ncol = 36)
simA <- probabilities
for (i in 1:697){
  probabilities[i,] <- as.numeric(RecError$action[[i]][[1]])
  probabilities.1[i,] <- as.numeric(RecError$prob1[[i]][[1]])
  probabilities.2[i,] <- as.numeric(RecError$prob2[[i]][[1]])
  simA[i,]<- as.numeric(RecError$simA[[i]][[1]])
}

probabilities <- as.data.frame(probabilities)
probabilities.1 <- as.data.frame(probabilities.1)
probabilities.2 <- as.data.frame(probabilities.2)
simA <- as.data.frame(simA)
probabilities$ID <- 1:697
simA$ID <- 1:697
probabilities.1$ID <- 1:697
probabilities.2$ID <- 1:697
colnames(probabilities) <- c(1:54, 'id')
colnames(simA) <- c(1:54, 'id')
colnames(probabilities.1) <- c(1:18, 'id')
colnames(probabilities.2) <- c(19:54, 'id')

probabilities_p <- cbind(probabilities.1, probabilities.2[,1:36])

probabilities %>% 
  pivot_longer(1:54, names_to = 'Trial', values_to = 'Action') -> probabilities.edit
probabilities_p %>% 
  dplyr::select(1:18, 20:55, id) %>%
  pivot_longer(1:54, names_to = 'Trial', values_to = 'Probability') -> probabilities.p.edit
simA %>% 
  pivot_longer(1:54, names_to = 'Trial', values_to = 'simA') -> simA.edit
p.plots <- cbind(probabilities.edit, probabilities.p.edit, simA.edit)

p.plots <- p.plots %>% dplyr::select(1, 3, 5, 6, 9) %>% 
  plyr::join(controlling %>% dplyr::select(1:14), by = 'id') %>% 
  mutate(Trial = as.numeric(Trial),
         Phase = ifelse(Trial < 19, 1, 2)) %>%
  distinct()

library(ggridges)

run = 1
if(run ==1){

llPlot <- ggplot(p.plots)+
  geom_density_ridges(aes(Probability, factor(Phase)), fill = 'light grey', scale = .95)+
  geom_density_ridges(aes(Probability, factor(Phase), fill = PartnerPolicy, color = PartnerPolicy), 
                      alpha = 0.7, scale = .95, rel_min_height = .01)+
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3"))+
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3"))+
  labs(x = 'Probability of choice for each trial')+
  scale_discrete_manual("point_color", values = c("#1B9E77", "#D95F02", "#7570B3"), guide = "none") +
  scale_y_discrete(labels = c("Phase1", "Phase 2"))+
  scale_x_continuous(breaks = c(0, 0.5, 1))+
  geom_vline(xintercept = 0.5)+
  theme_ridges()+
  theme(legend.position = c(0.1, 1),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_text(hjust = 0.5))
  
#df<-data.frame(x = c(log(0.5)*18, log(0.5)*36, log(0.5)*54), y = c('lik1', 'lik2', 'll'), xend = c(log(0.5)*18, log(0.5)*36, log(0.5)*54), yend = 0)
#ggplot(controlling %>% pivot_longer(1:3, names_to = 'Phase', values_to = 'll'))+
#  geom_segment(data = df, aes(x=x, y=y, xend=xend, yend = yend))+
#  geom_density_ridges(aes(ll, Phase), fill = 'light grey', scale = .95)+
#  geom_density_ridges(aes(ll, Phase, fill = PartnerPolicy, color = PartnerPolicy, point_color = PartnerPolicy), 
#                                jittered_points = TRUE,
#                                position = position_points_jitter(height = 0),
#                                point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7, scale = .95, rel_min_height = .01)+
#  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3"))+
#  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3"))+
#  labs(x = 'Sum LogLikelihood')+
#  scale_discrete_manual("point_color", values = c("#1B9E77", "#D95F02", "#7570B3"), guide = "none") +
#  scale_x_continuous(breaks = c(round(log(0.5)*18, 2), round(log(0.5)*36,2), round(log(0.5)*54, 2)))+
#  scale_y_discrete(labels = c("Phase1", "Phase 2", 'Phase 1 + 2'))+
#  theme_ridges()+
#  theme(legend.position = c(-0.1, -0.5),
#        legend.text = element_text(size = 14),
#        legend.title = element_text(size = 14),
#        axis.title.y = element_blank(),
#        panel.grid.major.x = element_blank(),
#        axis.title.x = element_text(hjust = 0.5))
#
title = sample(1:697, 1) 
y2 <- p.plots %>%
  mutate(Action = ifelse(Action==2, 0, 1),
         simA   = ifelse(simA==2, 0, 1),
         Probability = ifelse(Action == 0, 1-Probability, Probability),
         Trial = as.numeric(Trial)) %>%
  filter(id == title) %>%
  pivot_longer(c(2,4), names_to = 'Type', values_to = 'Value') %>%
  ggplot()+
  annotate(geom ='rect', xmin = 18, xmax = 54, ymin = 0, ymax = 1, alpha = 0.3, fill = '#D6EAF8')+
  annotate(geom ='text', x = c(15,21), y=1.1, colour = '#17202A', label = c('Phase1', 'Phase 2'), fontface = 'bold')+
  geom_line(aes(Trial, Value, linetype = Type, color = Type))+
  geom_vline(xintercept = 18)+
  coord_cartesian(xlim = c(0, 54))+
  scale_y_discrete(breaks = c(0, 0.5, 1))+
  scale_color_manual(values = c('#2C3E50', '#C0392B', '#17202A'))+
  labs(x = 'Trial', y = 'Action | Action Probability',
       title = paste("ID = ",title),
       subtitle = 'ID randomly drawn from the population', 
       caption = paste( 'Details of sampled participant:   ',
                        'Partner Policy = ',
                        indivParms %>% filter(id == title) %>% dplyr::select(PartnerPolicy) %>% distinct(),
                        ' alpha[m] = ', 
                        round(indivParms %>% filter(id == title, parameter == 'alpha', type == 'Mean') %>% dplyr::select(value), 2),
                        ' alpha[v] = ', 
                        round(indivParms %>% filter(id == title, parameter == 'alpha', type == 'Var') %>% dplyr::select(value), 2),
                        ' beta[m] = ', 
                        round(indivParms %>% filter(id == title, parameter == 'beta', type == 'Mean') %>% dplyr::select(value), 2),
                        ' beta[v] = ', 
                        round(indivParms %>% filter(id == title, parameter == 'beta', type == 'Var') %>% dplyr::select(value), 2)))+
  theme_minimal()+
  theme(        axis.text = element_text(size = 14),
                axis.title = element_text(size = 14),
                plot.title = element_text(size = 16),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.y = element_blank(),
                legend.position = c(0.85, 1.05),
                legend.direction = 'horizontal',
                legend.title = element_blank(),
                legend.box.background = element_rect(colour = 'black'))

design <- "ABCD
           ABCD
           EEEF
           GGGG"
            
patchwork::wrap_plots(A = P3, B = P1, C = P2, D = P4, E = llPlot, F = P5, G = y2, design = design) & patchwork::plot_annotation(tag_levels = 'A') 
((P1/P3) | (P2 / P4)) /
(llPlot|P5) / y2 & patchwork::plot_annotation(tag_levels = 'A') & 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))
}

# Recovery Analysis -------------------------------------------------------

RecRecovery <- readMat('/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/lap_BFS4_recovery1.mat')
RecReal     <- readMat('/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/lap_BFS4TEST.mat')

RealParms <- RecReal$cbm[,,1]$output[,,1]$parameters %>% 
  as.data.frame() %>% 
  mutate(id = 1:697) %>%

RecoveredParms <- RecRecovery$cbm[,,1]$output[,,1]$parameters %>% 
  as.data.frame() %>% 
  mutate(id = 1:697)

Fit_Rec <- plyr::join(RecoveredParms[,1:5], RealParms[,1:5], by = 'id') 
Fit_Rec <- plyr::join(Fit_Rec, indivParmsB, by = 'id') 

FitRecCor <- cor(Fit_Rec[,c(6:9, 1:4)])
P5 <- ggcorrplot::ggcorrplot(FitRecCor[1:4, 5:8], lab = T,
                             colors = c('#3498DB', '#E5E8E8', '#E74C3C'))+
  scale_x_discrete(labels = c(
    expression(paste(alpha[m], ' Rec')),
    expression(paste(beta[m], ' Rec')),
    expression(paste(alpha[v] , ' Rec')),
    expression(paste(beta[v] , ' Rec'))
  ))+
  scale_y_discrete(labels = c(
    expression(paste(alpha[m], ' Fit')),
    expression(paste(beta[m],  ' Fit')),
    expression(paste(alpha[v], ' Fit')),
    expression(paste(beta[v],  ' Fit'))
  ))+
  theme(legend.position = 'none')

P5

# Generative ability ------------------------------------------------------

non_listsimA <- as.data.frame(matrix(NA, nrow = 697, ncol = 55))
for (i in 1:697){
non_listsimA[i,1:54] <- RecError$simA[[i]][[1]]
non_listsimA[i,55] <- i
}
colnames(non_listsimA) <- c(1:54, 'id')

non_listsimA %>%
  pivot_longer(1:54, names_to = 'Trial', values_to = 'simA') %>%
  join(Intentions_both_matlab, by = c('id', 'Trial')) %>%
  join(indivParms, by = 'id') %>%
  mutate(id = as.numeric(id)) %>%
  na.omit() %>%
  group_by(id) %>%
  dplyr::select(-value, -type, -parameter) %>%
  distinct() %>%
  group_by(id) %>%
  mutate(CorrectSim = ifelse(simA == as.numeric(Answer), 1, 0),
         Match = ifelse(simA == Response, 1, 0)) -> testdf

aggregate(testdf$CorrectSim, by=list(id=testdf$id), FUN=sum) %>%
  join(testdf, by = 'id') %>%
  join(controlling %>% dplyr::select(id, ll, lik1, lik2), by = 'id') -> testdf2

summary(lm(x ~ scale(ICARTot) + scale(Persec) + Age + PartnerPolicy, data = testdf2, na.action = na.fail))

