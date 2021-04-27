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

simulatedDat <-  foreach(i = 1:1000, .combine = rbind) %dopar% {
genpar <- c(mysamp(1, 0, 3, 0, 5, 1000), mysamp(1, 0, 3, -5, 5, 1000), 
            mysamp(1, 0, 3, 0, 5, 1000), mysamp(1, 0, 3, 0, 5, 1000),
            mysamp(1, 0, 3, 0, 5, 1000), mysamp(1, 0, 3, -5, 5, 1000), 
            mysamp(1, 0, 1, 0, 1, 1000))
data = rbind(as.data.frame(Intentions_list_C[[1]][,c('Response', 'Option1_PPT', 'Option1_Partner', 'Option2_PPT', 'Option2_Partner', 'Trial', 'Real_Answer')]) %>% rename(Answer = Real_Answer) %>% mutate(Answer = ifelse(Answer == "Option 1", 1, 2)),
             as.data.frame(Intentions_list_G[[1]][,c('Response', 'Option1_PPT', 'Option1_Partner', 'Option2_PPT', 'Option2_Partner', 'Trial', 'Answer')]))
synD <- Phase1Sim(genpar, rbind(data)) #generate synthetic data
synD$Trial <- c(1:18, 1:36)
synD$alpha <- genpar[1]
synD$beta  <- genpar[2]
synD$alpha2<- genpar[5]
synD$beta2 <- genpar[6]
synD$alphav<- genpar[3]
synD$betav <- genpar[4]
synD$zeta  <- genpar[7]
synD$ppt   <- i
colnames(synD) <- c('Response', 'prob1', 'Option1_PPT', 'Option1_Partner', 'Option2_PPT', 'Option2_Partner', 'Answer', 'Trial', 'alpha', 'beta', 'alpha2', 'beta2', 'alpha_v', 'beta_v', 'zeta', 'ppt')
data.frame(
  synD
)
}

write.csv(simulatedDat, '/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/simulatedDat.csv')

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

                      ############################################
                      # Go into MatLab scripts for computational #
                      # work completed using the CBM functions  ##
                      ############################################

# Matlab data vis and analysis --------------------------------------------

# load data from matlab
library(R.matlab)
RecData <- readMat('/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/hbi_BFS_RW_8mods.mat')

RecData.frequency  <- RecData$cbm[,,1]$output[,,1]$model.frequency
RecData.exceed     <- RecData$cbm[,,1]$output[,,1]$exceedance.prob
RecData.p.exceed   <- RecData$cbm[,,1]$output[,,1]$protected.exceedance.prob
RecData.parameters <- RecData$cbm[,,1]$output[,,1]$parameters
RecData.groupmeans <- RecData$cbm[,,1]$output[,,1]$group.mean
RecData.grouperror <- RecData$cbm[,,1]$output[,,1]$group.hierarchical.errorbar

RecData.groupmeans[[4]][[1]] <- as.data.frame(RecData.groupmeans[[4]][[1]])
RecData.frequency            <- as.data.frame(RecData.frequency)
RecData.exceed               <- as.data.frame(RecData.exceed)
RecData.p.exceed             <- as.data.frame(RecData.p.exceed)
colnames(RecData.frequency)  <- c('B_6', 'B_shrink', 'B_zeta', 'B_4', 'B_2', 'RW', 'RW_3lrc', 'RW_2lrc')
colnames(RecData.exceed)     <- c('B_6', 'B_shrink', 'B_zeta', 'B_4', 'B_2', 'RW', 'RW_3lrc', 'RW_2lrc')
colnames(RecData.p.exceed)   <- c('B_6', 'B_shrink', 'B_zeta', 'B_4', 'B_2', 'RW', 'RW_3lrc', 'RW_2lrc')
colnames(RecData.groupmeans[[4]][[1]]) <- c('alpha_m', 'beta_m', 'alpha_v', 'beta_v')
colnames(RecData.parameters[[4]][[1]]) <- c('alpha_m', 'beta_m', 'alpha_v', 'beta_v')
colnames(RecData.grouperror[[4]][[1]]) <- c('alpha_m', 'beta_m', 'alpha_v', 'beta_v')
# Transform between 0 and positive integer
RecData.groupmeans[[4]][[1]][,1] <- (10/(1+exp(-RecData.groupmeans[[4]][[1]][,1])))
RecData.groupmeans[[4]][[1]][,3] <- exp(RecData.groupmeans[[4]][[1]][,3])
RecData.groupmeans[[4]][[1]][,4] <- exp(RecData.groupmeans[[4]][[1]][,4])

RecData.parameters[[4]][[1]][,1] <- (10/(1+exp(-RecData.parameters[[4]][[1]][,1])))
RecData.parameters[[4]][[1]][,3] <- exp(RecData.parameters[[4]][[1]][,3])
RecData.parameters[[4]][[1]][,4] <- exp(RecData.parameters[[4]][[1]][,4])

#augment data 
groupplot <- RecData.groupmeans[[4]][[1]] %>% 
                        select(alpha_m, alpha_v, beta_m, beta_v) %>%
                        pivot_longer(1:4, names_to = 'parameter', values_to = 'value') %>%
                        mutate(sd = RecData.grouperror[[4]][[1]] %>%
                                 as.data.frame() %>%
                                 dplyr::select(alpha_m, alpha_v, beta_m, beta_v) %>%
                                 t(), 
                               type = c(rep('alpha', 2), rep('beta', 2)),
                               metric = c('Mean', 'Var', 'Mean', 'Var')) %>%
                        dplyr::rename(sd = 3)
RecData.frequency %>%
  pivot_longer(1:8, names_to = 'model', values_to = 'metric') %>%
  mutate(type = 'Freqency') -> freq
RecData.exceed %>%
  pivot_longer(1:8, names_to = 'model', values_to = 'metric') %>%
  mutate(type = 'Ex. Probability') -> exprob
RecData.p.exceed %>%
  pivot_longer(1:8, names_to = 'model', values_to = 'metric') %>%
  mutate(type = 'Ex. Probability (Protected)') -> p.exprob
Rec.data.modelcomp <- rbind(freq, exprob, p.exprob)

groupplot.parms <- RecData.parameters[[4]][[1]]%>% 
  as.data.frame() %>%
  mutate(id = 1:697) %>%
  select(alpha_m, alpha_v, beta_m, beta_v, id) %>%
  pivot_longer(1:4, names_to = 'parameter', values_to = 'value') %>%
  mutate(type = ifelse(parameter %in% c('alpha_m', 'beta_m'), 'Mean', 'Var'),
         parameter = ifelse(parameter %in% c('alpha_m', 'alpha_v'), 'alpha', 'beta'))

Intentions_splice <- Intentions_guess %>% select(
  id, PartnerPolicy, Control, Sum, Game, Persec, ICARTot, ComLadder, Age, HI, SI, Agency
) %>%
  distinct()

indivParms <- plyr::join(Intentions_splice, groupplot.parms, by= 'id')

# Visualisations
# Model statistics

ggplot(groupplot) + 
  geom_point(data = groupplot %>% filter(parameter %in% c('alpha_m', 'alpha_v')) , aes(metric, value, color = type),size =5, show.legend = F) +
  geom_point(data = groupplot %>% filter(parameter %in% c('beta_m', 'beta_v')) , aes(metric, value, color = type)  ,size =5, show.legend = F) +
  geom_pointrange(data = groupplot %>% filter(parameter %in% c('alpha_m', 'alpha_v')) , aes(metric, value, ymin= value-sd, ymax=value+sd))+
  geom_pointrange(data = groupplot %>% filter(parameter %in% c('beta_m', 'beta_v')) , aes(metric, value, ymin= value-sd, ymax=value+sd))+
  expand_limits(y=0)+
  scale_color_manual(values = c('#0097A7', '#9575CD'))+
  labs(title = 'A | Group Parameter Estimates of the Winning Model')+
  facet_wrap(~type, scales = 'free', 
             labeller = label_parsed)+
  theme_minimal()+
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        strip.text.x = element_text(size = 18, face = 'bold'))+
  
ggplot(groupplot.parms %>% 
           filter(parameter == 'alpha')) + 
  geom_density(aes(value, fill = type), alpha = 0.75) +
  scale_fill_manual(values = c('#0097A7', '#80DEEA'), 
                    labels = c(expression(alpha[m]), expression(alpha[v])))+
  expand_limits(x=0)+
  labs(title = expression(paste('B | Individual ', alpha, ' Density Distr.')))+
  theme_minimal() +
  theme(legend.position = c(0.2, 0.8),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        strip.text.x = element_text(size = 18, face = 'bold'),
        legend.text = element_text(size = 14))+
  
ggplot(Rec.data.modelcomp)+
  geom_bar(aes(model, metric, fill = type), stat = 'identity', color="black", width = .4, position = 'dodge') +
  labs(title = 'C | Model Comparison')+
  scale_fill_brewer(palette = 'Set1')+
  scale_x_discrete(labels=c(
    'B_2' = expression(paste(beta[m], "",beta[v])),
    'B_4' = expression(paste(alpha[m], "", alpha[v], "\n", beta[m], "",beta[v])),
    'B_6' = expression(paste(alpha[m], alpha[v], "\n",beta[m], "",beta[v], "\n",alpha[m]^2, "",beta[m]^2)), 
    'B_shrink' = expression(paste(alpha[m], "",alpha[v],"\n", beta[m], "",beta[v], "\n",omega)),
    'B_zeta' = expression(paste(alpha[m], "",alpha[v], "\n",beta[m],"", beta[v],"\n", zeta)),
    'RW' = expression(paste(alpha[m],"", beta[m],"\n", tau,"\n", lambda)),
    'RW_2lrc' = expression(paste(alpha[m],"", beta[m], "\n",tau,"\n", lambda[pos],"", lambda[neg])),
    'RW_3lrc' = expression(paste(alpha[m],"", beta[m], "\n",tau,"\n", lambda[P], "",lambda[I],"", lambda[C]))
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
        strip.text.x = element_text(size = 18, face = 'bold'))+
  
ggplot(groupplot.parms %>% 
           filter(parameter =='beta')) + 
  geom_density(aes(value, fill = type), alpha = 0.75) +
  scale_fill_manual(values = c('#303F9F', '#9575CD'), 
                    labels = c(expression(beta[m]), expression(beta[v])))+
  labs(title = expression(paste('B | Individual ', beta, ' Density Distr.')))+
  theme_minimal() +
  theme(legend.position = c(0.2, 0.8),
        axis.title = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        strip.text.x = element_text(size = 18, face = 'bold'),
        legend.text = element_text(size = 14))
  
ggplot(indivParms) +
  geom_point(aes(value, Sum, color = PartnerPolicy), alpha = 0.1) +
  geom_smooth(aes(value, Sum, color = PartnerPolicy), method = 'lm') +
  coord_cartesian(ylim = c(10, 35))+
  scale_color_brewer(palette = 'Set1')+
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
  facet_wrap(parameter ~ ., 
             scale = 'free_y',
             labeller = label_parsed)+
  ggpubr::stat_cor(aes(Persec, value, color = type), method = 'spearman', label.y.npc = 0.2, label.x.npc = 'centre', show.legend = F)+
  theme_minimal() +
  theme(legend.position = c(0.25, 0.5), 
        strip.text.x = element_text(face = 'bold', size = 14))+
  labs(title = 'Persecutory Ideation by Individual Parameter Estimates')

ggplot(indivParms %>% 
         pivot_longer(10:11, names_to = 'attribute', values_to = 'metric')) +
  geom_point(aes( value,metric,  color = attribute), alpha = 0.1) +
  geom_smooth(aes(value,metric, color = attribute), method = 'lm') +
  scale_color_brewer(palette = 'Dark2', direction = -1, name = 'Attribute')+
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

# Linear model tests
library(tidybayes)
library(brms)
library(ggdist)

modelConfer1 <- indivParms %>% filter(parameter == 'beta', type == 'Var')
#model.compare(lm(scale(value) ~ scale(HI) * PartnerPolicy + scale(Persec) + scale(ICARTot) + scale(Age) + scale(Sum) + Control, data = modelConfer1, na.action = na.fail))
#model.compare(lm(scale(SI) ~ scale(value) * PartnerPolicy + scale(Persec) + scale(ICARTot) + scale(Age) + scale(Sum) + Control, data = modelConfer1, na.action = na.fail))
mc1brmHI1 <- brm(scale(value) ~ scale(HI)  + scale(Persec) + scale(ICARTot) + scale(Age) + scale(Sum) + Control, data = modelConfer1)
#mc1brmSI <- brm(scale(SI) ~ scale(value) * PartnerPolicy + scale(Persec) + scale(ICARTot) + scale(Age) + scale(Sum) + Control, data = modelConfer1)
modelConfer2 <- indivParms %>% filter(parameter == 'beta', type == 'Mean')
mc1brmHI2 <- brm(scale(value) ~ scale(HI) + scale(Persec) + scale(ICARTot) + scale(Age) + scale(Sum) + Control, data = modelConfer2)
modelConfer3 <- indivParms %>% filter(parameter == 'alpha', type == 'Var')
mc1brmHI3 <- brm(scale(value) ~ scale(HI)  + scale(Persec) + scale(ICARTot) + scale(Age) + scale(Sum) + Control, data = modelConfer3)
modelConfer4 <- indivParms %>% filter(parameter == 'alpha', type == 'Mean')
mc1brmHI4 <- brm(scale(value) ~ scale(HI)  + scale(Persec) + scale(ICARTot) + scale(Age) + scale(Sum) + Control, data = modelConfer4)

mc1brmHI1$fit %>% 
  as.data.frame() %>%
  pivot_longer(1:7, names_to = 'Parameter', values_to = 'value') %>% 
  mutate(type = 'beta_v') -> Plotmc1brmHI1
mc1brmHI2$fit %>% 
  as.data.frame() %>%
  pivot_longer(1:7, names_to = 'Parameter', values_to = 'value') %>% 
  mutate(type = 'beta_m') -> Plotmc1brmHI2
mc1brmHI3$fit %>% 
  as.data.frame() %>%
  pivot_longer(1:7, names_to = 'Parameter', values_to = 'value') %>% 
  mutate(type = 'alpha_v') -> Plotmc1brmHI3
mc1brmHI4$fit %>% 
  as.data.frame() %>%
  pivot_longer(1:7, names_to = 'Parameter', values_to = 'value') %>% 
  mutate(type = 'alpha_m') -> Plotmc1brmHI4

plotBayes1 <- rbind(Plotmc1brmHI1, Plotmc1brmHI2, Plotmc1brmHI3, Plotmc1brmHI4)
                  
#xlabs_1 <- c('Intercept', 'Sum', 'Age', 'HI', 'Value', 'Persec',  'HI x P:C', 'HI x I:C', 'ICAR', 'Control',  'P:C', 'I:C')

ggplot(plotBayes1)+
  #scale_y_discrete(labels = xlabs_1)+
  stat_halfeye(aes(value, reorder(Parameter, -value), fill = type)) +
  scale_fill_brewer(expression(paste('X-Axis ',beta, ' Weight =')), palette = 'Dark2',
                    labels = c(expression(paste(alpha[m])), 
                               expression(paste(alpha[v])), 
                               expression(paste(beta[m])), 
                               expression(paste(beta[v]))))+
  geom_vline(xintercept = 0)+
  theme_ggdist()+
  theme(axis.title.y =element_blank(),
        axis.title.x =element_blank(),
        legend.text = element_text(size = 14),
        legend.position = c(0.2, 0.2))

modelConfer5 <- indivParms %>% filter(parameter == 'beta', type == 'Var')
p1 <- model.compare(lm(scale(Sum) ~ scale(value) + PartnerPolicy + scale(Persec) + scale(ICARTot) + scale(Age)  + Control, data = modelConfer5, na.action = na.fail))
modelConfer6 <- indivParms %>% filter(parameter == 'beta', type == 'Mean')
p2 <- model.compare(lm(scale(Sum) ~ scale(value) + PartnerPolicy + scale(Persec) + scale(ICARTot) + scale(Age)  + Control, data = modelConfer6, na.action = na.fail))
modelConfer7 <- indivParms %>% filter(parameter == 'alpha', type == 'Var')
p3 <- model.compare(lm(scale(Sum) ~ scale(value) + PartnerPolicy + scale(Persec) + scale(ICARTot) + scale(Age)  + Control, data = modelConfer7, na.action = na.fail))
modelConfer8 <- indivParms %>% filter(parameter == 'alpha', type == 'Mean')
p4 <- model.compare(lm(scale(Sum) ~ scale(value) + PartnerPolicy + scale(Persec) + scale(ICARTot) + scale(Age)  + Control, data = modelConfer8, na.action = na.fail))

p1 <- p1 %>% as.data.frame() %>% mutate(parameter = 'beta_var')
p2 <- p2 %>% as.data.frame() %>% mutate(parameter = 'beta_mean')
p3 <- p3 %>% as.data.frame() %>% mutate(parameter = 'alpha_var')
p4 <- p4 %>% as.data.frame() %>% mutate(parameter = 'alpha_mean')
pAll <- rbind(p1, p2, p3, p4)

ggplot(pAll)+
  geom_bar(aes(term, estimate, fill = parameter), color = 'black', stat ='identity', position = 'dodge')+
  geom_errorbar(aes(term, estimate,group = parameter, ymin= conf.low, ymax=conf.high), color = 'black', stat ='identity', position = 'dodge')+
  labs(title = expression(paste(beta, ' estimates of total correct answers in Phase 2')),
       y = expression(paste(beta, ' weight')),
       x = 'Model Term')+
  scale_x_discrete(labels = c('Intercept', 'Control', 'I:C Policy', 'P:C Policy', 'Age', 'ICAR Score', 'Persec', 'Parm Value'))+
  scale_fill_manual(values = c('#0097A7', '#80DEEA', '#303F9F', '#9575CD'), name = 'Model Parameter As Predictor',
                    labels = c(expression(alpha[m]), expression(alpha[v]), expression(beta[m]), expression(beta[v])))+
  theme_minimal()+
  theme(legend.position = c(0.7, 0.2),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16),
        panel.grid.major.x = element_blank())

# Model Error Check -------------------------------------------------------


RecError <- readMat('/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/modelerror.mat')

loglikstats <- data.frame(lik1 = RecError$lik1,
                          lik2 = RecError$lik2,
                          ll   = RecError$F,
                          id   = 1:697)

controlling <- plyr::join(loglikstats, indivParms, by = 'id')

ggplot(controlling %>% 
         pivot_longer(1:3, names_to = 'll_type', values_to = 'metric'))+
  geom_density(aes(metric, fill = ll_type), alpha = 0.5)+
  geom_vline(xintercept = c(log(0.5)*18, log(0.5)*36,log(0.5)*54))+
  scale_fill_brewer(palette = 'Dark2')+
  theme_minimal()

probabilities <- matrix(NA, nrow = 697, ncol = 54)
probabilities.1 <- matrix(NA, nrow =  697, ncol = 18)
probabilities.2 <- matrix(NA, nrow =  697, ncol = 36)
simA <- probabilities
for (i in 1:697){
  probabilities[i,] <- as.numeric(RecError$action[[i]][[1]])
  probabilities.1[i,] <- as.numeric(RecError$prob1[[i]][[1]])
  probabilities.2[i,] <- as.numeric(RecError$prob1[[i]][[1]])
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

p.plots <- p.plots %>% dplyr::select(1, 3, 5, 6, 9)

run = 1
if(run ==1){

y1 <- ggplot(controlling %>% 
         pivot_longer(1:3, names_to = 'll_type', values_to = 'metric'))+
  geom_density(aes(metric, fill = ll_type), alpha = 0.5)+
  geom_vline(xintercept = c(log(0.5)*18, log(0.5)*36,log(0.5)*54))+
  scale_fill_brewer(palette = 'Dark2', labels = c('Phase1', 'Phase2', 'Both'), name = 'Loglikelihood')+
  labs(y = 'Density',
       title = 'Population Loglikelihood values')+
  annotate("text", x = c(log(0.5)*18, log(0.5)*36,log(0.5)*54), y = 0.1, 
           angle = 90,
           label = c('log(0.5)*18', 'log(0.5)*36', 'log(0.5)*54'),
           vjust = 1.5,
           fontface='bold'
           )+
  theme_minimal()+
  theme(legend.position = c(0.3, 0.8),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16),
        panel.grid.major.x = element_blank(),
        legend.box.background = element_rect(colour = 'black'))

title = sample(1:697, 1) 
y2 <- p.plots %>%
  mutate(Action = ifelse(Action==2, 0, 1),
         simA   = ifelse(simA==2, 0, 1),
         Probability = as.numeric(Probability),
         Trial = as.numeric(Trial)) %>%
  filter(id == title) %>%
  pivot_longer(c(2,4), names_to = 'Type', values_to = 'Value') %>%
  ggplot()+
  annotate(geom ='rect', xmin = 18, xmax = 54, ymin = 0, ymax = 1, alpha = 0.3, fill = '#D6EAF8')+
  annotate(geom ='text', x = c(15,21), y=1.1, colour = '#17202A', label = c('Phase1', 'Phase 2'), fontface = 'bold')+
  geom_line(aes(Trial, Value, linetype = Type, color = Type))+
  geom_vline(xintercept = 18)+
  coord_cartesian(xlim = c(0, 54))+
  scale_y_continuous(breaks = c(0, 0.5, 1))+
  scale_color_manual(values = c('#2C3E50', '#C0392B'))+
  labs(x = 'Trial', y = 'Action | Action Probability',
       title = paste("ID = ",title),
       subtitle = 'ID randomly drawn from the population')+
  theme_minimal()+
  theme(        axis.text = element_text(size = 14),
                axis.title = element_text(size = 14),
                plot.title = element_text(size = 16),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.y = element_blank(),
                legend.position = c(0.75, 1.05),
                legend.direction = 'horizontal',
                legend.box.background = element_rect(colour = 'black'))

y1/y2
}
  
# Recovery Analysis -------------------------------------------------------

RecRecovery <- readMat('/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/lap_BFS4_recovery.mat')
RecFt       <- readMat('/Volumes/GoogleDrive/My Drive/Dropbox/UoQ_BI/IntentionsGameModel/lap_BFS4.mat')

