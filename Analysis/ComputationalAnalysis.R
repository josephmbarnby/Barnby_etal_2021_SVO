#Intentions Game Master file
#Joe Barnby
#Joe.barnby@kcl.ac.uk | j.barnby@uq.edu.au

rm(list=ls(all=T))

library(easystats)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(doParallel)
library(foreach)
library(R.matlab)
library(patchwork)
library(ggridges)

source("Analysis/Functions.R")
source("Analysis/Phase1SimulationR.R")

registerDoParallel(cores = 6) #choose how many cores in your processor to use for the rest of the script

#load data ####

Intentions_BothPhase <- read.csv('Data/Intentions_BothPhase.csv')
Intentions_choice    <- read.csv('Data/Intentions_Phase1.csv')
Intentions_guess     <- read.csv('Data/Intentions_Phase2.csv')

## List of CSV files for other software

Intentions_list_C <- split(Intentions_choice, Intentions_choice$id)
Intentions_list_G <- split(Intentions_guess, Intentions_guess$id)

registerDoParallel(cores = 6) #choose how many cores in your processor to use for the rest of the script

# Simulated some data for matlab functions ------------------------------------------------------

simulatedHeur <- list()
simulatedDat  <- list()
testDat  <- Intentions_list_C[[1]] %>% dplyr::select(Trial, Option1_PPT, Option1_Partner, Option2_PPT, Option2_Partner, Response)

simulatedHeur[[i]] <-  foreach(i = 1:100, .combine = rbind) %dopar% {

  genpar <- c(mysamp(1, 0, 3, 0, 10, 1000), mysamp(1, 0, 3, -10, 10, 1000))
  data = as.data.frame(Intentions_list_C[[1]][,c('Response', 'Option1_PPT', 'Option1_Partner', 'Option2_PPT', 'Option2_Partner', 'Trial')])
  synD <- Phase1Sim(genpar, data) #generate synthetic data
  synD$Trial <- 1:18
  tn = length(synD$Trial)+1
  colnames(synD) <- c('simA', 'prob1', 'Option1_PPT', 'Option1_Partner', 'Option2_PPT', 'Option2_Partner', 'Trial')
  synD <- synD %>% dplyr::select(1:7) %>% as.data.frame()
  fitAttempt <- optim(fn = Phase1Wrapper,
                      par = genpar,
                      datAr = synD,
                      scbeta0 = -1)
  loglik     <- NA;
  try( loglik<- Phase1Fit (fitAttempt$par,
                                              synD));
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

                      # Go into MatLab scripts for computational #
                      # work completed using the CBM functions  ##

# Simple BIC Compare ------------------------------------------------------
library(R.matlab)

RecData2Parm    <- readMat('Modelling/LaplaceFittedModels/FittedParameters/lap_Model1.mat')
RecData4Parm    <- readMat('Modelling/LaplaceFittedModels/FittedParameters/lap_Model2.mat')
RecData2ParmIgn <- readMat('Modelling/LaplaceFittedModels/FittedParameters/lap_Model3.mat')
RecDataShrink   <- readMat('Modelling/LaplaceFittedModels/FittedParameters/lap_Model4.mat')
RecDataZeta     <- readMat('Modelling/LaplaceFittedModels/FittedParameters/lap_Model5.mat')
RecData5ParmFav <- readMat('Modelling/LaplaceFittedModels/FittedParameters/lap_Model6.mat')
RedData6ParmEPS2<- readMat('Modelling/LaplaceFittedModels/FittedParameters/lap_Model7.mat')
RecData6ParmEPS1<- readMat('Modelling/LaplaceFittedModels/FittedParameters/lap_Model8.mat')
RecData6ParmACT <- readMat('Modelling/LaplaceFittedModels/FittedParameters/lap_Model9.mat')
RecData6ParmNULL<- readMat('Modelling/LaplaceFittedModels/FittedParameters/lap_Model10.mat')
RecDataRW4      <- readMat('Modelling/LaplaceFittedModels/FittedParameters/lap_Model11.mat')
RecDataRW5UpDown<- readMat('Modelling/LaplaceFittedModels/FittedParameters/lap_Model12.mat')
RecDataRW53lrc  <- readMat('Modelling/LaplaceFittedModels/FittedParameters/lap_Model13.mat')
RecDataRW5      <- readMat('Modelling/LaplaceFittedModels/FittedParameters/lap_Model14.mat')

RecDataIndiv <- data.frame(Log = rep(NA, 697*14),
                           Model =
                             c(
                               rep('FourParm', 697), rep('BetaOnly', 697),
                               rep('ChoiceBias', 697), rep('ChoiceBias2', 697),
                               rep('ActionBias', 697), rep('SubjIgnore', 697),
                               rep('FavBias', 697), rep('Null', 697),
                               rep('Shrink', 697), rep('Zeta', 697),
                               rep('RW4', 697), rep('RW5', 697),
                               rep('RW5UpDown', 697), rep('RW53lrc', 697)),
                           Par = rep(NA, 697*14),
                           id = rep(1:697,14),
                           Type = c(rep('Bayes', 697*10), rep('Heuristic', 697*4)),
                           ModelNum = c(rep(2, 697),rep(1, 697),rep(7, 697),rep(8, 697),
                                        rep(9, 697),rep(3, 697),rep(6, 697),rep(10, 697),
                                        rep(4, 697),rep(5, 697),rep(11, 697),rep(14, 697),
                                        rep(12, 697),rep(13, 697)))
RecDataIndiv[which(RecDataIndiv$Model=='FourParm'   ),1] <- RecData4Parm$cbm[,,1]$output[,,1]$loglik
RecDataIndiv[which(RecDataIndiv$Model=='BetaOnly'   ),1] <- RecData2Parm$cbm[,,1]$output[,,1]$loglik
RecDataIndiv[which(RecDataIndiv$Model=='ChoiceBias' ),1] <- RecData6ParmEPS1$cbm[,,1]$output[,,1]$loglik
RecDataIndiv[which(RecDataIndiv$Model=='ChoiceBias2'),1] <- RedData6ParmEPS2$cbm[,,1]$output[,,1]$loglik
RecDataIndiv[which(RecDataIndiv$Model=='ActionBias' ),1] <- RecData6ParmACT $cbm[,,1]$output[,,1]$loglik
RecDataIndiv[which(RecDataIndiv$Model=='SubjIgnore' ),1] <- RecData2ParmIgn $cbm[,,1]$output[,,1]$loglik
RecDataIndiv[which(RecDataIndiv$Model=='FavBias'    ),1] <- RecData5ParmFav $cbm[,,1]$output[,,1]$loglik
RecDataIndiv[which(RecDataIndiv$Model=='Null'       ),1] <- RecData6ParmNULL$cbm[,,1]$output[,,1]$loglik
RecDataIndiv[which(RecDataIndiv$Model=='Shrink'     ),1] <- RecDataShrink   $cbm[,,1]$output[,,1]$loglik
RecDataIndiv[which(RecDataIndiv$Model=='Zeta'       ),1] <- RecDataZeta     $cbm[,,1]$output[,,1]$loglik
RecDataIndiv[which(RecDataIndiv$Model=='RW4'        ),1] <- RecDataRW4      $cbm[,,1]$output[,,1]$loglik
RecDataIndiv[which(RecDataIndiv$Model=='RW5'        ),1] <- RecDataRW5      $cbm[,,1]$output[,,1]$loglik
RecDataIndiv[which(RecDataIndiv$Model=='RW5UpDown'  ),1] <- RecDataRW5UpDown$cbm[,,1]$output[,,1]$loglik
RecDataIndiv[which(RecDataIndiv$Model=='RW53lrc'    ),1] <- RecDataRW53lrc  $cbm[,,1]$output[,,1]$loglik
RecDataIndiv[,3] <- c(rep(4, 697), rep(2, 697), rep(5, 697), rep(6, 697), rep(6, 697), rep(2, 697),
                      rep(5, 697), rep(6, 697), rep(5, 697), rep(5, 697), rep(4, 697), rep(5, 697),
                      rep(5, 697), rep(6, 697))

RecDataIndiv %>%
  mutate(BIC = -2 * Log + log(54) * as.numeric(Par)) %>%
  pivot_wider(id_cols = id, names_from = 'Model', values_from = 'BIC') %>%
  pivot_longer(3:11, names_to = 'Model', values_to = 'BIC') %>%
  ggplot()+
  geom_point(aes(FourParm, BIC, color = Model), alpha = 0.75)+
  coord_cartesian(xlim = c(25, 100), ylim = c(25, 150))+
  geom_abline(intercept = 0, slope = 1)+
  geom_abline(intercept = 4, slope = 1,  alpha = 0.75)+
  geom_abline(intercept = -4, slope = 1, alpha = 0.75)+
  geom_abline(intercept = 10, slope = 1, alpha = 0.5)+
  geom_abline(intercept = -10, slope = 1,alpha = 0.5)+
  labs(X = 'Winning Model BIC', y = 'BIC of other models')+
  theme_classic()

RecDataIndiv %>%
  mutate(BIC = -2 * Log + log(54) * as.numeric(Par)) %>%
  ggplot()+
  geom_jitter(aes(factor(ModelNum), BIC, color = Type),
              alpha = 0.05, show.legend = F)+
  stat_summary(aes(factor(ModelNum), BIC, color = Type), key_glyph = 'pointrange')+
  geom_text(data= RecDataIndiv %>%
              mutate(BIC = -2 * Log + log(54) * as.numeric(Par)) %>%
              group_by(ModelNum) %>%
              summarise(BIC = mean(BIC)),
            aes(factor(ModelNum), round(BIC,2), label = round(BIC,2)),
            check_overlap = T, nudge_y = -5, fontface = 'bold')+
  scale_color_brewer(palette = 'Set1')+
  ggpubr::stat_compare_means(aes(factor(ModelNum), BIC, color = Type), ref.group = '2',
                             label = 'p.signif', method = 'wilcox.test', paired = T, show.legend = F)+
  labs(x = 'Model', y = 'BIC')+
  ggdist::theme_tidybayes()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = c(0.2, 0.8))

# Matlab data load --------------------------------------------

# load data from matlab
RecData <- readMat('Modelling/Fit_CBM/hbi_models3.mat')

RecData.frequency  <- RecData$cbm[,,1]$output[,,1]$model.frequency
RecData.exceed     <- RecData$cbm[,,1]$output[,,1]$exceedance.prob
RecData.p.exceed   <- RecData$cbm[,,1]$output[,,1]$protected.exceedance.prob
RecData.parameters <- RecData$cbm[,,1]$output[,,1]$parameters
RecData.groupmeans <- RecData$cbm[,,1]$output[,,1]$group.mean
RecData.grouperror <- RecData$cbm[,,1]$output[,,1]$group.hierarchical.errorbar

RecData.groupmeans[[1]][[1]] <- as.data.frame(RecData.groupmeans[[1]][[1]])
RecData.frequency            <- as.data.frame(RecData.frequency)
RecData.exceed               <- as.data.frame(RecData.exceed)
RecData.p.exceed             <- as.data.frame(RecData.p.exceed)
colnames(RecData.frequency)  <- c('B_4','B_2','B_SubjIgnore')
colnames(RecData.exceed)     <- c('B_4','B_2','B_SubjIgnore')
colnames(RecData.p.exceed)   <- c('B_4','B_2','B_SubjIgnore')
colnames(RecData.groupmeans[[1]][[1]]) <- c('alpha_m', 'beta_m', 'alpha_v', 'beta_v')#'kappa'
colnames(RecData.parameters[[1]][[1]]) <- c('alpha_m', 'beta_m', 'alpha_v', 'beta_v')#'kappa'
colnames(RecData.grouperror[[1]][[1]]) <- c('alpha_m', 'beta_m', 'alpha_v', 'beta_v')#'kappa'
# Transform between 0 and positive integer
RecData.groupmeans[[1]][[1]][,1] <- 15*(1/(1+exp(-RecData.groupmeans[[1]][[1]][,1])))
RecData.groupmeans[[1]][[1]][,3] <- exp(RecData.groupmeans[[1]][[1]][,3])
RecData.groupmeans[[1]][[1]][,4] <- exp(RecData.groupmeans[[1]][[1]][,4])

RecData.parameters[[1]][[1]][,1] <- 15*(1/(1+exp(-RecData.parameters[[1]][[1]][,1])))
RecData.parameters[[1]][[1]][,3] <- exp(RecData.parameters[[1]][[1]][,3])
RecData.parameters[[1]][[1]][,4] <- exp(RecData.parameters[[1]][[1]][,4])

#augment data
groupplot <- RecData.groupmeans[[1]][[1]] %>%
  dplyr::select(alpha_m, alpha_v, beta_m, beta_v) %>%
  pivot_longer(1:4, names_to = 'parameter', values_to = 'value') %>%
  mutate(sd = RecData.grouperror[[1]][[1]] %>%
           as.data.frame() %>%
           dplyr::select(alpha_m, alpha_v, beta_m, beta_v) %>%
           t(),
         type = c(rep('alpha', 2), rep('beta', 2)),
         metric = c('Mean', 'Var', 'Mean', 'Var')) %>%
  dplyr::rename(sd = 3)
RecData.frequency %>%
  as.data.frame() %>%
  pivot_longer(1:3, names_to = 'model', values_to = 'metric') %>%
  mutate(type = 'Freqency') -> freq
RecData.exceed %>%
  as.data.frame() %>%
  pivot_longer(1:3, names_to = 'model', values_to = 'metric') %>%
  mutate(type = 'Ex. Probability') -> exprob
RecData.p.exceed %>%
  as.data.frame() %>%
  pivot_longer(1:3, names_to = 'model', values_to = 'metric') %>%
  mutate(type = 'Ex. Probability (Protected)') -> p.exprob
Rec.data.modelcomp <- rbind(freq, exprob, p.exprob)

groupplot.parms1 <- RecData.parameters[[1]][[1]] %>%
  as.data.frame() %>%
  mutate(id = 1:697) %>%
  dplyr::select(alpha_m, alpha_v, beta_m, beta_v,id) %>%
  pivot_longer(1:4, names_to = 'parameter', values_to = 'value') %>%
  mutate(type = ifelse(parameter %in% c('alpha_m', 'beta_m'), 'Mean', 'Var'),
         parameter = ifelse(parameter %in% c('alpha_m', 'alpha_v'), 'alpha', 'beta'))

groupplot.parms2 <- RecData.parameters[[1]][[1]]%>%
  as.data.frame() %>%
  mutate(id = 1:697)

Intentions_splice <- Intentions_guess %>% dplyr::select(
  id, PartnerPolicy, Control1 = Control, Sum, Game, Persec, ICARTot, Age, HI, SI, Agency, Sex
) %>%
  distinct()

Intentions_splice <- plyr::join(Intentions_splice, Intentions_choice %>% dplyr::select(Control ,id) %>% rename(Control2 = 'Control'), by = 'id')
Intentions_splice <- Intentions_splice %>%mutate(Control = Control1 + Control2)%>%dplyr::select(1:2, 4:12, 14)

indivParms <- plyr::join(Intentions_splice %>% mutate(PartnerPolicy = ifelse(PartnerPolicy == "Competative", "Competitive", PartnerPolicy)), groupplot.parms1, by= 'id') %>%
  distinct()
indivParmsB <- plyr::join(Intentions_splice %>% mutate(PartnerPolicy = ifelse(PartnerPolicy == "Competative", "Competitive", PartnerPolicy)), groupplot.parms2, by= 'id') %>%
  distinct()

# Simplex Plot ------------------------------------------------------------

SimplexR <- readMat('Data/SimplexPlot.mat')
SimplexR <- SimplexR$sumProbF

for(i in 1:length(SimplexR[1,,1])){
  for (k in 1:length(SimplexR[1,,1])){
    SimplexR[i,k,] <- SimplexR[i,k,]/(SimplexR[i,k,1]+SimplexR[i,k,2]+SimplexR[i,k,3])
  }
}

res = 100
alphares = 0.5
betares = res*2
SimplexC <- as.data.frame(SimplexR[,,1]); SimplexC$alpha_m <- seq(0, res, alphares); SimplexC$Policy <- 'Competitive'
SimplexP <- as.data.frame(SimplexR[,,2]); SimplexP$alpha_m <- seq(0, res, alphares); SimplexP$Policy <- 'Prosocial'
SimplexI <- as.data.frame(SimplexR[,,3]); SimplexI$alpha_m <- seq(0, res, alphares); SimplexI$Policy <- 'Individualist'

Simplex <- rbind(SimplexC, SimplexP, SimplexI)
colnames(Simplex) <- c(-100:100, 'alpha_m', 'Policy')
Simplex <- Simplex %>%
  pivot_longer(1:201, 'beta_m', values_to = 'Probability') %>%
  pivot_wider(id_cols = c(alpha_m, beta_m), names_from = Policy, values_from = Probability)

Scale01 <- function(x){(x-min(x))/(max(x)-min(x))}

SPlot <- ggplot(Simplex, aes(as.numeric(alpha_m), as.numeric(beta_m)))+
  geom_point(aes(color = Scale01(Prosocial)), alpha = 0.75)+
  scale_color_gradient2(low = '#1B9E77', mid = '#D95F02', high = '#7570B3',midpoint = 0.5,
                        breaks = c(0, 0.5, 1),
                        labels = c('Competitive', 'Individualist', 'Prosocial'))+
  geom_point(data = groupplot.parms2, aes(alpha_m*(100/15), beta_m*(100/20)))+
  #geom_density_2d(data = groupplot.parms2, aes(alpha_m*(100/15), beta_m*(100/20)))+
  #geom_smooth(data = groupplot.parms2, aes(alpha_m*(100/15), beta_m*(100/20)), method = 'lm')+
  scale_x_continuous(breaks = c(0, 33, 66, 100), labels = c(0, 5, 10, 15))+
  scale_y_continuous(breaks = c(-100, -50, 0, 50, 100), labels = c(-20, -10, 0, 10, 20))+
  guides(color = guide_colourbar(barwidth = 30, barheight = 1))+
  labs(x = expression(paste(alpha[ppt]^m)), y = expression(paste(beta[ppt]^m)))+
  theme_void()+
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = 'none',
        legend.direction = 'horizontal',
        legend.title = element_blank(),
        plot.margin = margin(1,3,1,3, unit = 'cm'))
SPlot

# Viz model distributions -------------------------------------------------

p_alpha <- ggplot(groupplot.parms2, aes(alpha_m, alpha_v))+
  geom_point()+
  geom_smooth(formula = y ~ x + I(x^2), method = "lm")+
  coord_cartesian(ylim = c(0, 10), xlim = c(0, 15))+
  scale_x_continuous(limits = c(0, 15), expand = c(0,0))+
  scale_y_continuous(limits = c(0, 10), expand = c(0,0))+
  labs(x = expression(paste(alpha[ppt]^m)), y = expression(paste(alpha^sigma)))+
  tidybayes::theme_tidybayes()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 20))

void <- ggplot()+geom_point(aes(1,1), colour="white")+
  theme(axis.ticks=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

top1<-ggplot(groupplot.parms2, aes(alpha_m))+
  geom_density(fill = "#0097A7")+
  geom_segment(aes(y = dnorm(alpha_m), x = 11.01,xend=11.01, yend=0.1375)) +
  scale_y_continuous(limits = c(0,0.195)) +
  scale_x_continuous(limits = c(0,15), expand = c(0,0)) +
  coord_cartesian(xlim = c(0, 15))+
  theme_void()
side1 <- ggplot(groupplot.parms2, aes(alpha_v))+
  geom_density(fill = '#80DEEA')+
  geom_segment(aes(y = dnorm(alpha_m), x = 3.83,xend=3.83, yend=0.57), size = 1) +
  scale_y_continuous(limits = c(0,0.6)) +
  scale_x_continuous(limits = c(0,10), expand = c(0,0)) +
  coord_flip(xlim = c(0, 10))+
  theme_void()

margPlot1 <- "
AAAAAAD
BBBBBBC
BBBBBBC
BBBBBBC
BBBBBBC
"
p_alpha_edit2 <- patchwork::wrap_plots(a = top1, b = p_alpha, c = side1, d = void, design = margPlot1)
p_alpha_edit2

p_beta <- ggplot(groupplot.parms2, aes(beta_m, beta_v))+
  geom_point()+
  geom_smooth(method = "lm")+
  coord_cartesian(ylim = c(0, 10), xlim = c(-20, 20))+
  scale_x_continuous(limits = c(-20, 20), expand = c(0,0))+
  scale_y_continuous(limits = c(0, 10), expand = c(0,0))+
  labs(x = expression(paste(beta[ppt]^m)), y = expression(paste(beta^sigma)))+
  tidybayes::theme_tidybayes()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 20))

top2 <- ggplot(groupplot.parms2, aes(beta_m))+
  geom_density(fill = "#303F9F")+
  #geom_vline(xintercept = -6.08, size = 1)+
  geom_segment(aes(y = dnorm(beta_m), x = -6.08,xend= -6.08, yend=0.01)) +
  scale_y_continuous(limits = c(0,0.09)) +
  scale_x_continuous(limits = c(-20, 20), expand = c(0,0))+
  coord_cartesian(xlim = c(-20, 20))+
  theme_void()
side2 <- ggplot(groupplot.parms2, aes(beta_v))+
  geom_density(fill = '#9575CD')+
  #geom_vline(xintercept = 4.06, size = 1)+
  geom_segment(aes(y = dnorm(beta_v), x = 4.06, xend=4.06, yend=0.25)) +
  scale_y_continuous(limits = c(0,0.3)) +
  scale_x_continuous(limits = c(0, 10), expand = c(0,0))+
  coord_flip(xlim = c(0, 10))+
  theme_void()

margPlot2 <- "
AAAAAAD
BBBBBBC
BBBBBBC
BBBBBBC
BBBBBBC
"
p_beta_edit2 <- patchwork::wrap_plots(a = top2, b = p_beta, c = side2, d = void, design = margPlot2)
p_beta_edit2

P2a <- groupplot.parms2 %>%
  pivot_longer(c(1,3,4), names_to = 'Metric', values_to = 'Value') %>%
  ggplot() +
  geom_vline(xintercept = 0, alpha = 0.5)+
  ggridges::geom_density_ridges(aes(Value, fct_rev(Metric), fill = Metric, point_color = Metric), alpha = 0.7, show.legend = F,
                                position = position_points_jitter(height = 0, yoffset = -0.05), jittered_points = TRUE,
                                point_shape = '|', point_size = 3, point_alpha = 1, scale = .9, quantile_lines = TRUE, quantiles = 2,
                                vline_size = 2)+
  scale_fill_manual(values = c('#0097A7', '#80DEEA', '#9575CD')) +
  scale_discrete_manual(aesthetics = "point_color", values = c('#0097A7', '#80DEEA', '#9575CD')) +
  scale_y_discrete(labels=c(
    'alpha_m' = expression(paste(alpha[ppt]^'m')),
    'alpha_v' = expression(paste(alpha^sigma)),
    'beta_v' = expression(paste(beta^sigma))
  )) +
  scale_x_continuous(limits = c(0,15))+
  theme_ridges()+
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        strip.text.x = element_blank())

P2b <- groupplot.parms2 %>%
  pivot_longer(c(2), names_to = 'Metric', values_to = 'Value') %>%
  ggplot() +
  geom_vline(xintercept = 0, alpha = 0.5)+
  ggridges::geom_density_ridges(aes(Value, factor(Metric), fill = Metric, point_color = Metric), alpha = 0.7, show.legend = F,
                                position = position_points_jitter(height = 0, yoffset = -0.05), jittered_points = TRUE,
                                point_shape = '|', point_size = 3, point_alpha = 1, quantile_lines = TRUE, quantiles = 2, scale = 4,
                                vline_size = 2)+
  #geom_label(x = 2.5, y = 1.4, label = paste(11.1,'[', 0.04,']'), fill = '#0097A7')+
  scale_fill_manual(values = c('#303F9F')) +
  scale_discrete_manual(aesthetics = "point_color", values = c('#303F9F')) +
  scale_y_discrete(labels=c(
    'beta_m' = expression(paste(beta[ppt]^m))
  )) +
  theme_ridges()+
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        strip.text.x = element_blank())
design1 <- "
AAAA
BBBB
BBBB
"
P2 <- wrap_plots(A = P2b, B = P2a, design = design1)

P3 <- ggplot(Rec.data.modelcomp)+
  geom_bar(aes(model, metric, fill = type), stat = 'identity', color="black", width = .4, position = 'dodge') +
  scale_fill_brewer(palette = 'Set1')+
  scale_x_discrete(labels=c(
    'B_SubjIgnore' = expression(paste("[",alpha[ppt]^m, beta[ppt]^m,"]")),
    'B_2' = expression(paste("[",beta[ppt]^m,beta^sigma,"]")),
    'B_4' = expression(paste("[",alpha[ppt]^m,alpha^sigma, beta[ppt]^m,beta^sigma,"]"))
  ))+
  labs(y = 'Model Responsibility')+
  theme_minimal()+
  theme(axis.title = element_blank(),
        legend.position = c(0.2, 0.7),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        strip.text.x = element_text(size = 18, face = 'bold'),
        legend.background = element_rect(colour = 'black'))

topdesign = "
  AABBCC
"

Ptop <- patchwork::wrap_plots(A = P3, B = p_alpha_edit2, c = p_beta_edit2, design = topdesign)
Ptop

# Check phase 2 parms against phase 1 ----------------------------------------

Phase1 <- readMat('Modelling/LaplaceFittedModels/FittedParameters/lap_Phase1Estimate.mat')
Phase2 <- readMat('Modelling/LaplaceFittedModels/FittedParameters/lap_Model2.mat')

Phase1_parms <-   Phase1$cbm[,,1]$output[,,1]$parameters
Phase1_parms[,1] <- (1/(1+exp(-Phase1_parms[,1])))*15

Phase2_parms <-   Phase2$cbm[,,1]$output[,,1]$parameters
Phase2_parms[,1] <- (1/(1+exp(-Phase2_parms[,1])))*15

parmCheck <- Phase1_parms %>%
  as.data.frame() %>%
  rename(alpha_phase1 = 1,
         beta_phase1 = 2) %>%
  mutate(id = 1:697) %>%
  plyr::join(Phase2_parms %>%
               as.data.frame() %>%
               rename(alpha_phase2 = 1,
                      beta_phase2 = 2) %>%
               mutate(id = 1:697), by = 'id')

ggcorrplot::ggcorrplot(cor(parmCheck %>% dplyr::select(1:2, 4:5)), lab = T, type = 'upper')
cor.test(parmCheck$alpha_phase1, parmCheck$alpha_phase2)
cor.test(parmCheck$beta_phase1, parmCheck$beta_phase2)

parmCheck %>%
  pivot_longer(c(1, 4), names_to = 'alpha', values_to = 'alpha_value') %>%
  pivot_longer(c(1, 3), names_to = 'beta', values_to = 'beta_value') %>%
  mutate(alpha = ifelse(alpha == 'alpha_phase2', 'Phase 1 + 2', 'Phase1 Only'),
         beta = ifelse(beta == 'beta_phase2', 'Phase 1 + 2', 'Phase1 Only')) -> parmCheck.P

ggplot(parmCheck) +
  geom_point( aes(alpha_phase1, alpha_phase2))+
  geom_smooth(aes(alpha_phase1, alpha_phase2))+

  ggplot(parmCheck) +
  geom_point( aes(beta_phase1, beta_phase2))+
  geom_smooth(aes(beta_phase1, beta_phase2))+

  ggplot(parmCheck)+
  geom_density(aes(alpha_phase1), color = 'grey')+
  geom_density(aes(alpha_phase2))+
  ggplot(parmCheck)+
  geom_density(aes(beta_phase1), color = 'grey')+
  geom_density(aes(beta_phase2))

# Model Error and assessment -------------------------------------------------------

# Load data and wrangle
RecError <- readMat('Modelling/LaplaceFittedModels/SimulatedData/modelError_Generative.mat')

loglikstats <- data.frame(lik1 = RecError$lik1,
                          lik2 = RecError$lik2,
                          ll   = RecError$F,
                          id   = 1:697)

controlling <- plyr::join(loglikstats, indivParms %>%
                            mutate(PartnerPolicy == ifelse(PartnerPolicy == 'Competative', 'Competitive', PartnerPolicy)), by = 'id')

probabilities   <- matrix(NA, nrow = 697, ncol = 54)
probabilities.1 <- matrix(NA, nrow = 697, ncol = 18)
probabilities.2 <- matrix(NA, nrow = 697, ncol = 36)
congruency      <- matrix(NA, nrow = 697, ncol = 36)
pptprob1        <- matrix(NA, nrow = 697, ncol = 36)
simA            <- matrix(NA, nrow = 697, ncol = 54)
simAFix         <- matrix(NA, nrow = 697, ncol = 36)

for (i in 1:697){
  probabilities[i,] <- as.numeric(RecError$action[[i]][[1]])
  probabilities.1[i,] <- as.numeric(RecError$prob1[[i]][[1]])
  probabilities.2[i,] <- as.numeric(RecError$prob2[[i]][[1]])
  pptprob1[i,]        <- as.numeric(RecError$pptprob1[[i]][[1]])
  congruency[i,]      <- as.numeric(t(RecError$cong[[i]][[1]]))
  simA[i,]   <- as.numeric(RecError$simA[[i]][[1]][1:54,])
  simAFix[i,]<- as.numeric(RecError$simAFix[[i]][[1]][19:54,])
}

probabilities <- as.data.frame(probabilities)
probabilities.1 <- as.data.frame(probabilities.1)
probabilities.2 <- as.data.frame(probabilities.2)
pptprob1 <- as.data.frame(pptprob1)
simA <- as.data.frame(simA)
simAFix <- as.data.frame(simAFix)
congruency <- as.data.frame(congruency)
congruency$ID <- 1:697
probabilities$ID <- 1:697
simA$ID <- 1:697
simAFix$ID <- 1:697
pptprob1$ID <- 1:697
probabilities.1$ID <- 1:697
probabilities.2$ID <- 1:697
colnames(probabilities) <- c(1:54, 'id')
colnames(congruency) <- c(1:36, 'id')
colnames(simA) <- c(1:54, 'id')
colnames(simAFix) <- c(1:36, 'id')
colnames(probabilities.1) <- c(1:18, 'id')
colnames(probabilities.2) <- c(19:54, 'id')
colnames(pptprob1) <- c(19:54, 'id')

probabilities_p <- cbind(probabilities.1, probabilities.2[,1:36])

probabilities %>%
  pivot_longer(1:54, names_to = 'Trial', values_to = 'Action') -> probabilities.edit
probabilities_p %>%
  dplyr::select(1:18, 20:55, id) %>%
  pivot_longer(1:54, names_to = 'Trial', values_to = 'Probability') -> probabilities.p.edit
pptprob1 %>%
  pivot_longer(1:36, names_to = 'Trial', values_to = 'prob1_valppt') -> pptprob1.edit
simA %>%
  pivot_longer(1:54, names_to = 'Trial', values_to = 'simA') -> simA.edit
simAFix %>%
  pivot_longer(1:36, names_to = 'Trial', values_to = 'simAFixed') -> simAFix.edit

non_listsimA <- as.data.frame(matrix(NA, nrow = 697, ncol = 55))
for (i in 1:697){
  non_listsimA[i,1:54] <- RecError$simA[[i]][[1]]
  non_listsimA[i,55] <- i
}
colnames(non_listsimA) <- c(1:54, 'id')

non_listsimA %>%
  pivot_longer(1:54, names_to = 'Trial', values_to = 'simA') %>%
  plyr::join(Intentions_BothPhase, by = c('id', 'Trial')) %>%
  plyr::join(indivParms, by = 'id') %>%
  mutate(id = as.numeric(id)) %>%
  na.omit() %>%
  group_by(id) %>%
  dplyr::select(-value, -type, -parameter) %>%
  distinct() %>%
  group_by(id) %>%
  mutate(CorrectSim = ifelse(simA == as.numeric(Answer), 1, 0),
         Match = ifelse(simA == Response, 1, 0),
         CorrectSim = sum(CorrectSim)) %>%
  plyr::join(controlling %>% dplyr::select(id, ll, lik1, lik2), by = 'id') %>%
  plyr::join(simAFix.edit %>% group_by(id) %>% mutate(Trial = 19:54), by = c('id', 'Trial')) %>%
  plyr::join(indivParmsB %>% dplyr::select(alpha_v, beta_v, alpha_m, beta_m, id), by = 'id') %>%
  mutate(CorrectFix = ifelse(simAFixed == Answer, 1, 0)) %>%
  group_by(id) %>%
  distinct() %>%
  mutate(CorrectFix = sum(CorrectFix)) %>%
  distinct() -> testdf2

p.plots <- cbind(probabilities.edit, probabilities.p.edit, simA.edit)

p.plots <- p.plots %>% dplyr::select(1, 3, 5, 6, 9) %>%
  plyr::join(controlling %>% dplyr::select(1:14), by = 'id') %>%
  mutate(Trial = as.numeric(Trial),
         Phase = ifelse(Trial < 19, 1, 2)) %>%
  distinct()

# Plot data

llPlot_t <- ggplot(p.plots)+
  geom_vline(xintercept = 0.5)+
  geom_density_ridges(aes(Probability, factor(Phase)), fill = 'light grey', scale = .95)+
  geom_density_ridges(aes(Probability, factor(Phase), fill = PartnerPolicy, color = PartnerPolicy),
                      alpha = 0.7, scale = .95, rel_min_height = .01)+
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3"))+
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3"))+
  labs(x = 'Probability of choice for each trial')+
  scale_discrete_manual("point_color", values = c("#1B9E77", "#D95F02", "#7570B3"), guide = "none") +
  scale_y_discrete(labels = c("Phase1", "Phase 2"))+
  scale_x_continuous(breaks = c(0, 0.5, 1))+
  theme_ridges()+
  theme(legend.position = c(0, 1),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_text(hjust = 0.5))
llPlot_b <- ggplot(p.plots %>% pivot_longer(6:7, names_to = 'llPhase', values_to = 'Values') %>%
                     dplyr::select(llPhase, Values, PartnerPolicy, id) %>% distinct())+
  geom_density_ridges(aes(Values, factor(llPhase)), fill = 'light grey', scale = .95)+
  geom_density_ridges(aes(Values, factor(llPhase), fill = PartnerPolicy, color = PartnerPolicy),
                      alpha = 0.7, scale = .95, rel_min_height = .01)+
  annotate("segment", x = log(0.5)*36, xend = log(0.5)*36, y = 0, yend = 2, colour = "black")+
  annotate("text", x = (log(0.5)*36)+6, y = 1.5,  colour = "black", label = 'log(0.5)*36')+
  annotate("segment", x = log(0.5)*18, xend = log(0.5)*18, y = 0, yend = 1, colour = "black")+
  annotate("text", x = (log(0.5)*18)+6, y = 0.5,  colour = "black", label = 'log(0.5)*18')+
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3"))+
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3"))+
  labs(x = 'Loglikelihood for each participant')+
  scale_discrete_manual("point_color", values = c("#1B9E77", "#D95F02", "#7570B3"), guide = "none") +
  scale_y_discrete(labels = c("Phase1", "Phase 2"))+
  theme_ridges()+
  theme(legend.position = 'none',
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_text(hjust = 0.5))

llPlot <- llPlot_t|llPlot_b
llPlot

#set.seed(123)
extractppt <- sample(1:697,1)
y2 <- p.plots %>%
  mutate(Action = ifelse(Action==2, 0, 1),
         simA   = ifelse(simA==2, 0, 1),
         Probability = ifelse(Action == 0, 1-Probability, Probability),
         Trial = as.numeric(Trial)) %>%
  filter(id == extractppt) %>%
  pivot_longer(c(2,4), names_to = 'Type', values_to = 'Value') %>%
  ggplot()+
  annotate(geom ='rect', xmin = 18, xmax = 54, ymin = 0, ymax = 1, alpha = 0.3, fill = '#D6EAF8')+
  annotate(geom ='text', x = c(15,21), y=1.05, colour = '#17202A', label = c('Phase1', 'Phase 2'), fontface = 'bold')+
  geom_line(aes(Trial, Value, linetype = Type, color = Type))+
  geom_vline(xintercept = 18)+
  coord_cartesian(xlim = c(0, 54), ylim = c(0,1), clip='off')+
  scale_y_continuous(breaks = c(0, 0.5, 1))+
  scale_color_manual(values = c('#2C3E50', '#C0392B', '#17202A'))+
  labs(x = 'Trial', y = 'p(Choice=1)',
       subtitle = 'ID randomly drawn from the population'
  )+
  theme_minimal()+
  theme(        axis.text = element_text(size = 14),
                axis.text.y = element_text(size = 12),
                axis.title = element_text(size = 14),
                plot.title = element_text(size = 16),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.y = element_blank(),
                legend.position = c(0.85, -0.05),
                legend.direction = 'horizontal',
                legend.title = element_blank(),
                legend.box.background = element_rect(colour = 'black'))
y2

# Recovery Analysis -------------------------------------------------------

RecRecovery <- readMat('Modelling/LaplaceFittedModels/RecoveredParameters/lap_Model2_recovery.mat')
RecReal     <- readMat('Modelling/LaplaceFittedModels/FittedParameters/lap_Model2.mat')

RealParms <- RecReal$cbm[,,1]$output[,,1]$parameters %>%
  as.data.frame() %>%
  mutate(id = 1:697)

RecoveredParms <- RecRecovery$cbm[,,1]$output[,,1]$parameters %>%
  as.data.frame() %>%
  mutate(id = 1:697)

Fit_Rec <- plyr::join(RecoveredParms[,1:5], RealParms[,1:5], by = 'id')
Fit_Rec <- plyr::join(Fit_Rec, indivParmsB, by = 'id')

FitRecCor <- cor(Fit_Rec[,c(6:9, 1:4)])
FitRecPCor <- ggcorrplot::cor_pmat(Fit_Rec[,c(6:9, 1:4)])
P5 <- ggcorrplot::ggcorrplot(FitRecCor[1:4, 5:8], lab = T,
                             colors = c('#3498DB', '#E5E8E8', '#E74C3C'), p.mat = FitRecPCor[1:4, 5:8])+
  scale_x_discrete(labels = c(
    expression(paste(alpha^m, ' Rec')),
    expression(paste(beta^m , ' Rec')),
    expression(paste(alpha^sigma, ' Rec')),
    expression(paste(beta^sigma , ' Rec'))
  ))+
  scale_y_discrete(labels = c(
    expression(paste(alpha^m, ' Fit')),
    expression(paste(beta^m ,  ' Fit')),
    expression(paste(alpha^sigma, ' Fit')),
    expression(paste(beta^sigma ,  ' Fit'))
  ))+
  theme(legend.position = 'none')

P5

# Figure 3 -----------------------------------------------------------

design <- "AAAAAAA
           BBBBBCC
           DDDDDEE"

patchwork::wrap_plots(A = Ptop, B = llPlot, C = SPlot + theme(legend.position = 'none'), D = y2, E = P5, design = design)

# Core Regression Models -------------------------------------------------------------

library(lme4)
ControlDF <- plyr::join(indivParmsB, testdf2 %>% dplyr::select(CorrectFix, CorrectSim, id), by = 'id') %>% distinct()
ControlDF <- ControlDF %>% mutate(PartnerPolicy = factor(PartnerPolicy, levels = c('Competitive', 'Individualist','Prosocial')))
#Total correct score by parameters and covariates
model.compare(lm(scale(Sum) ~ scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) + offset(scale(CorrectFix))+
                   scale(Persec) + scale(ICARTot) + scale(Age) + Sex + Control,
                 data = ControlDF,
                 na.action = na.fail))

#Persecutory Ideation by parameters and covariates
model.compare(lm(scale(Persec) ~ scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) + scale(CorrectFix)+
                   scale(ICARTot) + scale(Age) + Sex + Control,
                 data = ControlDF,
                 na.action = na.fail))

#ICAR by parameters and covariates
model.compare(lm(scale(ICARTot) ~ scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) + scale(CorrectFix)+
                   scale(Persec) + scale(Age) + Sex + Control,
                 data =ControlDF,
                 na.action = na.fail))

#HI by parameters and covariates
model.compare(lm(scale(HI) ~ scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) + scale(CorrectFix) + PartnerPolicy+
                   scale(Persec) + scale(ICARTot) + scale(Age) + Sex + Control,
                 data = ControlDF,
                 na.action = na.fail))

#SI by parameters and covariates
model.compare(lm(scale(SI) ~ scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) + scale(CorrectFix) + PartnerPolicy+
                   scale(Persec) + scale(ICARTot) + scale(Age) + Sex + Control,
                 data = ControlDF,
                 na.action = na.fail))

#For each sum
data1c = ControlDF %>% filter(PartnerPolicy=='Competitive')
model.compare(lm(scale(Sum) ~ scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) + scale(CorrectFix) +
                   scale(Persec) + scale(ICARTot) + Sex + scale(Age) + Control,
                 data = data1c,
                 na.action = na.fail))

data2c = ControlDF %>% filter(PartnerPolicy=='Individualist')
model.compare(lm(scale(Sum) ~ scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) + scale(CorrectFix) +
                   scale(Persec) + scale(ICARTot) + Sex + scale(Age) + Control,
                 data = data2c,
                 na.action = na.fail))

data3c = ControlDF %>% filter(PartnerPolicy=='Prosocial')
model.compare(lm(scale(Sum) ~ scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) + scale(CorrectFix) +
                   scale(Persec) + scale(ICARTot) + Sex + scale(Age) + Control,
                 data = data3c,
                 na.action = na.fail))

pAll <- data.frame(
  Estimate = c(-0.026, 0.180, 0.414, 0.870, 0.740, 0.171,-0.038, 0.097, 0.153, 0.002,-0.590, 0.767),
  conf.low = c(-0.110, 0.119, 0.356, 0.810, 0.631, 0.077,-0.111, 0.021, 0.089,-0.049,-0.695, 0.704),
  conf.high =c( 0.013, 0.241, 0.472, 0.930, 0.840, 0.270, 0.040, 0.173, 0.217, 0.083,-0.484, 0.830),
  Significant = c(F, T, T, T, T, T, F, T, T, F, T, T),
  Var = rep(c('alpha_m', 'alpha_v', 'beta_m', 'beta_v'), 3),
  Policy = c(rep('Competitive', 4), rep('Individualist', 4), rep('Prosocial', 4))

)

pA <- ggplot(pAll)+
  geom_bar(aes(Var, Estimate, fill = Policy, alpha = Significant), color = 'black', stat ='identity', position = 'dodge')+
  geom_errorbar(aes(Var, Estimate,group = Policy, ymin= conf.low, ymax=conf.high),
                color = 'black', stat ='identity', position = 'dodge')+
  labs(title = 'Total Correct Predictions',
       subtitle = '(Controlling for Age, Sex, ICAR Score, Persecutory Ideation, Participant-Partner Baseline Similarity, Task Comprehension)',
       y = expression(paste(beta, ' weight | 95% Confidence Interval'))
  )+
  scale_x_discrete(labels = c(expression(alpha[ppt]^m), expression(alpha^sigma),expression(beta[ppt]^m),expression(beta^sigma)
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
pA

# Congruency analysis -----------------------------------------------------

congruency %>%
  pivot_longer(1:36, 'Trial', values_to = 'Con') %>%
  group_by(id) %>%
  mutate(ConSum = sum(Con)) %>%
  plyr::join(indivParmsB, by = 'id') -> congruency

C <- congruency %>%
  dplyr::select(ConSum, id, HI, SI, Persec, ICARTot, Age, Sex, Control, PartnerPolicy,
                alpha_m, beta_m, beta_v, alpha_v) %>%
  plyr::join(testdf2 %>% dplyr::select(id, CorrectFix), by = 'id') %>%
  distinct() %>%
  ungroup() %>%
  mutate(HI = scale(HI), SI = scale(SI),
         Persec = scale(Persec), ICARTot = scale(ICARTot),
         Age = scale(Age), ConSum = scale(ConSum), CorrectFix = scale(CorrectFix))

#change partner factor level if required
C <- C %>% mutate(PartnerPolicy=factor(PartnerPolicy, levels = c('Individualist', 'Competitive', 'Prosocial')))

model.compare(lm(HI ~ ConSum + CorrectFix + PartnerPolicy + Age + Sex + Persec + ICARTot + Control, data = C, na.action = na.fail))

C1 <- C %>% filter(PartnerPolicy == 'Prosocial')
model.compare(lm(HI ~ ConSum + CorrectFix+ Age + Sex + Persec + ICARTot + Control, data = C1, na.action = na.fail))
C2 <- C %>% filter(PartnerPolicy == 'Individualist')
model.compare(lm(HI ~ ConSum + CorrectFix+ Age + Sex + Persec + ICARTot + Control, data = C2, na.action = na.fail))
C3 <- C %>% filter(PartnerPolicy == 'Competitive')
model.compare(lm(HI ~ ConSum + CorrectFix+ Age + Sex + Persec + ICARTot + Control, data = C3, na.action = na.fail))

model.compare(lm(SI ~ ConSum + CorrectFix + PartnerPolicy + Age + Sex + Persec + ICARTot + Control, data = C, na.action = na.fail))

C1b <- C %>% filter(PartnerPolicy == 'Prosocial')
model.compare(lm(SI ~ ConSum + CorrectFix + Age + Sex + Persec + ICARTot + Control, data = C1b, na.action = na.fail))
C2b <- C %>% filter(PartnerPolicy == 'Individualist')
model.compare(lm(SI ~ ConSum + CorrectFix + Age + Sex + Persec + ICARTot + Control, data = C2b, na.action = na.fail))
C3b <- C %>% filter(PartnerPolicy == 'Competitive')
model.compare(lm(SI ~ ConSum + CorrectFix + Age + Sex + Persec + ICARTot + Control, data = C3b, na.action = na.fail))

ConReg <- data.frame(
  Estimate = c(-0.14, 0.06, -0.23, 0.00, 0.23, -0.08),
  UCI    = c(  -0.04, 0.37, -0.06, 0.00, 0.37,  0.05),
  LCI    = c(  -0.23,-0.08, -0.40, 0.00, 0.08, -0.61),
  p      = c(T, F, T, F, T, F),
  Policy = c('Prosocial', 'Individualist', 'Competitive', 'Prosocial', 'Individualist', 'Competitive'),
  Attribute = c(rep('Harmful Intent', 3), rep('Self Interest', 3)),
  alpha  = c(1, 0.2, 1, 0, 1, 0.2)

)

AttributeCon <- ggplot(ConReg)+
  geom_bar(aes(Policy, Estimate, fill = Policy, alpha = alpha), color = 'black', stat ='identity', position = 'dodge')+
  geom_errorbar(aes(Policy, Estimate,group = Policy, ymin= LCI, ymax=UCI),
                color = 'black', stat ='identity', position = 'dodge')+
  labs(title = 'Attributions ~ Total Congruency',
       y = expression(paste(beta, ' weight | 95% Confidence Interval'))
  )+
  facet_wrap(~ Attribute, strip.position = 'bottom')+
  scale_fill_brewer(name = 'Partner Policy', palette = 'Dark2')+
  scale_alpha_continuous(guide = F)+
  coord_cartesian(ylim = c(-1, 1))+
  theme_minimal()+
  theme(legend.position = c(0.25, 0.8),
        legend.direction = 'vertical',
        legend.box.background = element_rect(colour = 'black'),
        legend.text = element_text(size = 11),
        axis.text.y = element_text(size = 14),
        strip.text.x = element_text(size =16, vjust = -1),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16),
        panel.grid.major.x = element_blank())

# Figure 4 ----------------------------------------------------------------

design4 <- "
        AABB
"
patchwork::wrap_plots(A = pA, b = AttributeCon, design = design4) & plot_annotation(tag_levels = "A")

# Generative ability ------------------------------------------------------

GenReg = testdf2 %>% dplyr::select(id, Sum, ICARTot, CorrectFix, CorrectSim, alpha_m, alpha_v,beta_m, beta_v, PartnerPolicy, Age, Persec, Sex, Control) %>% distinct()
model.compare(lm(scale(CorrectSim) ~ scale(ICARTot) + scale(CorrectFix) + scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) +
                   scale(Persec) + Age + Sex + PartnerPolicy, data = GenReg, na.action = na.fail))

GenRegP = GenReg %>% filter(PartnerPolicy == 'Prosocial')
model.compare(lm(scale(CorrectSim) ~ scale(ICARTot) + scale(CorrectFix) + scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) +
                   scale(Persec) + Age + Sex + Control, data = GenRegP, na.action = na.fail))

GenRegI = GenReg %>% filter(PartnerPolicy == 'Individualist')
model.compare(lm(scale(CorrectSim) ~ scale(ICARTot) + scale(CorrectFix) + scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) +
                   scale(Persec) + Age + Sex + Control, data = GenRegI, na.action = na.fail))

GenRegC = GenReg %>% filter(PartnerPolicy == 'Competitive')
model.compare(lm(scale(CorrectSim) ~ scale(ICARTot) + offset(scale(CorrectFix)) + scale(alpha_m) + scale(alpha_v) + scale(beta_m) + scale(beta_v) +
                   scale(Persec) + Age + Sex + Control, data = GenRegC, na.action = na.fail))

# Model permutation test --------------------------------------------------

RecPermute <- readMat('Modelling/LaplaceFittedModels/SimulatedData/modelError_Permute.mat')
PermutedParms <- as.data.frame(RecPermute$parms); colnames(PermutedParms) <- c('alpha_p', 'beta_p', 'asd_p', 'bsd_p')

PermutedParms[,1] <- 1/(1+exp(-PermutedParms[,1]))*15
PermutedParms[,3] <- exp(PermutedParms[,3])
PermutedParms[,4] <- exp(PermutedParms[,4])

permuteSimA <- matrix(NA, 36, 650) # extract the simulated sum scores from the model
permuteSimFix <- matrix(NA, 36, 650) # extracted the simulated predicted score from the model

for (i in 1:650){
  permuteSimA[,i] <- RecPermute$simA[[i]][[1]][19:54,]
  permuteSimFix[,i] <- RecPermute$simAFix[[i]][[1]][19:54,]
}
colnames(permuteSimA) <- 1:650;
colnames(permuteSimFix) <- 1:650;

permuteSimA %>%
  as.data.frame() %>%
  mutate(Trial = 1:36) %>%
  pivot_longer(1:650, names_to = 'id', values_to = 'simAPermuted') %>%
  mutate(id = as.numeric(id)) %>%
  plyr::join(PermutedParms %>% mutate(id = 1:650), by = 'id') %>%
  plyr::join(permuteSimFix%>%
               as.data.frame() %>%
               mutate(Trial = 1:36) %>%
               pivot_longer(1:650, names_to = 'id', values_to = 'simAFix') %>%
               mutate(id = as.numeric(id)),
             by = c('id', 'Trial')) %>%
  plyr::join(indivParmsB %>%
               dplyr::select(alpha_m, beta_m, beta_v, alpha_v, id),
             by = 'id') %>%
  plyr::join(Intentions_guess %>%
               dplyr::select(Answer, id, Trial, PartnerPolicy, Age, Sex, Control, ICARTot, Persec),
             by = c('id', 'Trial')) %>%
  plyr::join(ControlDF %>%
               dplyr::select(id, Sum, CorrectFix),
             by = 'id') %>%
  mutate(permuteCor = ifelse(simAPermuted == Answer, 1, 0),
         permuteFixCor = ifelse(simAFix == Answer, 1, 0)) %>%
  group_by(id) %>%
  mutate(sumPCor = sum(permuteCor),
         sumPFixCor = sum(permuteFixCor)) %>%
  dplyr::select(-Trial, -simAPermuted, -Answer, -permuteCor, -simAFix, -permuteFixCor) %>%
  distinct() %>%
  na.omit()-> permutedPlot

ggplot(permutedPlot, aes(alpha_p, asd_p))+geom_point()+geom_smooth()+ggpubr::stat_cor() #check cor (should not correlate)
ggplot(permutedPlot, aes(beta_p, bsd_p))+geom_point()+geom_smooth()+ggpubr::stat_cor()  # check cor (should not correlate)

permutedPlotP <- permutedPlot %>% filter(PartnerPolicy == 'Prosocial')
permutedPlotC <- permutedPlot %>% filter(PartnerPolicy == 'Competative')
permutedPlotI <- permutedPlot %>% filter(PartnerPolicy == 'Individualist')

model.compare(lm(scale(sumPCor) ~ scale(alpha_m) + scale(beta_m) + scale(asd_p) + scale(bsd_p) + scale(sumPFixCor)+
                   scale(Persec) + scale(ICARTot) + Age + Sex + Control,
                 data = permutedPlotP, na.action = na.fail))
model.compare(lm(scale(sumPCor) ~ scale(alpha_m) + scale(beta_m) + scale(asd_p) + scale(bsd_p) + scale(sumPFixCor)+
                   scale(Persec) + scale(ICARTot) + Age + Sex + Control,
                 data = permutedPlotI, na.action = na.fail))
model.compare(lm(scale(sumPCor) ~ scale(alpha_m) + scale(beta_m) + scale(asd_p) + scale(bsd_p) + scale(sumPFixCor)+
                   scale(Persec) + scale(ICARTot) + Age + Sex + Control,
                 data = permutedPlotC, na.action = na.fail))

# Calculate final marginals -----------------------------------------------

MargSims <- readMat('Modelling/LaplaceFittedModels/SimulatedData/modelError_Generative.mat')

alpha_marg    <- as.data.frame(matrix(NA, nrow = 697, ncol = 241))
beta_marg     <- as.data.frame(matrix(NA, nrow = 697, ncol = 241))
alpha_margPPT <- as.data.frame(matrix(NA, nrow = 697, ncol = 241))
beta_margPPT  <- as.data.frame(matrix(NA, nrow = 697, ncol = 241))

for (i in 1:697) {
  alpha_marg[i,]    <- as.vector(MargSims$alpha.m2[[i]][[1]])
  beta_marg[i,]     <- as.vector(MargSims$beta.m2[[i]][[1]])
  alpha_margPPT[i,] <- as.vector(MargSims$alpha.m[[i]][[1]])
  beta_margPPT[i,]  <- as.vector(MargSims$beta.m[[i]][[1]])
}

colnames(alpha_marg) <- seq(0, 30, 0.125)
colnames(beta_marg) <- seq(-30, 30, 0.25)
colnames(alpha_margPPT) <- seq(0, 30, 0.125)
colnames(beta_margPPT) <- seq(-30, 30, 0.25)

cbind(alpha_marg %>%
        mutate(id = 1:697) %>%
        pivot_longer(1:241, names_to = 'index_a', values_to = 'alpha_lik'),
      beta_marg %>%
        mutate(id = 1:697) %>%
        pivot_longer(1:241, names_to = 'index_b', values_to = 'beta_lik'),
      alpha_margPPT %>%
        mutate(id = 1:697) %>%
        pivot_longer(1:241, names_to = 'index_a', values_to = 'alphaPPT_lik'),
      beta_margPPT %>%
        mutate(id = 1:697) %>%
        pivot_longer(1:241, names_to = 'index_b', values_to = 'betaPPT_lik')) %>%
  dplyr::select(1, 2, 3, 9, 5, 6, 12) %>%

  plyr::join(congruency, by = 'id') %>%
  dplyr::select(id, index_a, index_b, alpha_lik, beta_lik, alphaPPT_lik, betaPPT_lik,Sum, Persec, Age, Sex, Control, ICARTot,HI, SI, PartnerPolicy, alpha_m, beta_m, beta_v, alpha_v) %>%
  mutate(index_a = as.numeric(index_a),
         index_b = as.numeric(index_b)) %>%
  arrange(id, index_a, index_b) %>%
  distinct() -> marginals

# Partner Parameter Check -------------------------------------------------

Phase1Partner <- readMat('Modelling/LaplaceFittedModels/FittedParameters/lap_PartnerParms.mat')
Phase1Partner_parms     <-   Phase1Partner$cbm[,,1]$output[,,1]$parameters
Phase1Partner_parms[,1] <- (1/(1+exp(-Phase1Partner_parms[,1])))*15

Phase1Partner_parms %>%
  as.data.frame() %>%
  rename(alpha_partner = 1,
         beta_partner = 2) %>%
  mutate(id = 1:697) %>%
  plyr::join(indivParmsB %>%
               as.data.frame() %>%
               mutate(id = 1:697), by = 'id') %>%
  group_by(PartnerPolicy) %>%
  summarise(
    alpha = median(alpha_partner),
    beta = median(beta_partner),
    alphasd = sd(alpha_partner),
    betasd = sd(beta_partner)
  )

#Use optim (MLE) instead of hierarchical fit
Intentions_Phase1Check <- Intentions_BothPhase %>%
  na.omit() %>%
  group_by(id) %>%
  mutate(Trial = 1:36)

ControlDF$AlphaPar <- rep(NA, 697)
ControlDF$BetaPar <- rep(NA, 697)

#Estimate parameters
for (i in 1:697){
  data   <- Intentions_Phase1Check %>%
    filter(id == i) %>%
    as.data.frame()
  data  <- sapply(data, as.numeric)
  testpar<- as.numeric(c(0, 0))
  MLEfit <- optim(par  = testpar,
                  fn   = Phase1WrapperPartner,
                  datAr = data)
  ControlDF[i,'AlphaPar'] <- MLEfit$par[1]
  ControlDF[i,'BetaPar' ] <- MLEfit$par[2]
}

# Test alternative utility instead

testAltUtility <- readMat('Modelling/LaplaceFittedModels/AlternativeUtility.mat')

UtAltChoicesSimA <- matrix(NA, 18, 697)
RecChoicesSimA <- matrix(NA, 18, 697)

for (i in 1:697){
  UtAltChoicesSimA[,i] <- testAltUtility$simA[[i]][[1]][1:18,]
  RecChoicesSimA[,i]   <- RecError$simA[[i]][[1]][1:18,]
}

colnames(UtAltChoicesSimA) <- 1:697
colnames(RecChoicesSimA) <- 1:697

UtAltChoicesSimA %>%
  as.data.frame() %>%
  mutate(Trial = 1:18) %>%
  pivot_longer(1:697, names_to = 'id', values_to = 'simAAlternative') %>%
  mutate(id = as.numeric(id)) %>%
  plyr::join(RecChoicesSimA %>%
               as.data.frame() %>%
               mutate(Trial = 1:18) %>%
               pivot_longer(1:697, names_to = 'id', values_to = 'simARec') %>%
               mutate(id = as.numeric(id)),
             by = c('id', 'Trial')) %>%
  plyr::join(indivParmsB %>%
               dplyr::select(alpha_m, beta_m, beta_v, alpha_v, id),
             by = 'id') %>%
  plyr::join(Intentions_choice %>%
               dplyr::select(Response, id, Trial, PartnerPolicy, Age, Sex, Control, ICARTot, Persec),
             by = c('id', 'Trial')) %>%
  mutate(altUtCor = ifelse(simAAlternative == Response, 1, 0),
         recUtCor = ifelse(simARec == Response, 1, 0)) %>%
  group_by(id) %>%
  mutate(sumUtAltCor = sum(altUtCor),
         sumUtRecCor = sum(recUtCor)) %>%
  dplyr::select(-Trial, -simAAlternative, -Response, -altUtCor, -recUtCor, -simARec) %>%
  distinct() %>%
  na.omit()-> UtAltChoicesPlot

ggplot(UtAltChoicesPlot)+
  geom_jitter(aes(sumUtRecCor, sumUtAltCor))+
  geom_smooth(aes(sumUtRecCor, sumUtAltCor), method = 'lm')+
  ggpubr::stat_cor(aes(sumUtRecCor, sumUtAltCor))+
  labs(x = 'Simulated correct answers given alternative choices',
       y = 'Simulated correct answers given standard choices')+
  coord_cartesian(xlim = c(0,18), ylim = c(0,18))+
  scale_color_brewer(palette = 'Dark2')+
  ggridges::theme_ridges()+
  theme(legend.position = 'none')

# Generative Model Simulations -----------------------

GenModelPT <- readMat('Modelling/LaplaceFittedModels/SimulatedData/GenParmsPTV.mat')
GenModelPM <- readMat('Modelling/LaplaceFittedModels/SimulatedData/GenParmsPMV.mat')
GenModelPL <- readMat('Modelling/LaplaceFittedModels/SimulatedData/GenParmsPLV.mat')
GenModelIT <- readMat('Modelling/LaplaceFittedModels/SimulatedData/GenParmsITV.mat')
GenModelIM <- readMat('Modelling/LaplaceFittedModels/SimulatedData/GenParmsIMV.mat')
GenModelIL <- readMat('Modelling/LaplaceFittedModels/SimulatedData/GenParmsILV.mat')
GenModelCT <- readMat('Modelling/LaplaceFittedModels/SimulatedData/GenParmsCTV.mat')
GenModelCM <- readMat('Modelling/LaplaceFittedModels/SimulatedData/GenParmsCMV.mat')
GenModelCL <- readMat('Modelling/LaplaceFittedModels/SimulatedData/GenParmsCLV.mat')

GenList <- list()
GenMarg <- list()
n = 250

for (k in 1:9){

  if(k==1){
    GenModel <-GenModelPT
    par      <- c(3.94,-6.87,1,1)
  }else if (k==2){
    GenModel <-GenModelPL
    par      <- c(3.94,-6.87,6,6)
  }else if (k==3){
    GenModel <-GenModelIT
    par      <- c(13.07, -0.57,1,1)
  }else if (k==4){
    GenModel <-GenModelIL
    par      <- c(13.07, -0.57,6,6)
  }else if (k==5){
    GenModel <-GenModelCT
    par      <- c(0.99,6.74,1,1)
  }else if (k==6){
    GenModel <-GenModelCL
    par      <- c(0.99,6.74,6,6)}
  else if (k==7){
    GenModel <-GenModelPM
    par      <- c(3.94,-6.87,2.5,2.5)}
  else if (k==8){
    GenModel <-GenModelIM
    par      <- c(13.07, -0.57,2.5,2.5)}
  else if (k==9){
    GenModel <-GenModelCM
    par      <- c(0.99,6.74,2.5,2.5)}

  GenModelsimA                  <- matrix(NA, nrow = n, ncol = 36)
  GenModelsimAFix               <- matrix(NA, nrow = n, ncol = 36)
  GenModelalpha_marg    <- as.data.frame(matrix(NA, nrow = n, ncol = 121))
  GenModelbeta_marg     <- as.data.frame(matrix(NA, nrow = n, ncol = 121))
  GenModelalpha_margPPT <- as.data.frame(matrix(NA, nrow = n, ncol = 121))
  GenModelbeta_margPPT  <- as.data.frame(matrix(NA, nrow = n, ncol = 121))
  GenModelnon_listsimA  <- as.data.frame(matrix(NA, nrow = n, ncol = 55))

  for (i in 1:n){
    GenModelsimA[i,]            <- as.numeric(GenModel$simA[[i]][[1]][19:54,])
    GenModelsimAFix[i,]         <- as.numeric(GenModel$simAFix[[i]][[1]][19:54,])
    GenModelalpha_marg[i,]      <- as.vector(GenModel$alpha.m2[[i]][[1]])
    GenModelbeta_marg[i,]       <- as.vector(GenModel$beta.m2[[i]][[1]])
    GenModelalpha_margPPT[i,]   <- as.vector(GenModel$alpha.m[[i]][[1]])
    GenModelbeta_margPPT[i,]    <- as.vector(GenModel$beta.m[[i]][[1]])
    GenModelnon_listsimA[i,1:54]<- GenModel$simA[[i]][[1]]
    GenModelnon_listsimA[i,55]  <- i
  }

  GenModelsimA            <- as.data.frame(GenModelsimA)
  GenModelsimAFix         <- as.data.frame(GenModelsimAFix)
  GenModelsimA$ID         <- 1:n
  GenModelsimAFix$ID      <- 1:n
  colnames(GenModelsimA)            <- c(1:36, 'id')
  colnames(GenModelsimAFix)         <- c(1:36, 'id')
  colnames(GenModelalpha_marg) <- seq(0, 15, 0.125)
  colnames(GenModelbeta_marg) <- seq(-15, 15, 0.25)
  colnames(GenModelalpha_margPPT) <- seq(0, 15, 0.125)
  colnames(GenModelbeta_margPPT) <- seq(-15, 15, 0.25)
  colnames(GenModelnon_listsimA) <- c(1:54, 'id')

  GenModelsimA %>%
    pivot_longer(1:36, names_to = 'Trial', values_to = 'simA') -> GenModelsimA.edit
  GenModelsimAFix %>%
    pivot_longer(1:36, names_to = 'Trial', values_to = 'simAFixed') -> GenModelsimAFix.edit

  cbind(GenModelalpha_marg %>%
          mutate(id = 1:n) %>%
          pivot_longer(1:121, names_to = 'index_a', values_to = 'alpha_lik'),
        GenModelbeta_marg %>%
          mutate(id = 1:n) %>%
          pivot_longer(1:121, names_to = 'index_b', values_to = 'beta_lik'),
        GenModelalpha_margPPT %>%
          mutate(id = 1:n) %>%
          pivot_longer(1:121, names_to = 'index_a', values_to = 'alphaPPT_lik'),
        GenModelbeta_margPPT %>%
          mutate(id = 1:n) %>%
          pivot_longer(1:121, names_to = 'index_b', values_to = 'betaPPT_lik')) %>%
    dplyr::select(1, 2, 3, 9, 5, 6, 12) %>%
    plyr::join(congruency %>% dplyr::select(id, PartnerPolicy), by = 'id') %>%
    dplyr::select(id, index_a, index_b, alpha_lik, beta_lik, alphaPPT_lik, betaPPT_lik,PartnerPolicy) %>%
    mutate(index_a = as.numeric(index_a),
           index_b = as.numeric(index_b),
           model = k) %>%
    arrange(id, index_a, index_b) %>%
    distinct() -> GenModelmarginals

  GenModelnon_listsimA %>%
    pivot_longer(1:54, names_to = 'Trial', values_to = 'simA') %>%
    plyr::join(Intentions_BothPhase, by = c('id', 'Trial')) %>%
    plyr::join(indivParms %>% dplyr::select(id, PartnerPolicy), by = 'id') %>%
    mutate(id = as.numeric(id)) %>%
    na.omit() %>%
    distinct() %>%
    group_by(id) %>%
    mutate(CorrectSim = ifelse(simA == as.numeric(Answer), 1, 0),
           Match =      ifelse(simA == Response, 1, 0),
           CorrectSim = sum(CorrectSim)) %>%
    plyr::join(GenModelsimAFix.edit %>% group_by(id) %>% mutate(Trial = 19:54), by = c('id', 'Trial')) %>%
    mutate(CorrectFix = ifelse(simAFixed == Answer, 1, 0),
           alpha_m = par[1],
           beta_m  = par[2],
           alpha_v = par[3],
           beta_v  = par[4],
           model = k) %>%
    group_by(id) %>%
    distinct() %>%
    mutate(CorrectFix = sum(CorrectFix)) %>%
    distinct() -> Gentestdf2

  GenList[[k]] <- Gentestdf2
  GenMarg[[k]] <- GenModelmarginals

}

GenData <- do.call(rbind, GenList)
GenMargData <- do.call(rbind, GenMarg)



# Figure S2 ---------------------------------------------------------------

ggplot(GenMargData %>%
         mutate(SD = factor(ifelse (model %in% c(2, 4, 6), 'High [6]',
                                    ifelse(model %in% c(7, 8, 9), 'Medium [2.5]', 'Low [1]')),
                            levels = c('Low [1]', 'Medium [2.5]', 'High [6]')),
                `Participant Type`     = ifelse (model %in% c(1, 2, 7), 'Prosocial',
                                                 ifelse(model %in% c(3, 4, 8), 'Individualist', 'Competitive')))) +
  stat_summary(aes(index_a, alpha_lik, color = PartnerPolicy, linetype = 'Posterior'), geom = 'line', size = 1.2)+
  stat_summary(aes(index_a, alphaPPT_lik, linetype = 'Prior'), geom = 'line', size = 1.2)+
  labs(title = 'A', x = expression(paste('Simulated ', alpha[par], ' (Trial 36)')), y = 'Density Distribution')+
  facet_grid(SD ~ `Participant Type`, label = label_both)+
  scale_color_brewer(palette = 'Dark2', name = 'Partner SVO')+
  scale_linetype_manual(name ='Belief', values = c(1,3))+
  ggdist::theme_tidybayes()+
  theme(strip.background = element_blank(),
        title = element_text(face = 'bold'),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.position = c(0.4, 0.25),
        legend.text = element_text(size =14),
        legend.title = element_text(size =14),
        legend.direction = 'horizontal')

ggplot(GenMargData %>%
         mutate(SD = factor(ifelse (model %in% c(2, 4, 6), 'High [6]',
                                    ifelse(model %in% c(7, 8, 9), 'Medium [2.5]', 'Low [1]')),
                            levels = c('Low [1]', 'Medium [2.5]', 'High [6]')),
                `Participant Type`     = ifelse (model %in% c(1, 2, 7), 'Prosocial',
                                                 ifelse(model %in% c(3, 4, 8), 'Individualist', 'Competitive')))) +
  stat_summary(aes(index_b, beta_lik, color = PartnerPolicy, linetype = 'Posterior'), geom = 'line', size = 1.2)+
  stat_summary(aes(index_b, betaPPT_lik, linetype = 'Prior'), geom = 'line', size = 1.2)+
  labs(title = 'B', x = expression(paste('Simulated ', beta[par], ' (Trial 36)')), y = 'Density Distribution')+
  facet_grid(SD ~ `Participant Type`, label = label_both)+
  scale_color_brewer(palette = 'Dark2', name = 'Partner SVO')+
  scale_linetype_manual(name ='Belief', values = c(1,3))+
  ggdist::theme_tidybayes()+
  theme(strip.background = element_blank(),
        title = element_text(face = 'bold'),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.position = c(0.45, 0.25),
        legend.text = element_text(size =14),
        legend.title = element_text(size =14),
        legend.direction = 'horizontal')

# Figure S7 ---------------------------------------------------------------

SimAPlot <- ggplot(testdf2 %>% dplyr::select(CorrectSim, Sum, PartnerPolicy, id) %>% distinct())+
  geom_jitter(aes(CorrectSim, Sum, color = PartnerPolicy), alpha = 0.5)+
  geom_smooth(aes(CorrectSim, Sum, color = PartnerPolicy), method = 'lm')+
  labs(x = expression(paste('Simulated correct answers given ',
                            alpha[ppt]^m,
                            beta[ppt]^m,
                            alpha^sigma,
                            beta^sigma)),
       y = 'Real Correct Answers')+
  ggpubr::stat_cor(aes(CorrectSim, Sum, color = PartnerPolicy), show.legend = F)+
  scale_color_brewer(palette = 'Dark2')+
  ggridges::theme_ridges()+
  theme(legend.position = 'none')

PredAPlot <- ggplot(testdf2 %>% dplyr::select(CorrectFix, Sum, PartnerPolicy, id) %>% distinct())+
  geom_density(aes(CorrectFix,fill = PartnerPolicy), alpha = 0.75)+
  labs(x = expression(paste('Participant-partner similarity scores given ',
                            alpha[ppt]^m,
                            beta[ppt]^m)),
       y = 'Density')+
  scale_fill_brewer(palette = 'Dark2')+
  ggridges::theme_ridges()+
  theme(legend.position = c(0.1, 0.75))

(SimAPlot | PredAPlot) & plot_annotation(tag_levels = 'A')

# Figure S8 ----------------------------------------------------------

Dist1 <- ggplot(marginals %>% filter(alpha_lik < 0.07)) +
  geom_density(aes(index_a, y = alpha_lik, color = PartnerPolicy, group = id), size = 0.03, stat = 'identity') +
  stat_summary(aes(index_a, alpha_lik, color = PartnerPolicy), geom = 'line', size = 1.2)+
  labs(x = expression(paste(alpha[par], ' (Trial 36)')), y = 'Density Distribution')+
  scale_color_brewer(palette = 'Dark2', name = 'Policy')+
  ggdist::theme_tidybayes()+
  theme(strip.background.x = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 14),
        legend.position = c(0.4, 0.9),
        legend.direction = 'horizontal')

Dist2 <- ggplot(marginals) +
  geom_density(aes(index_b, y = beta_lik, color = PartnerPolicy, group = id), size = 0.03, stat = 'identity') +
  stat_summary(aes(index_b, beta_lik, color = PartnerPolicy), geom = 'line', size = 1.2)+
  labs(x = expression(paste(beta[par], ' (Trial 36)')), y = 'Density Distribution')+
  scale_color_brewer(palette = 'Dark2', name = 'Policy')+
  ggdist::theme_tidybayes()+
  theme(strip.background.x = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 14),
        legend.position = 'none')

(Dist1 | Dist2)

marginals %>%
  plyr::join(
    Phase1Partner_parms %>%
      as.data.frame() %>%
      rename(alpha_part = 1,
             beta_part = 2) %>%
      mutate(id = 1:697),
    by = 'id') -> marginals_part

# Figure S9 ---------------------------------------------------------------
library(sjPlot)
x <- ggplot(data1c)+
  geom_smooth(aes(CorrectFix, Sum), method = 'lm', color = 'black')+
  geom_jitter(aes(CorrectFix, Sum, color = beta_m))+
  scale_color_gradient2(low = '#7570B3', mid = 'white', high = '#1B9E77', name = expression(paste(beta[ppt]^m)),
                        breaks = c(-15, -5, 0, 5))+
  coord_cartesian(ylim = c(0, 36), xlim = c(0,36))+
  ggridges::theme_ridges()+
  labs(x = 'Predicted Score: Competitive', y = 'True Correct Answers')+
  ggplot(data2c)+
  geom_smooth(aes(CorrectFix, Sum), method = 'lm', color = 'black')+
  geom_jitter(aes(CorrectFix, Sum, color = alpha_m))+
  scale_color_gradient(low = 'white', high = '#D95F02', name = expression(paste(alpha[ppt]^m)),
                       breaks = c(0, 5, 10, 15))+
  coord_cartesian(ylim = c(0, 36), xlim = c(0,36))+
  ggridges::theme_ridges()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()
  )+
  labs(x = 'Predicted Score: Individualist')+
  ggplot(data3c)+
  geom_smooth(aes(CorrectFix, Sum), method = 'lm', color = 'black')+
  geom_jitter(aes(CorrectFix, Sum, color = beta_m))+
  scale_color_gradient2(low = '#7570B3', mid = 'white', high = '#1B9E77', name = expression(paste(beta[ppt]^m)))+
  coord_cartesian(ylim = c(0, 36), xlim = c(0,36))+
  ggridges::theme_ridges()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()
  )+
  labs(x = 'Predicted Score: Prosocial')


parmY <- ggplot(data1c)+
  geom_smooth(aes(beta_m, Sum), method = 'lm', color = 'black')+
  geom_jitter(aes(beta_m, Sum, color = beta_m))+
  scale_color_gradient2(low = '#7570B3', mid = 'white', high = '#1B9E77', name = expression(paste(beta[ppt]^m)),
                        breaks = c(-15, -5, 0, 5), guide = 'none')+
  ggpubr::stat_cor(aes(beta_m, Sum), label.y.npc = 'bottom')+
  coord_cartesian(ylim = c(0, 36))+
  ggridges::theme_ridges()+
  labs(y = 'True Correct Answers', x = expression(paste('Parameter estimate ', beta[ppt]^m)))+
  geom_text(x = -5, y = 0.5, label = 'Partner:Competitive', check_overlap = T)+
  ggplot(data2c)+
  geom_smooth(aes(alpha_m, Sum), method = 'lm', color = 'black')+
  geom_jitter(aes(alpha_m, Sum, color = alpha_m))+
  scale_color_gradient(low = 'white', high = '#D95F02', name = expression(paste(alpha[ppt]^m)),
                       breaks = c(0, 5, 10, 15), guide = 'none')+
  geom_text(x = 5, y = 0.5, label = 'Partner:Individualist', check_overlap = T)+
  ggpubr::stat_cor(aes(alpha_m, Sum), label.y.npc = 'bottom')+
  coord_cartesian(ylim = c(0, 36))+
  ggridges::theme_ridges()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()
  )+
  labs(y = 'True Correct Answers', x = expression(paste('Parameter estimate ', alpha[ppt]^m)))+
  ggplot(data3c)+
  geom_smooth(aes(beta_m, Sum), method = 'lm', color = 'black')+
  geom_jitter(aes(beta_m, Sum, color = beta_m))+
  scale_color_gradient2(low = '#7570B3', mid = 'white', high = '#1B9E77',
                        name = expression(paste('Parameter estimate ', beta[ppt]^m)), guide = 'none')+
  geom_text(x = -5, y = 0.5, label = 'Partner:Prosocial', check_overlap = T)+
  ggpubr::stat_cor(aes(beta_m, Sum), label.y.npc = 'bottom')+
  coord_cartesian(ylim = c(0, 36))+
  ggridges::theme_ridges()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()
  )+
  labs(y = 'True Correct Answers', x = expression(paste('Parameter estimate ', beta[ppt]^m)))


(x &
    ggpubr::stat_cor(aes(CorrectFix, Sum), label.y.npc = 'bottom')&
    theme(legend.position = 'top',
          legend.direction = 'horizontal')) /
  parmY/
  (xCPlot | xIPlot | xPPlot)

interP <- ControlDF %>% filter(PartnerPolicy == 'Prosocial')
xP <- lm(scale(Sum) ~ scale(CorrectFix) * scale(beta_m) + scale(Persec) + scale(ICARTot) + Age + Sex + Control,
         data = interP,
         na.action = na.fail)
xPPlot <- plot_model(xP, type = "pred", terms = c("CorrectFix", "beta_m [-15, 0, 15]"))+
  scale_color_brewer(palette = 'PRGn')+
  scale_fill_brewer(palette = 'PRGn')+
  labs(x = 'Predicted Score', y='True Correct Answers')+
  ggridges::theme_ridges()+
  theme(plot.title = element_blank(),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        legend.position = 'none')
interI <- ControlDF %>% filter(PartnerPolicy == 'Individualist')
model.compare(lm(scale(Sum) ~ scale(CorrectFix) * scale(alpha_m) + scale(Persec) + scale(ICARTot) + Age + Sex + Control,
                 data = interI,
                 na.action = na.fail))
xIPlot <- plot_model(xI, type = "pred", terms = c("CorrectFix", "alpha_m [0, 5, 10, 15]"))+
  scale_color_brewer(palette = 'Oranges')+
  scale_fill_brewer(palette = 'Oranges')+
  labs(x = 'Predicted Score', y='True Correct Answers')+
  ggridges::theme_ridges()+
  theme(plot.title = element_blank(),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        legend.position = 'none')
interC <- ControlDF %>% filter(PartnerPolicy == 'Competitive')
xC <- lm(scale(Sum) ~ scale(CorrectFix) * scale(beta_m) + scale(Persec) + scale(ICARTot) + Age + Sex + Control,
         data = interC,
         na.action = na.fail)
xCPlot <- plot_model(xC, type = "pred", terms = c("CorrectFix", "beta_m [-15, 0, 15]"))+
  scale_color_brewer(palette = 'PRGn')+
  scale_fill_brewer(palette = 'PRGn')+
  labs(x = 'Predicted Score', y='True Correct Answers')+
  ggridges::theme_ridges()+
  theme(plot.title = element_blank(),
        axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        legend.position = 'none')

#low = '#1B9E77', mid = '#D95F02', high = '#7570B3'

# Explainer Plot ----------------------------------------------------------

ggplot(GenMargData %>%
         mutate(SD = factor(ifelse (model %in% c(2, 4, 6), 'High [6]',
                                    ifelse(model %in% c(7, 8, 9), 'Medium [2.5]', 'Low [1]')),
                            levels = c('Low [1]', 'Medium [2.5]', 'High [6]')),
                `Participant Type`     = ifelse (model %in% c(1, 2, 7), 'Prosocial',
                                                 ifelse(model %in% c(3, 4, 8), 'Individualist', 'Competitive'))) %>%
         filter(`Participant Type` == 'Prosocial',
                SD == 'Low [1]')) +
  #stat_summary(aes(index_b, beta_lik, color = PartnerPolicy, linetype = 'Posterior'), geom = 'line', size = 1.2)+
  stat_summary(aes(index_b, betaPPT_lik, linetype = 'Prior Belief'), geom = 'line', size = 1.2)+
  #stat_summary(aes(index_b+20, betaPPT_lik), geom = 'line', size = 1.2, linetype = 1, color = '#1B9E77')+
  labs(x = expression(paste('Simulated ', beta[par])), y = 'Density Distribution')+
  #geom_vline(xintercept = 11.06, size = 1, color = '#1B9E77')+

  annotate("segment", x = -8.5, xend = -5.5, y = 0.02, yend = 0.02, colour = "red", size=1, alpha=1, arrow=arrow(ends = 'both', type = 'closed', length = unit(0.3, 'cm')))+
  annotate("segment", x = -5.5, xend = -7, y = 0.05, yend = 0.05, colour = "blue", size=1, alpha=1)+
  geom_vline(xintercept = -7, size = 1, color = 'blue')+
  annotate('text', x = -4, y = 0.05, label = expression(paste(beta[ppt]^m)),check_overlap = T, size = 12, color = 'blue')+
  annotate('text', x = -10, y = 0.02, label = expression(paste(beta^sigma)),check_overlap = T, size = 12, color = 'red')+

  #annotate("segment", x = -5.5, xend = 10, y = 0.08, yend = 0.08, colour = "black", size=1, alpha=1,
  #         arrow = arrow(length = unit(0.2, 'cm')))+
  #annotate('text', x = 1.6, y = 0.080, label = 'Representational distance required to move to
  #learn about the partner effectively',check_overlap = T, size = 6)+

  #facet_grid(SD ~ `Participant Type`, label = label_both)+
  scale_color_brewer(palette = 'Dark2', name = 'Partner SVO')+
  scale_linetype_manual(name ='', values = c(3))+
  scale_y_continuous(expand= c(0,0))+
  ggdist::theme_tidybayes()+
  coord_cartesian(xlim = c(-15, 15))+
  theme(strip.background = element_blank(),
        plot.title = ggtext::element_markdown(face = 'bold'),
        axis.text = element_text(size = 24),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size =20),
        legend.title = element_text(size =20),
        legend.direction = 'vertical')


ggplot(GenMargData %>%
         mutate(SD = factor(ifelse (model %in% c(2, 4, 6), 'High [6]',
                                    ifelse(model %in% c(7, 8, 9), 'Medium [2.5]', 'Low [1]')),
                            levels = c('Low [1]', 'Medium [2.5]', 'High [6]')),
                `Participant Type`     = ifelse (model %in% c(1, 2, 7), 'Prosocial',
                                                 ifelse(model %in% c(3, 4, 8), 'Individualist', 'Competitive'))) %>%
         filter(`Participant Type` == 'Prosocial',
                SD == 'Low [1]')) +
  #stat_summary(aes(index_b, beta_lik, color = PartnerPolicy, linetype = 'Posterior'), geom = 'line', size = 1.2)+
  stat_summary(aes(index_a+2, alphaPPT_lik, linetype = 'Prior Belief'), geom = 'line', size = 1.2)+
  #stat_summary(aes(index_b+20, betaPPT_lik), geom = 'line', size = 1.2, linetype = 1, color = '#1B9E77')+
  labs(x = expression(paste('Simulated ', alpha[par])), y = 'Density Distribution')+
  #geom_vline(xintercept = 1, size = 1, color = '#1B9E77')+

  annotate("segment", x = 2.5+2, xend = 5.5+2, y = 0.01, yend = 0.01, colour = "red", size=1, alpha=1, arrow=arrow(ends = 'both', type = 'closed', length = unit(0.3, 'cm')))+
  annotate("segment", x = 4+2, xend = 6+2, y = 0.03, yend = 0.03, colour = "blue", size=1, alpha=1)+
  geom_vline(xintercept = 4+2, size = 1, color = 'blue')+
  annotate('text', x = 7+2, y = 0.03, label = expression(paste(alpha[ppt]^m)),check_overlap = T, size = 12, color = 'blue')+
  annotate('text', x = 7+2, y = 0.01, label = expression(paste(alpha^sigma)),check_overlap = T, size = 12, color = 'red')+

  #annotate("segment", x = 5, xend = 1.2, y = 0.04, yend = 0.04, colour = "black", size=1, alpha=1,
  #         arrow = arrow(length = unit(0.2, 'cm')))+
  #annotate('text', x = 8.5, y = 0.040, label = 'Representational distance required to move to
  #learn about the partner effectively',check_overlap = T, size = 5)+

  #facet_grid(SD ~ `Participant Type`, label = label_both)+
  scale_color_brewer(palette = 'Dark2', name = 'Partner SVO')+
  scale_linetype_manual(name ='', values = c(3))+
  scale_y_continuous(expand = c(0,0))+
  ggdist::theme_tidybayes()+
  coord_cartesian(xlim = c(0, 15))+
  theme(strip.background = element_blank(),
        plot.title = ggtext::element_markdown(face = 'bold'),
        axis.text = element_text(size = 24),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = c(0.9, 0.5),
        legend.text = element_text(size =20),
        legend.title = element_text(size =20),
        legend.direction = 'vertical')

little <- ggplot(GenMargData %>%
                   mutate(SD = factor(ifelse (model %in% c(2, 4, 6), 'Large SD',
                                              ifelse(model %in% c(7, 8, 9), 'Medium SD', 'Low SD')),
                                      levels = c('Low SD', 'Medium SD', 'Large SD')),
                          `Participant Type`     = ifelse (model %in% c(1, 2, 7), 'Prosocial',
                                                           ifelse(model %in% c(3, 4, 8), 'Individualist', 'Competitive'))) %>%
                   filter(`Participant Type` == 'Prosocial',
                          PartnerPolicy == 'Competitive',
                          SD == 'Low SD')) +
  stat_summary(aes(index_b, beta_lik, color = PartnerPolicy, linetype = 'Posterior Belief'), geom = 'line', size = 1.2, color = 'black')+
  stat_summary(aes(index_b, betaPPT_lik, linetype = 'Prior Belief'), geom = 'line', size = 1.2)+
  labs(x = expression(paste('Simulated ', beta[par])), y = 'Density Distribution')+
  geom_vline(xintercept = 11.06, size = 1, color = '#1B9E77')+
  #facet_grid(SD ~ .)+
  scale_color_brewer(palette = 'Dark2', name = 'Partner SVO', guide = 'none')+
  scale_linetype_manual(name ='', values = c(1,3))+
  ggdist::theme_tidybayes()+
  coord_cartesian(xlim = c(-15, 15))+

  annotate("segment", x = -5.5, xend = -2, y = 0.08, yend = 0.08, colour = "black", size=1, alpha=1,
           arrow = arrow(length = unit(0.2, 'cm')))+

  annotate('text', x = 14, y = 0.07, colour = 'blue',
           label = expression(paste(beta[ppt]^m, ' = -7')), size = 6)+
  annotate('text', x = 14, y = 0.03, colour = 'red',
           label = expression(paste(beta^sigma, ' = 1')), size = 6)+

  theme(strip.background = element_blank(),
        plot.title = ggtext::element_markdown(face = 'bold'),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size =14),
        legend.title = element_text(size =14),
        legend.direction = 'vertical')

medium <- ggplot(GenMargData %>%
                   mutate(SD = factor(ifelse (model %in% c(2, 4, 6), 'Large SD',
                                              ifelse(model %in% c(7, 8, 9), 'Medium SD', 'Low SD')),
                                      levels = c('Low SD', 'Medium SD', 'Large SD')),
                          `Participant Type`     = ifelse (model %in% c(1, 2, 7), 'Prosocial',
                                                           ifelse(model %in% c(3, 4, 8), 'Individualist', 'Competitive'))) %>%
                   filter(`Participant Type` == 'Prosocial',
                          PartnerPolicy == 'Competitive',
                          SD == 'Medium SD')) +
  stat_summary(aes(index_b, beta_lik, color = PartnerPolicy, linetype = 'Posterior Belief'), geom = 'line', size = 1.2, color = 'black')+
  stat_summary(aes(index_b, betaPPT_lik, linetype = 'Prior Belief'), geom = 'line', size = 1.2)+
  labs(x = expression(paste('Simulated ', beta[par])), y = 'Density Distribution')+
  geom_vline(xintercept = 11.06, size = 1, color = '#1B9E77')+
  #facet_grid(SD ~ .)+
  scale_color_brewer(palette = 'Dark2', name = 'Partner SVO', guide = 'none')+
  scale_linetype_manual(name ='', values = c(1,3))+
  ggdist::theme_tidybayes()+
  coord_cartesian(xlim = c(-15, 15))+

  annotate("segment", x = -5.5, xend = 1, y = 0.05, yend = 0.05, colour = "black", size=1, alpha=1,
           arrow = arrow(length = unit(0.2, 'cm')))+

  annotate('text', x = 14, y = 0.07, colour = 'blue',
           label = expression(paste(beta[ppt]^m, ' = -7')), size = 6)+
  annotate('text', x = 14, y = 0.03, colour = 'red',
           label = expression(paste(beta^sigma, ' = 3')), size = 6)+

  theme(strip.background = element_blank(),
        plot.title = ggtext::element_markdown(face = 'bold'),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(.1, 0.75),
        legend.text = element_text(size =14),
        legend.title = element_text(size =14),
        legend.direction = 'vertical')

large <- ggplot(GenMargData %>%
                  mutate(SD = factor(ifelse (model %in% c(2, 4, 6), 'Large SD',
                                             ifelse(model %in% c(7, 8, 9), 'Medium SD', 'Low SD')),
                                     levels = c('Low SD', 'Medium SD', 'Large SD')),
                         `Participant Type`     = ifelse (model %in% c(1, 2, 7), 'Prosocial',
                                                          ifelse(model %in% c(3, 4, 8), 'Individualist', 'Competitive'))) %>%
                  filter(`Participant Type` == 'Prosocial',
                         PartnerPolicy == 'Competitive',
                         SD == 'Large SD')) +
  stat_summary(aes(index_b, beta_lik, color = PartnerPolicy, linetype = 'Posterior Belief'), geom = 'line', size = 1.2, color = 'black')+
  stat_summary(aes(index_b, betaPPT_lik, linetype = 'Prior Belief'), geom = 'line', size = 1.2)+
  labs(x = expression(paste('Simulated ', beta[par])), y = 'Density Distribution')+
  geom_vline(xintercept = 11.06, size = 1, color = '#1B9E77')+
  #facet_grid(SD ~ .)+
  scale_color_brewer(palette = 'Dark2', name = 'Partner SVO', guide = 'none')+
  scale_linetype_manual(name ='', values = c(1,3))+
  ggdist::theme_tidybayes()+
  coord_cartesian(xlim = c(-15, 15))+

  annotate("segment", x = -5.5, xend = 6, y = 0.025, yend = 0.025, colour = "black", size=1, alpha=1,
           arrow = arrow(length = unit(0.2, 'cm')))+

  annotate('text', x = 14, y = 0.03, colour = 'blue',
           label = expression(paste(beta[ppt]^m, ' = -7')), size = 6)+
  annotate('text', x = 14, y = 0.01, colour = 'red',
           label = expression(paste(beta^sigma, ' = 7')), size = 6)+

  theme(strip.background = element_blank(),
        plot.title = ggtext::element_markdown(face = 'bold'),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size =14),
        legend.title = element_text(size =14),
        legend.direction = 'vertical')

littleM <- ggplot(GenMargData %>%
                    mutate(SD = factor(ifelse (model %in% c(2, 4, 6), 'Large SD',
                                               ifelse(model %in% c(7, 8, 9), 'Medium SD', 'Low SD')),
                                       levels = c('Low SD', 'Medium SD', 'Large SD')),
                           `Participant Type`     = ifelse (model %in% c(1, 2, 7), 'Prosocial',
                                                            ifelse(model %in% c(3, 4, 8), 'Individualist', 'Competitive'))) %>%
                    filter(`Participant Type` == 'Prosocial',
                           PartnerPolicy == 'Competitive',
                           SD == 'Low SD')) +
  stat_summary(aes(index_b, beta_lik, color = PartnerPolicy, linetype = 'Posterior Belief'), geom = 'line', size = 1.2, color = 'black')+
  stat_summary(aes(index_b, betaPPT_lik, linetype = 'Prior Belief'), geom = 'line', size = 1.2)+
  labs(x = expression(paste('Simulated ', beta[par])), y = 'Density Distribution')+
  geom_vline(xintercept = 11.06, size = 1, color = '#1B9E77')+
  #facet_grid(SD ~ .)+
  scale_color_brewer(palette = 'Dark2', name = 'Partner SVO', guide = 'none')+
  scale_linetype_manual(name ='', values = c(1,3))+
  ggdist::theme_tidybayes()+
  coord_cartesian(xlim = c(-15, 15))+

  annotate("segment", x = -5.5, xend = -2, y = 0.07, yend = 0.07, colour = "black", size=1, alpha=1,
           arrow = arrow(length = unit(0.2, 'cm')))+

  annotate('text', x = 14, y = 0.07, colour = 'blue',
           label = expression(paste(beta[ppt]^m, ' = -7')), size = 6)+
  annotate('text', x = 14, y = 0.03, colour = 'red',
           label = expression(paste(beta^sigma, ' = 1')), size = 6)+

  theme(strip.background = element_blank(),
        plot.title = ggtext::element_markdown(face = 'bold'),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size =14),
        legend.title = element_text(size =14),
        legend.direction = 'vertical')

mediumM <- ggplot(GenMargData %>%
                    mutate(SD = factor(ifelse (model %in% c(2, 4, 6), 'Large SD',
                                               ifelse(model %in% c(7, 8, 9), 'Medium SD', 'Low SD')),
                                       levels = c('Low SD', 'Medium SD', 'Large SD')),
                           `Participant Type`     = ifelse (model %in% c(1, 2, 7), 'Prosocial',
                                                            ifelse(model %in% c(3, 4, 8), 'Individualist', 'Competitive'))) %>%
                    filter(`Participant Type` == 'Individualist',
                           PartnerPolicy == 'Competitive',
                           SD == 'Low SD')) +
  annotate("segment", x = -1, xend = 3.5, y = 0.05, yend = 0.05, colour = "black", size=1, alpha=1,
           arrow = arrow(length = unit(0.2, 'cm')))+

  annotate('text', x = 14, y = 0.07, colour = 'blue',
           label = expression(paste(beta[ppt]^m, ' = 0')), size = 6)+
  annotate('text', x = 14, y = 0.03, colour = 'red',
           label = expression(paste(beta^sigma, ' = 1')), size = 6)+

  stat_summary(aes(index_b, beta_lik, color = PartnerPolicy, linetype = 'Posterior Belief'), geom = 'line', size = 1.2, color = 'black')+
  stat_summary(aes(index_b, betaPPT_lik, linetype = 'Prior Belief'), geom = 'line', size = 1.2)+
  labs(x = expression(paste('Simulated ', beta[par])), y = 'Density Distribution')+
  geom_vline(xintercept = 11.06, size = 1, color = '#1B9E77')+
  #facet_grid(SD ~ .)+
  scale_color_brewer(palette = 'Dark2', name = 'Partner SVO', guide = 'none')+
  scale_linetype_manual(name ='', values = c(1,3))+
  ggdist::theme_tidybayes()+
  coord_cartesian(xlim = c(-15, 15))+

  theme(strip.background = element_blank(),
        plot.title = ggtext::element_markdown(face = 'bold'),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(.1, 0.75),
        legend.text = element_text(size =14),
        legend.title = element_text(size =14),
        legend.direction = 'vertical')

largeM <- ggplot(GenMargData %>%
                   mutate(SD = factor(ifelse (model %in% c(2, 4, 6), 'Large SD',
                                              ifelse(model %in% c(7, 8, 9), 'Medium SD', 'Low SD')),
                                      levels = c('Low SD', 'Medium SD', 'Large SD')),
                          `Participant Type`     = ifelse (model %in% c(1, 2, 7), 'Prosocial',
                                                           ifelse(model %in% c(3, 4, 8), 'Individualist', 'Competitive'))) %>%
                   filter(`Participant Type` == 'Competitive',
                          PartnerPolicy == 'Competitive',
                          SD == 'Low SD')) +
  annotate("segment", x = 6, xend = 8.2, y = 0.05, yend = 0.05, colour = "black", size=1, alpha=1,
           arrow = arrow(length = unit(0.2, 'cm')))+

  annotate('text', x = 14, y = 0.07, colour = 'blue',
           label = expression(paste(beta[ppt]^m, ' = 7')), size = 6)+
  annotate('text', x = 14, y = 0.03, colour = 'red',
           label = expression(paste(beta^sigma, ' = 1')), size = 6)+

  stat_summary(aes(index_b, beta_lik, linetype = 'Posterior Belief'), geom = 'line', color = 'black', size = 1.2)+
  stat_summary(aes(index_b, betaPPT_lik, linetype = 'Prior Belief'), geom = 'line', size = 1.2)+
  labs(x = expression(paste('Simulated ', beta[par])), y = 'Density Distribution')+
  geom_vline(xintercept = 11.06, size = 1, color = '#1B9E77')+
  #facet_grid(SD ~ .)+
  scale_color_brewer(palette = 'Dark2', name = 'Partner SVO', guide = 'none')+
  scale_linetype_manual(name ='', values = c(1,3))+
  ggdist::theme_tidybayes()+
  coord_cartesian(xlim = c(-15, 15))+
  theme(strip.background = element_blank(),
        plot.title = ggtext::element_markdown(face = 'bold'),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size =14),
        legend.title = element_text(size =14),
        legend.direction = 'vertical')

start

(little/medium/large)

(littleM/mediumM/largeM)

ggplot()+
  ggplot(GenMargData %>%
           mutate(SD = factor(ifelse (model %in% c(2, 4, 6), 'Large SD',
                                      ifelse(model %in% c(7, 8, 9), 'Medium SD', 'Low SD')),
                              levels = c('Low SD', 'Medium SD', 'Large SD')),
                  `Participant Type`     = ifelse (model %in% c(1, 2, 7), 'Prosocial',
                                                   ifelse(model %in% c(3, 4, 8), 'Individualist', 'Competitive'))) %>%
           filter(`Participant Type` == 'Competitive',
                  PartnerPolicy == 'Competitive',
                  SD == 'Low SD')) +
  #stat_summary(aes(index_b, beta_lik, linetype = 'Posterior Belief'), geom = 'line', color = 'black', size = 1.2)+
  #stat_summary(aes(index_b, betaPPT_lik, linetype = 'Prior Belief'), geom = 'line', size = 1.2)+
  theme_void()+
  annotate('text', x = 5, y = 5, label = expression(paste('Increasing ', beta[ppt]^m)), color= 'blue', size = 12)


ggplot(Simplex, aes(as.numeric(alpha_m), as.numeric(beta_m)))+
  geom_point(aes(color = Scale01(Prosocial)), alpha = 0.75)+
  scale_color_gradient2(low = '#1B9E77', mid = '#D95F02', high = '#7570B3',midpoint = 0.5,
                        breaks = c(0, 0.5, 1), name = 'SVO Archetype',
                        labels = c('Competitive', 'Individualist', 'Prosocial'))+
  #geom_smooth(data = groupplot.parms2, aes(alpha_m*(100/15), beta_m*(100/20)), method = 'lm')+
  geom_point(x = 40, y = -50, color ='blue', size = 5)+
  geom_point(x = 40, y = -50, color ='red', size = 10, fill = NA, shape = 21)+
  geom_point(x = 10, y = 50, color ='black', size = 5)+
  annotate('segment', x = 38, y = -44, color = 'black', xend = 12, yend = 43, size = 1, arrow = arrow(length = unit(0.3, 'cm'), type = 'closed'))+
  #annotate('text', x = 30, y = 50, color = 'black', size = 4, label = "Partner's joint parameter values", fontface = 'bold')+
  #annotate('text', x = 63, y = -50, color = 'black', size = 4, label = "Participant's joint parameter values", fontface = 'bold')+
  scale_x_continuous(breaks = c(0, 33, 66, 100), labels = c(0, 5, 10, 15))+
  scale_y_continuous(breaks = c(-100, -50, 0, 50, 100), labels = c(-20, -10, 0, 10, 20))+
  labs(x = expression(paste(alpha[ppt]^m)), y = expression(paste(beta[ppt]^m)))+
  theme_void()+
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 24),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20, margin = margin(1,3,3,3), face = 'bold'))


