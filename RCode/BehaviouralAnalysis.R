#Intentions Behavioural Analysis
#Joe Barnby
#Joe.barnby@kcl.ac.uk | j.barnby@uq.edu.au

# Load data and libs ------------------------------------------------------

#rm(list = ls())
library(easystats)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)

source("Functions.R")

#load data

Intentions <- read.csv("Intentions_BothPhase.csv")
Intentions_choice <- read.csv("Intentions_Phase1.csv")
Intentions_guess  <- read.csv("Intentions_Phase2.csv")
# Summary stats and behav models ------------------------------------------

#test summaries Intentions

names(Intentions)

model1 <- Intentions_guess %>%
  #filter(Game == "Guess") %>%
  select(Persec, PartnerPolicy, Sum, Sex, Age, HI, SI, ICARTot, id, Control) %>%
  mutate(
    Sex = ifelse(Sex == "Male", 1, 0),
    PartnerPolicy = factor(
      PartnerPolicy,
      levels = c( "Individualist", "Competative", "Prosocial"),
      ordered = F
    )
  ) %>%
  distinct()

#visualise the density
mu1 <-
  plyr::ddply(Intentions,
              c("PartnerPolicy"),
              summarise,
              grp.mean = median(Sum))

Intentions_guess %>%

  ggplot() +

  geom_vline(
    data = mu1 ,
    aes(xintercept = grp.mean, color = PartnerPolicy),
    linetype = "dashed",
    show.legend = F
  ) +
  geom_label_repel(
    data = mu1,
    aes(
      x = grp.mean,
      y = 0.4,
      label = round(grp.mean, 2),
      color = PartnerPolicy
    ),
    show.legend = F,
    segment.alpha = 0.01
  )+
  geom_density(aes(Sum, fill = PartnerPolicy, group = PartnerPolicy))

model.compare(lm(
  scale(Sum) ~ scale(Persec) + PartnerPolicy + scale(Age) + Sex + scale(ICARTot) + scale(Control),
  na.action = na.fail,
  data = model1,
))

model.compare(lm(
  scale(HI) ~ PartnerPolicy + scale(Persec) +  Age + Sex + scale(ICARTot) + Control,
  na.action = na.fail,
  data = model1,
))

model1a <- model1 %>%
  filter(PartnerPolicy == "Prosocial") %>%
  dplyr::select(Sum, Persec, Age, Sex, HI, SI, ICARTot, id, Control) %>%
  distinct()

model.compare(lm(
  scale(HI) ~ scale(Persec) + Age + Sex + Control + scale(ICARTot),
  na.action = na.fail,
  data = model1a
))

model.compare(lm(
  scale(Sum) ~ scale(Persec) + scale(HI) + scale(SI) + Age + Sex + Control + scale(ICARTot),
  na.action = na.fail,
  data = model1a
))

  model1b <- model1 %>%
  filter(PartnerPolicy == "Individualist") %>%
  select(Sum, Persec, Age, HI, SI, Sex, ICARTot, id, Control) %>%
  distinct()

model.compare(lm(
  scale(HI) ~ scale(Persec) + Age + Sex + scale(ICARTot) + Control,
  na.action = na.fail,
  data = model1b
))

model.compare(lm(
  scale(Sum) ~ scale(Persec) + scale(HI) + scale(SI) + Age + Sex + Control + scale(ICARTot),
  na.action = na.fail,
  data = model1b
))

model1c <- model1 %>%
  filter(PartnerPolicy == "Competative") %>%
  select(Sum, Persec, Age, Sex, HI, SI, ICARTot, id, Control) %>%
  distinct()

model.compare(lm(
  scale(HI) ~ scale(Persec) + Age + Sex + scale(ICARTot) + Control,
  na.action = na.fail,
  data = model1c
))

model.compare(lm(
  scale(Sum) ~ scale(Persec) + scale(HI) + scale(SI) + Age + Sex + scale(ICARTot) + Control,
  na.action = na.fail,
  data = model1c
))



# Figure 2 ----------------------------------------------------------------

Regressions <- tribble(
  ~Trait,   ~Value, ~CILow, ~CIHigh, ~Partner,
   'Paranoia', -0.08, -0.14, -0.02,  "Overall",
   'ICAR',      0.11,  0.05,  0.17,  "Overall",
   'Paranoia', -0.05, -0.23,  0.03, "Prosocial",
   'ICAR',      0.01, -0.05,  0.20, "Prosocial",
   'Paranoia', -0.01, -0.16,  0.09, "Individualist",
   'ICAR',      0.11,  0.01,  0.22, "Individualist",
   'Paranoia', -0.17, -0.30, -0.04, "Competitive",
   'ICAR',      0.003, -0.09,  0.16, "Competitive",
   'Across Sample', 0.00, 0.00, 0.00, "Prosocial",
   'Across Sample',-1.39,-1.53,-1.24, "Individualist",
   'Across Sample',-0.56,-0.71,-0.42, "Competitive"
)

Regress <- Regressions %>%
  filter(Trait != "Across Sample") %>%
  mutate(Partner = factor(Partner,
                          levels = c("Overall",
                                     "Prosocial",
                                     "Individualist",
                                     "Competitive")),
         Trait   = factor(Trait,
                          levels = c("Paranoia",
                                     "ICAR"))) %>%

  ggplot()+
  geom_bar(
    aes(Trait, Value, fill = Partner),
    stat = 'identity',
    position = 'dodge') +
  geom_errorbar(
    aes(Trait, Value,
      ymin = CILow,
      ymax = CIHigh,
      group = Partner
      ),
    width = 0.2,
    position = position_dodge(0.9)) +

  scale_fill_manual(values = c("#E7298A" , "#7570B3", "#D95F02", "#1B9E77"))+

  labs(x = "",
       y = "Regression Coefficient") +

  bbplot::bbc_style() +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 14, colour = "grey"),
        legend.direction = "vertical",
        legend.position = c(0.75, 0.95),
        axis.title.y = element_text(size = 10 ))
Regress

AcrossSamp <- Regressions %>%

  filter(Trait == "Across Sample") %>%
  mutate(Partner = factor(Partner,
                          levels = c("Prosocial", "Competitive", "Individualist"))) %>%

  ggplot()+
  geom_bar(
    aes(Partner, Value),
        fill = "#2E86C1",
        stat = 'identity',
        position = 'dodge') +
  geom_errorbar(
    aes(Partner, Value,
      ymin = CILow,
      ymax = CIHigh
    ),
    width = 0.2,
    position = position_dodge(0.9)) +

  labs(x = "", y = 'Regression Coefficient') +

  bbplot::bbc_style() +
  theme(plot.title = element_text(size = 20),
        axis.title.y = element_text(size= 12),
        plot.subtitle = element_text(size = 14, colour = "grey"),
        legend.direction = "vertical",
        legend.position = c(0.75, 0.95))
AcrossSamp

#partner divisions by ordinal persecutory categories

HISIplot <- Intentions_guess %>%


  mutate(
    PersecLevel = ifelse(Persec > 3.66, 'High', 'Low'),
    PartnerPolicy = fct_reorder(PartnerPolicy, scale(HI), .desc = T)
  ) %>%
  rename('Harmful Intent' = HI,
         'Self Interest'  = SI) %>%
  pivot_longer(8:9, names_to = 'attribute', values_to = 'value') %>%
  dplyr::select(PartnerPolicy, value, attribute, PersecLevel, id) %>%
  distinct() %>%

  ggplot() +
  geom_jitter(aes(PartnerPolicy,
                  value,
                  color = PersecLevel,
                  alpha = value))+
  geom_boxplot(aes(PartnerPolicy,
                   value,
                   fill = PersecLevel),
               color = "black", outlier.shape = NA) +
  ggpubr::stat_compare_means(aes(PartnerPolicy,
                                 value,
                                 color = PersecLevel,
                                 alpha = value),
                             label = 'p.signif', size = 10,
                             label.y.npc = c(0.7, 0.8, 0.4, 0.2, 0.4, 0.2),
                             show.legend = F)+

  labs(y = 'Attribution Rating')+
  scale_x_discrete(labels = c('Competitive', 'Individualist', 'Prosocial'))+
  scale_fill_brewer(name = "Persecutory Ideation", palette = "Reds", direction = -1) +
  scale_colour_brewer(name = "Persecutory Ideation", palette = "Reds", direction = -1) +
  scale_alpha_continuous(guide = 'none')+
  facet_wrap(~ attribute)+
 coord_cartesian(clip = 'off')+
  theme(legend.position = c(0.4, 0.1),
        legend.direction = 'horizontal')+
  bbplot::bbc_style()+
  theme(legend.title = element_text(size = 12),
        legend.position = 'bottom',
        strip.text.x = element_text(hjust = 0.5 ,size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
HISIplot

library(patchwork)

attSum <- ggplot(indivParmsB %>%
         pivot_longer(10:11, names_to = 'attribute', values_to = 'value_a') %>%
         dplyr::select(id, Sum, PartnerPolicy, attribute, value_a) %>%
         distinct())+
  geom_smooth(aes(value_a, Sum, color = attribute), method = 'lm')+
  ggpubr::stat_cor(aes(value_a, Sum, color = attribute),  show.legend = F, label.y.npc = 0.6)+
  labs(x = 'Attribute Rating',y= 'Total Correct Answers in Phase 2')+
  scale_color_brewer(palette = 'Set1', name = 'Attribute', labels = c('Harmful Intent', 'Self Interest'))+
  facet_wrap(~PartnerPolicy)+
  theme_minimal()+
  theme(legend.position = c(0.9, 0.3),
        strip.text.x = element_text( size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.title = element_text(size =12),
        legend.text = element_text(size = 12),
        panel.grid.major.x =element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.box.background = element_rect(colour = "black"))

p1 <-(AcrossSamp +theme(axis.text.y = element_text(size = 12),
                      axis.text.x = element_text(size = 12)) |
  Regress + theme(legend.direction = 'vertical',
                   legend.position = c(0.1, 0.85),
                   legend.text = element_text(size = 10),
                   legend.key.size = unit(0.75,"line"),
                   plot.title = element_blank(),
                   plot.subtitle = element_blank(),
                   axis.title.y = element_text(size = 12),
                   axis.text.y = element_text(size = 12),
                   axis.text.x = element_text(size = 12),
                   legend.box.background = element_rect(colour = "black"))) /
  HISIplot + theme(legend.text = element_text(size = 12),
                   legend.title = element_text(size = 12),
                   legend.position = c(0.1, 0.2),
                   axis.text.y = element_text(size = 12),
                   legend.direction = 'vertical',
                   legend.box.background = element_rect(colour = "black"),
                   strip.text.x = element_text(size = 14, face = 'bold'))
  p1/attSum &  patchwork::plot_annotation(tag_levels = "A")

#explicit style

#Choices
Intentions_choice %>%
    mutate(Type1 = ifelse(Option1_PPT == 10 & Option1_Partner == 6 & Option2_PPT == 8 & Option2_Partner == 2, 'Individual',
                          ifelse(Option1_PPT == 12 & Option1_Partner == 6 & Option2_PPT == 10& Option2_Partner == 2, 'Individual',
                                 ifelse(Option1_PPT == 11 & Option1_Partner == 6 & Option2_PPT == 9 & Option2_Partner == 2, 'Individual',
                                        Type1))),
           Type2 = ifelse(Option1_PPT == 10 & Option1_Partner == 6 & Option2_PPT == 8 & Option2_Partner == 2, 'Competative',
                          ifelse(Option1_PPT == 12 & Option1_Partner == 6 & Option2_PPT == 10& Option2_Partner == 2, 'Competative',
                                 ifelse(Option1_PPT == 11 & Option1_Partner == 6 & Option2_PPT == 9 & Option2_Partner == 2, 'Competitive',
                                        Type2))),
           Choice = ifelse(Response == 1, Type1, Type2),
           ChoiceAction = ifelse(Choice == 'Prosocial', 1, ifelse(Choice == 'Individual', 2, 3))) %>%
    mutate(Prosocial=ifelse(ChoiceAction == 1, 1, 0),
           Individualistic=ifelse(ChoiceAction == 2, 1, 0),
           Competitive=ifelse(ChoiceAction == 3, 1, 0))->choice_alt

choice_alt %>%
  filter(PartnerPolicy == 'Competative') %>%
  group_by(ID) %>%
  mutate(ProsocialSum = sum(Prosocial),
         IndivSum = sum(Individualistic)) -> choiceModel

model.compare(lm(scale(ProsocialSum) ~ scale(Persec) + scale(ICARTot) + Age + Sex + Control, data = choiceModel, na.action = na.fail))
confint(lm(scale(IndivSum) ~ scale(Persec) + scale(ICARTot) + Age + Sex + Control, data = choiceModel, na.action = na.fail))

choice_alt %>%
  group_by(ID) %>%
  mutate(
         Prosocial = sum(Prosocial)/18,
         Individualistic = sum(Individualistic)/18,
         Competitive = sum(Competitive)/18) %>%
  ungroup() %>%
  dplyr::select(ID, Prosocial, Individualistic, Competitive,
                Persec, ICARTot, Age, Sex, Control) %>%
  distinct() -> Intentions_phase1choice


summary(glm(factor(ChoiceAction) ~ scale(Persec) + scale(ICARTot) + Age + Sex + Control,
            family = 'binomial',
                     data = Intentions_choice %>%
              mutate(ChoiceAction = ifelse(ChoiceAction == 3, 1, 0))))

Intentions_phase1choice %>%
  pivot_longer(2:4, names_to = 'Policy', values_to = 'Perc') -> forChi

# What is the effect of the treatment on the value ?
model=lm(Perc ~ Policy, data = forChi)
ANOVA=aov(model)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'Policy', conf.level=0.95)

Intentions_phase1choice %>%
    distinct() %>%
    summarise(P = mean(Prosocial),
              I = mean(Individualistic),
              C = mean(Competitive),
            Psd = sd(Prosocial),
            Isd = sd(Individualistic),
            Csd = sd(Competitive))

model.compare(lm(scale(Prosocial) ~ scale(Persec) + scale(ICARTot) + scale(Age) + Sex + Control, data = Intentions_phase1choice, na.action = na.fail))
model.compare(lm(scale(Individualistic) ~ scale(Persec) + scale(ICARTot) + scale(Age) + Sex + Control, data = Intentions_phase1choice, na.action = na.fail))
summary(lm(scale(Competitive) ~ scale(Persec) + scale(ICARTot) + scale(Age) + Sex + Control, data = Intentions_phase1choice, na.action = na.fail))

# S4 ----------------------------------------------------------------------

ggplot(Intentions_guess %>% mutate(PartnerPolicy = ifelse(PartnerPolicy == 'Competative', 'Competitive', PartnerPolicy)))+
  stat_summary(aes(Trial, Correct, fill = PartnerPolicy), geom = 'ribbon', alpha = 0.5)+
  stat_summary(aes(Trial, Correct, color = PartnerPolicy), geom = 'line')+
  scale_fill_brewer(palette = 'Dark2')+
  scale_color_brewer(palette = 'Dark2')+
  scale_y_continuous(breaks = c(0, 0.5, 1))+
  labs(x = 'Trial', y = 'p(Correct)')+
  theme_minimal()+
  theme(legend.position = c(0.75, 0.25),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))

# S5 ----------------------------------------------------------------------

Intentions_guess %>%
  filter(Final_Guess != "Please select an option") %>%
  mutate(PartnerPolicy = ifelse(PartnerPolicy == 'Competative', 'Competitive', PartnerPolicy),
         PersecLevel = ifelse(Persec >= 3.82, "High", "Low"),
         ICARLevel   = ifelse(ICARTot >= 5, "High", "Low")) %>%
  dplyr::select(Final_Guess,
         PersecLevel,
         ICARLevel,
         PartnerPolicy,
         ID) %>%
  distinct() %>%

  ggplot() +

  geom_bar(aes(PersecLevel, fill = Final_Guess),
           stat = 'count',
           position = "fill") +
  geom_label(aes(PersecLevel, fill = Final_Guess, label = ..count..),
             stat = 'count',
             position = "fill",
             vjust = 0.5,
             show.legend = F)+
  scale_fill_brewer(palette = "Dark2", name = "Final Guess")+

  facet_wrap( ~ PartnerPolicy)+
  bbplot::bbc_style()+
  theme(axis.text.y = element_blank(),
        strip.text.x = element_text(hjust = 0.5),
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.75,"line"))


