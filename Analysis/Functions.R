
# Glossary ----------------------------------------------------------------

#model.compare() # run IT analysis on a model object
#mysamp() #sample data with limits


# IT Analysis  --------------------------------------------------------------

model.compare <- function(x){ # X needs to be a model object, e.g. lm, clm, lmer

  library(MuMIn)
  library(knitr)

#run MuMIn model averaging
x.set          <- dredge    (x, REML = F)
x.models       <- get.models(x.set, subset = delta<2)
x.a            <- model.avg (x.models, adjusted=FALSE, revised.var=TRUE)

#summarise global model
print(summary(x.a))
print(importance(x.a))
print(confint(x.a))

#coefs <- tidy(x.a, conf.int = T)

#plot models

#  library(hrbrthemes)
#
#ggplot(coefs %>%
#                       mutate(term = fct_reorder(term, estimate, .desc = T))) +
#
#  geom_pointrange(aes(x=term, y=estimate, ymin=conf.low, ymax=conf.high)) +
#  geom_hline(yintercept = 0, col = "orange") +
#
#  theme_ipsum()+
#  theme(axis.title.x = element_blank())
#
#
#
}

#using model.compare

#sample <- lm(Sepal.Length ~ Sepal.Width + Petal.Length * Petal.Width, data = iris,
#             na.action = na.fail)
#model.compare(sample)


# Sample data with limits -------------------------------------------------

mysamp <- function(n, m, s, lwr, upr, nnorm) {
  samp <- rnorm(nnorm, m, s)
  samp <- samp[samp >= lwr & samp <= upr]
  if (length(samp) >= n) {
    return(sample(samp, n))
  }
  stop(simpleError("Not enough values to sample from. Try increasing nnorm."))
}

# using mysamp

#mysamp(100, 0, 1, 0, 100, 10000)

