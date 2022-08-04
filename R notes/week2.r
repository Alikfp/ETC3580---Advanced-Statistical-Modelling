library(faraway)
library(tidyverse)
library(visreg)

#############################################
# gavote example
#############################################

gavote <- gavote %>%
  as_tibble() %>%
  rename(usage = rural) %>%
  mutate(
    undercount = (ballots - votes) / ballots,
    pergore = gore / votes,
    county = rownames(faraway::gavote)
  )

# Fit big regression
lmod <- lm(
  undercount ~ pergore + perAA + equip + econ + usage + atlanta,
  data=gavote)

# Variable selection
summary(lmod)
best1 <- step(lmod)
summary(best1)
AIC(best1)


# include 2-way interactions
best2  <- lm(
    undercount ~ (pergore + perAA + equip + econ + usage + atlanta)^2,
    data=gavote) %>% 
  step(trace=FALSE)
# Best model:
summary(best2)
AIC(best2)
# Why are there missing values?
gavote %>% 
  count(econ, equip) %>%
  spread(equip, n)
## Too few observations for paper

visreg(best2, 'econ', by='equip', gg=TRUE) + theme_bw()
visreg(best2, 'perAA', by='equip', gg=TRUE) + theme_bw()
visreg(best2, 'pergore', by='usage', gg=TRUE) + theme_bw()
visreg2d(best2, 'perAA', 'pergore')

# Diagnostic plots for linear model
library(broom)
lmod_aug <- augment(best2) %>%
  bind_cols(select(gavote, atlanta))

# Check heteroscedasticity
lmod_aug %>%
  ggplot(aes(x=.fitted, y=.resid)) +
    geom_point() + geom_smooth()

# Check linearity of relationships and homogeneity of groups
lmod_aug %>%
  ggplot(aes(x=pergore, y=.resid)) +
  geom_point() + geom_smooth()
lmod_aug %>%
  ggplot(aes(x=perAA, y=.resid)) +
  geom_point() + geom_smooth()
lmod_aug %>%
  ggplot(aes(x=equip, y=.resid)) +
  geom_boxplot()

lmod_aug %>%
  gather(x, val, c("atlanta","econ","equip","perAA","pergore","usage")) %>%
  ggplot(aes(val, .resid)) +
    geom_point() +
    facet_wrap(~x, scales='free')

# Check residual normality
lmod_aug %>%
  ggplot(aes(x=.resid)) +
  geom_histogram(bins = nclass.FD(lmod_aug$.resid),
                 boundary=0)
lmod_aug %>%
  ggplot(aes(sample=.resid)) +
  geom_qq()

# Look at cases with large residuals
lmod_aug %>%
  filter(.resid > 0.075) %>%
  glimpse()

# Identify influential points 
lmod_aug %>% 
  ggplot(aes(x=.cooksd)) + 
  geom_dotplot() 
lmod_aug %>% 
  filter(.cooksd > 0.2) %>% 
  glimpse() 

lmod_aug %>% 
  ggplot(aes(x=.hat)) + 
  geom_dotplot() 
lmod_aug %>% 
  filter(.hat > 0.5) %>% 
  glimpse() 
gavote %>% 
  count(econ, equip) %>%
  spread(equip, n)