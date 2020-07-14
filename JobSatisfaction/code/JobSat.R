# ************************************************************************
# Title: JobSat.R
# Author: Kyle Wasserberger 
# Description: Import, clean, explore and analyze the Job Satisfaction
#              Data.
#              The research question is to estimate the relation between
#              satisfaction with pay and morale. There is also interest
#              in exploring possible gender and racial differences in 
#              job satisfaction, and in whether the percent of employees
#              in a department that are below the sample average of morale
#              moderates the relation between satisfaction with pay and 
#              morale.
#              Variables:
#              deptid - department id variable
#              morale - employee morale score
#              satpay - satisfaction with pay score
#              female - employee gender (0 = male, 1 = female)
#              white - employee racial category (0 = nonwhite, 1 = white)
#              pctbelow - percent of employees below sample average of morale
# Created: Monday, 22 June 2020
# Updated: 2020-06-28
# R version: R version 3.6.1
# ************************************************************************

# load packages ----

library(knitr)
library(psych)
library(car)
library(effects)
library(emmeans)
library(lme4)
library(parameters)
library(ggplot2)
library(texreg)
library(arm)
library(performance)
library(lmerTest)
library(interactions)

# Import and clean data ----

jobsat <- read.table("data/jobsat.txt", header = TRUE)

# view data summary
str(jobsat)

# clean factors
jobsat <- within(jobsat, {
  deptid <- factor(deptid)
  female <- factor(female,labels = c('male','female'))
  white <- factor(white,labels = c('non-white','white'))
})

# re-view data summary
x <- summary(jobsat)


# Summary statistics ----
describe(jobsat[,c(2,3,6)])


# Plot differences in satpay vs. morale by sex ----

p.sex <- ggplot(jobsat, aes(x = jitter(satpay), y = morale)) +
  geom_point(alpha = .3) +
  theme_bw() +
  facet_wrap(~ female) +
  geom_smooth(method = 'lm')

p.sex


# Plot differences in satpay vs. morale by race ----

p.race <- ggplot(jobsat, aes(x = jitter(satpay), y = morale)) +
  geom_point(alpha = .3) +
  theme_bw() +
  facet_wrap(~ white) +
  geom_smooth(method = 'lm')

p.race


# Plot differences in satpay vs. morale by sex & race ----

p.sr <- ggplot(jobsat, aes(x = jitter(satpay), y = morale)) +
  geom_point(alpha = .3) +
  theme_bw() +
  facet_wrap(~ white + female) +
  geom_smooth(method = 'lm')

p.sr



# Centering function ----

ctr <- function(x) {x - mean(x)}

c.satpay <- ctr(jobsat$satpay)
c.morale <- ctr(jobsat$morale)
c.pctbelow <- ctr(jobsat$pctbelow)

# Intercept-only Model ----

mod.base <- lmer(morale ~ 1 + (1 | deptid), data = jobsat, REML = FALSE)

screenreg(list(mod.base), 
          custom.model.names = c('Intercept-Only'))

# One predictor model (satpay) ----

mod.1 <- lmer(morale ~ satpay + (1 | deptid), data = jobsat, REML = FALSE)
mod.1c <- lmer(morale ~ c.satpay + (1 | deptid), data = jobsat, REML = FALSE)

screenreg(list(mod.base, mod.1, mod.1c),
          custom.model.names = c('Intercept-Only','One Predictor','One Predictor (centered)'))

# Three predictor model (satpay, female, white) ----

mod.3 <- lmer(morale ~ satpay + female + white + (1 | deptid), data = jobsat, REML = FALSE)
mod.3c <- lmer(morale ~ c.satpay + female + white + (1 | deptid), data = jobsat, REML = FALSE)

screenreg(list(mod.1, mod.1c,mod.3,mod.3c),
          custom.model.names = c('One Predictor', 'One Predictor Centered',
                                 'Three Predictor','Three Predictor Centered'))

# no significant main effect for sex

# Two predictor model (satpay, white) ----

mod.4 <- lmer(morale ~ c.satpay + white + (1 | deptid), data = jobsat, REML = FALSE)

# Add random effect for race
mod.4c <- lmer(morale ~ c.satpay + white + (1 + white | deptid), data = jobsat, REML = FALSE)

screenreg(list(mod.4, mod.4c),
          custom.model.names = c('SatPay + Race', 'Race Random Effect'))



# Interaction models ----

mod.int <- lmer(morale ~ c.satpay * white + (1 | deptid), data = jobsat, REML = FALSE)

screenreg(list(mod.int))

interact_plot(model = mod.int, pred = c.satpay, modx = white,
              x.label = 'Pay Satisfaction (centered)',
              y.label = 'Morale',
              interval = TRUE,
              legend.main = 'Race',
              main.title = 'Pay Satisfaction vs. Morale by Race')

# Percent below moderation model ----

mod.percent <- lmer(morale ~ c.satpay * c.pctbelow + (1 | deptid), data = jobsat, REML = FALSE)

screenreg(list(mod.percent))

interact_plot(model = mod.percent, pred = c.satpay, modx = c.pctbelow,
              x.label = 'Pay Satisfaction (centered)',
              y.label = 'Morale',
              interval = TRUE,
              legend.main = '%Below',
              main.title = 'Pay Satisfaction vs. Morale by %Below')

johnson_neyman(mod.percent,c.satpay,c.pctbelow)



