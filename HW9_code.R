##############################################################################
### ### ###
### ### ###    STAT 6420 Homework no 9
### ### ###
#########################################################################

####### 
### ### ### Set up 

### Load in packages

library(here)
library(car)
library(MASS)
library(tidyverse)
library(leaps)
library(DAAG)

### Load in data files 

prob <- read.table(here("data9.1.txt"), header = T, sep="\t")

lm1 <- lm(LogSurvival ~ blood.clotting + prognosis + enzyme.function + liver.function + age + gender + alcohol, data = prob)

####### 
### ### ### Problem 1

## "Prior to variable selection, identify outliers and influential points and exclude them (at least temporarily) 
## ".. also add any tranformations of the variables that seem appropriate" 

###   check for outliers and influential points.. 

# Leaverge: 
x <- model.matrix(lm1)
lev <- hat(x)
plot(lev)
abline(h = 2*5/54)
#sum(lev) ##9
lev[lev > .19] # so we have a lot of data points above the threshold.. 

# Influential observations
cook <- cooks.distance(lm1)
plot(cook)
identify(1:54, cook)

# Look at the residual vs fitted plot. Comment.
residual <- lm1$residuals
fitted <- lm1$fitted.values
plot(residual ~ fitted)

plot(lm1) ## qqplot okay? --> log normal? 
pairs(FecalSoil2[c(7,11:16)], lower.panel=panel.smooth, cex = .8, pch = 21, bg="steelblue",
      diag.panel=panel.hist, cex.labels = 1.2, font.labels=2, upper.panel=panel.cor)




###(a) Use the Backward Elimination Procedure to find the best fitting variables.

lm1a <- lm(LogSurvival ~ blood.clotting + prognosis + enzyme.function + liver.function + age + gender + alcohol, data = prob)
summary(lm1a) #remove liver.function
lm1a <- lm(LogSurvival ~ blood.clotting + prognosis + enzyme.function  + age + gender + alcohol, data = prob)
summary(lm1a) #can't remove alcoholnone because part of the same categorical variable ...
              # Next highest is age
lm1a <- lm(LogSurvival ~ blood.clotting + prognosis + enzyme.function + gender + alcohol, data = prob)
summary(lm1a) #remove gendermale
lm1a <- lm(LogSurvival ~ blood.clotting + prognosis + enzyme.function  + alcohol, data = prob)
summary(lm1a) 

###(b) Use the Forward Selection Procedure to find the best fitting variables.

lm1bi <- lm(LogSurvival ~ blood.clotting, data = prob)
summary(lm1bi) ##0.0726
lm1bi <- lm(LogSurvival ~ prognosis, data = prob)
summary(lm1bi) ## 0.000334
lm1bi <- lm(LogSurvival ~ enzyme.function, data = prob)
summary(lm1bi) ## 8.38e-08
lm1bi <- lm(LogSurvival ~ liver.function, data = prob)
summary(lm1bi) ## 1.1e-07
lm1bi <- lm(LogSurvival ~ age, data = prob)
summary(lm1bi) ## 0.295
lm1bi <- lm(LogSurvival ~ gender, data = prob)
summary(lm1bi) ## 0.0915 
lm1bi <- lm(LogSurvival ~ alcohol, data = prob)
summary(lm1bi) ## 0.0128

# best is enzyme.function so start with that 

lm1bii <- lm(LogSurvival ~ enzyme.function + blood.clotting, data = prob)
summary(lm1bii) ## 0.000531
lm1bii <- lm(LogSurvival ~ enzyme.function + prognosis, data = prob)
summary(lm1bii) ## 2.23e-07
lm1bii <- lm(LogSurvival ~ enzyme.function + liver.function, data = prob)
summary(lm1bii) ## 2.17e-05
lm1bii <- lm(LogSurvival ~ enzyme.function + age, data = prob)
summary(lm1bii) ## 0.196
lm1bii <- lm(LogSurvival ~ enzyme.function + gender, data = prob)
summary(lm1bii) ## 0.178 
lm1bii <- lm(LogSurvival ~ enzyme.function + alcohol, data = prob)
summary(lm1bii) ## 0.0111

# now add prognosis

lm1biii <- lm(LogSurvival ~ prognosis + enzyme.function + blood.clotting, data = prob)
summary(lm1biii) ## 5.66e-05
lm1biii <- lm(LogSurvival ~ prognosis + enzyme.function + liver.function, data = prob)
summary(lm1biii) ## 0.00311
lm1biii <- lm(LogSurvival ~ prognosis + enzyme.function + age, data = prob)
summary(lm1biii) ## 0.165
lm1biii <- lm(LogSurvival ~ prognosis + enzyme.function + gender, data = prob)
summary(lm1biii) ## 0.329 
lm1biii <- lm(LogSurvival ~ prognosis + enzyme.function + alcohol, data = prob)
summary(lm1biii) ## 2.80e-05 & 0.52

# lets add blood.clotting next (then probably alcohol)
lm1biv <- lm(LogSurvival ~ blood.clotting + prognosis + enzyme.function + liver.function, data = prob)
summary(lm1biv) ## 0.00311 --> not significant 
lm1biv <- lm(LogSurvival ~ blood.clotting + prognosis + enzyme.function + age, data = prob)
summary(lm1biv) ## 0.123
lm1biv <- lm(LogSurvival ~ blood.clotting + prognosis + enzyme.function + gender, data = prob)
summary(lm1biv) ## 0.366 
lm1biv <- lm(LogSurvival ~ blood.clotting + prognosis + enzyme.function + alcohol, data = prob)
summary(lm1biv) ## 0.000137 & 0.483260

# final model .. . 
lm1bv <- lm(LogSurvival ~ blood.clotting + prognosis + enzyme.function + alcohol, data = prob)
summary(lm1bv) ## 0.000137 & 0.483260

# Checked with Shiyuan...  Can also add liver function or stop here


###(c) Using the automated procedure for Stepwise Regression.

lmc <- step(lm1)

###(d) Identify the best number of variables.


#source - https://towardsdatascience.com/selecting-the-best-predictors-for-linear-regression-in-r-f385bf3d93e9

Best_Subset <- regsubsets(LogSurvival~., data =prob, nbest = 1,      # 1 best model for each number of predictors 
                          nvmax = NULL,    # NULL for no limit on number of variables 
                          force.in = NULL, force.out = NULL, method = "exhaustive") 
summary_best_subset <- summary(Best_Subset) 
as.data.frame(summary_best_subset$outmat)

which.max(summary_best_subset$adjr2) ## 6 out of 8 is max 
plot(summary_best_subset$adjr2) ## Can see the corner is at 4 so don't really need more than 4 for this model





###(e) Perform Cross-validation.


lm1e <- lm(LogSurvival ~ blood.clotting + prognosis + enzyme.function + liver.function + age + gender + alcohol, data = prob)

CV <- CVlm(lm1e, seed = 1, m=3, plotit = TRUE, data = prob)

attr(CV, "ms")

###(f) Choose a final model. Perform the residual analysis for your final model fitting surgery data set.

#options are as follows: 
lm1a
lm1bv
lmc

#Let's select the model from a... 
summary(lm1a)

plot(lm1a)

cook <- cooks.distance(lm1a)
plot(cook)


