##############################################################################
### ### ###
### ### ###    STAT 6420 Homework no 8
### ### ###
#########################################################################

####### 
### ### ### Set up 

### Load in packages

library(here)
library(car)

### Load in data files 

#prob <- read.table(here("HW6", "data6.1.txt"), header = T)


####### 
### ### ### Problem 1

  ## 1a 

Fertilizer = c(12,5,15,17,20,14,6,23,11,13,8,18,22,25)
fert_sqed <- Fertilizer^2
Yield = c(24,18,31,33,26,30,20,25,25,27,21,29,29,26)

lm1 <- lm(Yield ~ (Fertilizer + fert_sqed))
lm2 <- lm(Yield ~ (Fertilizer))   ### !!!! This isn't working

  ## 1b 

plot(Yield ~ Fertilizer)
plot(function(x){coef(lm1)[1]+coef(lm1)[2]*x + coef(lm1)[3]*x^2}, from = 0, to = 30, add = T, col ="red")


plot(Yield ~ Fertilizer)
abline(lm2)



  ## 1c 
model1 <- lm(Yield ~ (Fertilizer + fert_sqed))

group = c(3,1,5,6,7,4,1,8,3,4,2,6,8,9)
dat = cbind(group,Fertilizer,Yield)
dat = dat[order(group),]

model2 = lm(Yield~factor(group))
summary(model2)
aov2 = aov(model2)
summary(aov2)

SSpe = sum((aov2$res)^2)
dfpe = aov2$df.res


summary(model1)
aov1 = aov(model1)
summary(aov1)

####### 
### ### ### Problem 4

  ## 4b

prob4 <- read.table(here("HW8", "data8.4.txt"), header = T)

# rate is response variable (first column)

x <- as.matrix(prob4[,-1])
x <- scale(x)
e <- eigen(t(x) %*% x)
e$val
sqrt(max(e$val)/min(e$val)) #conditional number


# Now check out VIFs


a <- summary(lm(x[,1] ~ x[,-1]))$r.squared
 # 0.99262
1/(1-a )

diag(solve(cor(x)))


    ## 4d (6 points) Do variable selection in R. Choose a final model.


lmd <- lm(RATE ~ ., data = prob4)

# Using the automated procedure for Stepwise Regression.

stepped <- step(lmd)

#returns: 

lmdi <- lm(formula = RATE ~ LEN + SLIM + SIGS + ACPT + PA, data = prob4)

summary(lmdi)

    ## 4e (3 points) Check whether everything is ok with the final model. (i.e. look at the residual vs fitted and Q-Q plots.)


plot(lmdi) #residual v fitted look weird 
# check weirdness with box-cox

boxcox(lmdi,plotit=T)
boxcox(lmdi,lambda=seq(-.75,.75,by=0.05),plotit=T) ## Lambda = 0 

## Based on the box-cox plots, use log transformation 

prob4 <- prob4 %>% mutate(RATE = log(RATE))

# So now need to refit everything... 

lmd <- lm(RATE ~ ., data = prob4)
# Using the automated procedure for Stepwise Regression.

step(lmd)

lmdii <- lm(formula = RATE ~ LEN + SLIM + SIGS + PA, data = prob4)
summary(lmdii)
plot(lmdii)


## Now everything looks fine
