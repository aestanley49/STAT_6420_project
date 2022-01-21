##############################################################################
### ### ###
### ### ###    STAT 6420 Homework no 5 
### ### ###
#########################################################################

####### 
### ### ### Set up 

### Load in packages
library(here)
library(tidyverse)
library(reshape)
library(kableExtra)
library(MASS) #for box-cox
library(stargazer) #for court case problem

### Load in data files 

prob1 <- read.table(here("HW6", "data6.1.txt"), header = T)
prob2 <- read.table(here("HW6", "data6.2.txt"), header = T)


####### 
### ### ### Problem 1

 ### 1a Make dummy variables for the categorical variables (sediment, treatment, and block).
factor(prob1$sediment)
factor(prob1$treatment)
factor(prob1$block)

prob1$Sed_S <- ifelse(prob1$sediment == 'S', 1, 0)
prob1$Sed_N <- ifelse(prob1$sediment == 'N', 1, 0)

prob1$Treat_C <- ifelse(prob1$treatment == 'C', 1, 0)
prob1$Treat_E <- ifelse(prob1$treatment == 'E', 1, 0)

prob1$Block_1 <- ifelse(prob1$block == '1', 1, 0)
prob1$Block_2 <- ifelse(prob1$block == '2', 1, 0)
prob1$Block_3 <- ifelse(prob1$block == '3', 1, 0)
prob1$Block_4 <- ifelse(prob1$block == '4', 1, 0)
prob1$Block_5 <- ifelse(prob1$block == '5', 1, 0)



 ### 1b Plot the response against the time variable. 

plot(prob1$day, prob1$chl)



 ### 1c 

    ## Regression fit of model 

prob1$block

lm1a <- lm(chl ~ Sed_S + Sed_N + Treat_C + Treat_E + Block_2 + Block_3 + Block_4 + Block_5 + day, data = prob1)
### Getting NAs here for some reason
## I don't know why these are different
lm1b <- lm(chl ~ factor(sediment) + factor(treatment) + factor(block) + day, data = prob1)

summary(lm1b)

    ## Anova of Model 

anova(lm1b)


    ## F test for the block variable
    ## --> get rid of nonsig data and refit..
 ## Not sure how to select from factor levels but I know I can remove the data bu subsetting 

prob1a <- prob1[-which(prob1$block == 2),] #remove block 2
prob1a <- prob1a[-which(prob1a$sediment == "S"),]

prob1b <- prob1[-which(prob1$sediment == "S"),]

## lm1c <- lm(chl ~ factor(treatment) + factor(block) + day, data = prob1a) ## numbers here are wrong

lm1c <- lm(chl ~ factor(treatment) + factor(block, exclude=c(2)) + day, data = prob1a)
## how to exclude factors lm(dependent ~ factor(independent1, exclude=c('b','d')) + independent2)

## try making block 2 NA
prob1c <- prob1

#prob1c$sediment <- as.numeric(prob1c$sediment)
prob1c[which(prob1c$block == 2),3] <- NA

lm1c <- lm(chl ~ factor(treatment) + factor(block) + day, data = prob1c)


summary(lm1c)

## Anova of Model 

anova(lm1c)




  ### 1d Residuals vs. Fitted Values 

r <- lm1c$residuals
f <- lm1c$fitted.values
plot(f ~ r)



  ### 1e  Now you can decide to do a variance-stablizing transformation of the response. Repeat (b) through (d) 


# So first we want to look at the box-cox 
boxcox(lm1c,lambda = seq(-.25,.25,by=0.05) ) 
# lamda is close to 0 so we want to log y 

prob1c <- prob1c %>% mutate(chl = log(chl))

plot(prob1c$day, prob1c$chl)

lm1e <- lm(chl ~ factor(sediment) + factor(treatment) + factor(block) + day, data = prob1c)

summary(lm1e)

# Anova of Model 

anova(lm1e)


r <- lm1e$residuals
f <- lm1e$fitted.values
plot(f ~ r)


  ### 1f

#qqplot
plot(lm1e)

#leaverges
x = model.matrix(lm1e)
n = dim(x)[1]
p = dim(x)[2]
lev = hat(x)
plot(lev,ylab="Leverages",main="Index plot of Leverages")

#cooks

cook = cooks.distance(lm1e)
plot(cook)

####### 
### ### ### Problem 2

  ### 2a
# Obtain a test of the hypothesis that salary adjusted for years in current rank, highest degree, 
#and years since highest degree is the same for each of the three ranks, versus the alternative that the 
#salaries are not the same. 


lm2 <- lm(SL ~ factor(RK) + YR + factor(DG) + YD, data = prob2)

summary(lm2)

# This lets us test:
  # Rank 1 = Rank 2
  # Rank 1 = Rank 3 

# Need to test Rank 2 = Rank 3 


x = model.matrix(lm2)
n = length(prob2$SL)
p = length(lm2$coef)
SSE = sum(lm2$res^2)
round(solve(t(x)%*%x),4)*SSE/(n-p)

#Calculate the t-value
t = (lm2$coef[2]-lm2$coef[3])/sqrt(1115085.317+1613785.960-2*845344.198)
print(t)
print(pt(t,n-p))




  ### 2b

# First check the residuals vs. Fitted Values

plot(lm2)


# Box-Cox transformation 
boxcox(lm2,lambda = seq(-1.5,1.0,by=0.05) ) 




  ### 2c  test for nonconstant variance as a function of the sex indicator

prob2a <- prob2 %>% mutate(SL = log(SL))

boxplot(prob2a$SX, prob2a$SL)

prob2a %>% ggplot(aes(x=SX,y=SL, group = SX)) + 
  geom_boxplot()





### 2d  Test to see if the sex differential in transformed salary is the same in each rank.

# Creating subsetted dataframes: 

Assistant <- prob2a[which(prob2a$RK == 1),]
Associate <- prob2a[which(prob2a$RK == 2),]
Full <- prob2a[which(prob2a$RK == 3),]


# Run the same model with each subset: 

lm2d_Assistant <- lm(SL ~  YR+ factor(SX) + factor(DG) + YD, data = Assistant)
lm2d_Associate <- lm(SL ~  YR + factor(SX) + factor(DG) + YD, data = Associate)
lm2d_Full <- lm(SL ~  YR + factor(SX) + factor(DG) + YD, data = Full)

summary(lm2d_Assistant)
summary(lm2d_Associate)
summary(lm2d_Full)





### 2e  Using all the predictors, analyze these data with regard to the question of differential salaries for men 
#and women faculty, and summarize your results in a fashion that might be useful in court.

lm2ei <- lm(SL ~ factor(SX) + factor(RK) + YR + factor(DG) + YD, data = prob2a)
lm2eii <- lm(SL ~  factor(RK) + YR + factor(DG) + YD, data = prob2a) #remove sex 
lm2eiii <- lm(SL ~  factor(SX) + YR + factor(DG) + YD, data = prob2a) #remove rank 
lm2eiiii <- lm(SL ~   YR + factor(DG) + YD, data = prob2a) # remove sex and rank  
lm2eiiiii <- lm(SL ~   YR + factor(RK), data = prob2a) # final model 


stargazer(lm2ei,  lm2eii, lm2eiii, lm2eiiii, lm2eiiiii, 
type = "html", title = "Gender discrimination", align = TRUE, out = "HW6_table.html", # AIC isn't calculated for lm models so adding manually
star.char = c("+", "*", "**", "***"),
star.cutoffs = c(.1, .05, .01, .001),  
notes.append = FALSE, 
notes = c("+ p<.1; * p<0.05; ** p<0.01; *** p<0.001"),
 add.lines=list(
   c("AIC", round(AIC(lm2ei),1) , c("AIC", round(AIC(lm2eii),1), c("AIC", round(AIC(lm2eiii),1) , c("AIC", round(AIC(lm2eiiii),1), c("AIC", round(AIC(lm2eiiiii),1)
                                                                   ))  )))))


### 2f  Fit exactly the same model you found in (2e), except leave out the effects of rank. 
#Summarize and compare the results of leaving out rank effects on inferences concerning differential in pay by sex.

# Decided to include this information in the same table above for e






