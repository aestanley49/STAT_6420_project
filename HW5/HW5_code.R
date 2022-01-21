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

### Load in data files 

prob2 <- read.table(here("HW5", "data5.2.txt"), header = T)
prob3 <- read.table(here("HW5", "data5.3.txt"), header = T)
prob4 <- read.table(here("HW5", "data5.4.txt"))
prob5 <- read.table(here("HW5", "data5.5.txt"))


### ### ### ### ### ### ### ###  Problems


### ### ### ### Problems 2... 

### ### 2d

g1 = lm(Weight~factor(Gender)+HC, data = prob2)
table_2d <- summary(g1)
kable(table_2d)


 ## ********** Need to write down model 

### ### 2e

g = lm(Weight~factor(Gender)+HC-1, data = prob2)
summary(g)


### ### 2f


# Model without Intercept 
plot(prob2$HC, prob2$Weight, xlim=c(-15,10), ylim=c(100,280), type="n",
     main="Model without Intercept", xlab="Centered Height",
     cex.lab=1.5, cex.main=2)
points(prob2$HC[1:10], prob2$Weight[1:10], pch=20,cex=2, col=2)
points(prob2$HC[11:20], prob2$Weight[11:20], pch=20,cex=2, col=4)
abline(g$coef[1], g$coef[3], lwd=3, col = 2)
abline(g$coef[2], g$coef[3], lwd=3, col = 4)
legend(-15,275,c("Female","Male"),fill=c(2,4),cex=.75)


# Model with Intercept 
plot(prob2$HC, prob2$Weight, xlim=c(-15,10), ylim=c(100,280), type="n",
     main="Model with Intercept", xlab="Centered Height",
     cex.lab=1.5, cex.main=2)
points(prob2$HC[1:10], prob2$Weight[1:10], pch=15,cex=2, col=2)
points(prob2$HC[11:20], prob2$Weight[11:20], pch=15,cex=2, col=4)
abline(g1$coef[1], g1$coef[3], lwd=3, col = 2)
abline(g1$coef[2]+g1$coef[1], g1$coef[3], lwd=3, col = 4)
legend(-15,275,c("Female","Male"),fill=c(2,4),cex=.75)


### ### ### ### Problems 3... 

### ### 3b  -- plot 

prob3_melt <- melt(prob3, id.vars = "Hours")


g = lm(value~factor(variable)*Hours, data = prob3_melt)
summary(g)

summary(g)


#plot(prob3_melt$Hours, prob3_melt$value)


plot(prob3_melt$Hours, prob3_melt$value,
     main = "Plot of Test Data", xlab = "Hours", ylab = "Average Scores")
points(prob3_melt$Hours[1:4], prob3_melt$value[1:4], pch=20,cex=2, col=2)
points(prob3_melt$Hours[5:8], prob3_melt$value[5:8], pch=20,cex=2, col=4)
points(prob3_melt$Hours[9:12], prob3_melt$value[9:12], pch=20,cex=2, col=3)
points(prob3_melt$Hours[13:16], prob3_melt$value[13:16], pch=20,cex=2, col=5)
abline(g$coef[1], g$coef[5], lwd=3, col = 2)
abline(g$coef[2]+g$coef[1], g$coef[6]+g$coef[5], lwd=3, col = 4)
abline(g$coef[3]+g$coef[1], g$coef[7]+g$coef[5], lwd=3, col = 3)
abline(g$coef[4]+g$coef[1], g$coef[8]+g$coef[5], lwd=3, col = 5)




### ### ### ### Problems 4... 

### Fit three separate regression models after removing (1) Libya, (2) Japan and (3) both
###  Look at the Cook’s distance plots
### Residual plots 

#plain model 
raw <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = prob4)
summary(raw)

countries <- rownames(prob4)
x <- c(1:length(countries))
cook_base <- cooks.distance(raw)
plot(x,cook_base, type = "n", main = "Cook's Distance base model", cex.main = 1, 
     xlab = "countries", ylab = "Cook's Distance")
segments(x,0,x, cook_base, lwd = 2)
identify(1:length(x), cook_base, countries, cex = .75)



#remove libya 
no_libya <- prob4[-c(49),]
no_L <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = no_libya)
summary(no_L)

x <- c(1:length(rownames(no_libya)))
cook_no_L <- cooks.distance(no_L)
plot(x,cook_no_L, type = "n", main = "Cook's Distance no Libya model", cex.main = 1, 
     xlab = "countries", ylab = "Cook's Distance")
segments(x,0,x, cook_no_L, lwd = 2)
identify(1:length(x), cook_no_L, countries, cex = .75)


#remove Japan
no_japan <- prob4[-c(23),]
no_J <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = no_japan)
summary(no_J)

x <- c(1:length(rownames(no_japan)))
cook_no_J <- cooks.distance(no_J)
plot(x,cook_no_J, type = "n", main = "Cook's Distance no Japan model", cex.main = 1, 
     xlab = "countries", ylab = "Cook's Distance")
segments(x,0,x, cook_no_J, lwd = 2)
identify(1:length(x), cook_no_J, countries, cex = .75)


#Remove Libya and Japan
no_libya_no_japan <- prob4[-c(23,49),]
no_J_or_L <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = no_libya_no_japan)
summary(no_J_or_L)

x <- c(1:length(rownames(no_libya_no_japan)))
cook_no_J_or_L <- cooks.distance(no_J_or_L)
plot(x,cook_no_J_or_L, type = "n", main = "Cook's Distance no Libya or Japan model", cex.main = 1, 
     xlab = "countries", ylab = "Cook's Distance")
segments(x,0,x, cook_no_J_or_L, lwd = 2)
identify(1:length(x), cook_no_J_or_L, countries, cex = .75)


# Residual plots 

savings <- prob4
g <- lm(sr ~ pop15+pop75+dpi+ddpi,savings)

d <- lm(sr ~ pop75 + dpi + ddpi,savings)$res
m <- lm(pop15 ~ pop75 + dpi + ddpi,savings)$res
plot(m,d,xlab="pop15 residuals",ylab="Saving residuals",
       main="Partial Regression")
abline(0,g$coef['pop15'])

plot(savings$pop15,g$res+g$coef['pop15']*savings$pop15,xlab="pop'n under 15",
       ylab="Saving(adjusted)",main="Partial Residual")
abline(0,g$coef['pop15'])


### ### ### ### Problems 5... 

## Full model with CIG as response 

model1 <- lm(CIG ~ BLAD+LUNG+KID+LEUK,prob5)

summary(model1)

## put together residual plots.. 

#Plot for BLAD
plot(prob5$BLAD,model1$res+model1$coef['BLAD']*prob5$BLAD,xlab="BLAD",
       ylab="CIG",main="Partial Residual BLAD")
abline(0,model1$coef['BLAD'])

#Plot for LUNG
plot(prob5$LUNG,model1$res+model1$coef['LUNG']*prob5$LUNG,xlab="LUNG",
     ylab="CIG",main="Partial Residual LUNG")
abline(0,model1$coef['LUNG'])

#Plot for KID
plot(prob5$KID,model1$res+model1$coef['KID']*prob5$KID,xlab="KID",
     ylab="CIG",main="Partial Residual KID")
abline(0,model1$coef['KID'])

#Plot for LEUK
plot(prob5$LEUK,model1$res+model1$coef['LEUK']*prob5$LEUK,xlab="LEUK",
     ylab="CIG",main="Partial Residual LEUK")
abline(0,model1$coef['LEUK'])


### ## Check for possible outliers by.. 

# Looking at residual vs fit 
plot(model1$fit,model1$res,xlab="Fitted",ylab="Residuals",main="Logged Response")

#Cook's distance
states <- rownames(prob5)
cook <- cooks.distance(model1)
plot(cook,ylab="Cooks distances")
identify(1:44,cook, states)

#Leaverage
x <- model.matrix(model1)
lev <- hat(x)
plot(lev,ylab="Leverages",main="Index plot of Leverages")

#Jackknife

jack <- rstudent(model1)
plot(jack,ylab="Jackknife Residuals",main="Jackknife Residuals")
jack[abs(jack)==max(abs(jack))]



### ## Okay so can see there are outliers - try removing the biggest (NE) first

prob5_no_NE <- prob5[-(26), ]

model2 <- lm(CIG ~ BLAD+LUNG+KID+LEUK,prob5_no_NE)

## repeat outlier checks..

# Looking at residual vs fit 
plot(model2$fit,model2$res,xlab="Fitted",ylab="Residuals",main="Logged Response")

#Cook's distance
states <- rownames(prob5_no_NE)
cook <- cooks.distance(model2)
plot(cook,ylab="Cooks distances")
identify(1:43,cook, states)

#Leaverage
x <- model.matrix(model2)
lev <- hat(x)
plot(lev,ylab="Leverages",main="Index plot of Leverages")

#Jackknife

jack <- rstudent(model2)
plot(jack,ylab="Jackknife Residuals",main="Jackknife Residuals")
jack[abs(jack)==max(abs(jack))]


### ## Still outliers so now lets try removing AK 


prob5_no_NE_AK <- prob5[-c(1,26), ]

model3 <- lm(CIG ~ BLAD+LUNG+KID+LEUK,prob5_no_NE_AK)

## repeat outlier checks..

# Looking at residual vs fit 
plot(model3$fit,model3$res,xlab="Fitted",ylab="Residuals",main="Logged Response")

#Cook's distance
states <- rownames(prob5_no_NE_AK)
cook <- cooks.distance(model3)
plot(cook,ylab="Cooks distances")
identify(1:42,cook, states)

#Leaverage
x <- model.matrix(model3)
lev <- hat(x)
plot(lev,ylab="Leverages",main="Index plot of Leverages")

#Jackknife

jack <- rstudent(model3)
plot(jack,ylab="Jackknife Residuals",main="Jackknife Residuals")
jack[abs(jack)==max(abs(jack))]


### ## Check for heteroskadsticity 

boxcox(model3,plotit=T)
boxcox(g,plotit=T,lambda=seq(0.5,1.5,by=0.1))


## Based on lecutre notes, we should take the log because non linear relationship.. 

model4 <- lm(log(CIG) ~ BLAD+LUNG+KID+LEUK,prob5_no_NE_AK)
summary(model4)


### can drop non significant predictors 

model5 <- lm(log(CIG) ~ LUNG+KID+LEUK,prob5_no_NE_AK)
summary(model5)


### Check final answer with res v fitted plot and QQplot 

plot(model5$fit,model5$res,xlab="Fitted",ylab="Residuals",main="Logged Response")

qqnorm(model5$res,ylab="Raw Residuals")
qqline(model5$res)

