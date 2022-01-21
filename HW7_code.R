##############################################################################
### ### ###
### ### ###    STAT 6420 Homework no 7
### ### ###
#########################################################################

####### 
### ### ### Set up 

### Load in packages

library(here)
library(car)
library(MASS)
library(tidyverse)
library(kableExtra)

### Load in data files 

prob2 <- read.table(here("HW7", "data7.2.txt"), header = T)
prob3 <- read.table(here("HW7", "data7.3.txt"), header = T)
prob4 <- read.table(here("HW7", "data7.4.txt"), header = T)
prob5 <- read.table(here("HW7", "data7.5.txt"), header = T)
prob6 <- read.table(here("HW7", "data7.6.txt"), header = T)


####### 
### ### ### Problem 2

## 2a  
  # Fit a regression model with ‘species’ as response variable and ‘Area’, ‘Eleva- tion’, ‘Nearest’, 
  #‘Scruz’ and ‘Adjacent’ as predictor variables.

lm2 <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = prob2)
summary(lm2)


## 2b 
# Look at the residual vs fitted plot. Comment.

residual <- lm2$residuals
fitted <- lm2$fitted.values
plot(residual ~ fitted)

## 2c
  # Do the Box-Cox transformation.

boxcox(lm2,plotit=T)
boxcox(lm2,lambda=seq(0.0,1.0,by=0.05),plotit=T)

  ## Based on the box-cox plots, use transformation of = 0.5 or y ^ 1/2

  ## Checking... 

lm2b <- lm((Species^.5) ~ Area + Elevation + Nearest + Scruz + Adjacent, data = prob2)
summary(lm2b)

residual <- lm2b$residuals
fitted <- lm2b$fitted.values
plot(residual ~ fitted)



####### 
### ### ### Problem 3

## Lets make a scatter plot to look at the data distribution 
plot(prob3$x, prob3$y)

## The relationship isn't quiet linear so take log of both variables 

prob3 <- prob3 %>% mutate(logx = log(x)) %>% mutate(logy = log(y)) 
plot(prob3$logx, prob3$logy)

## Look at model summary

lm3 <- lm(logy ~ logx, data = prob3)
summary(lm3)

## Now check the residuals and qq plot 

residual <- lm3$residuals
fitted <- lm3$fitted.values
plot(residual ~ fitted)

qqnorm(lm3$res,ylab="Raw Residuals")
qqline(lm3$res)





####### 
### ### ### Problem 4

## The full marks for the first three homeworks were 30, 20 and 20 respectively, 
    #and that for the first midterm exam was 20.

## Need to change from raw score to fraction (scale?)


prob4 <- prob4 %>% mutate(H1 = H1/30) %>% mutate(H2 = H2/20)%>% mutate(H3 = H3/20) %>% mutate(E1 = E1/20)

lm4 <- lm(E1 ~ H1 + H2 + H3, data = prob4)
summary(lm4)

residual <- lm4$residuals
fitted <- lm4$fitted.values
plot(residual ~ fitted)

### We want to look for outliers or influential points 
  # So we could use leverages (leverage is a measure of how far away the independent variable values of an observation are from those of the other observations)
  
prob4$predicted <- predict(lm4)

prob4a <- prob4 %>% mutate(difference = abs(predicted - E1)) %>% mutate(student_ID = rownames(prob4)) %>% arrange(-difference)

kable(head(prob4a))

## For each student, create table with the difference btw the observed and predicted values of exam scores 





####### 
### ### ### Problem 5
 ## First have to fit the full model.. 

lm5a <- lm(y ~ x1 + x2 + x3, data = prob5)
summary(lm5a) ## Nothing is significant but the R-squared values are very high 

vif(lm5a) #Values are high 


## Correlation matrix

#set up
PredictorsOnlyPixel <- prob5$y
PredictAndResponsePixel <- prob5[,-c(4)]
PredictAndResponseGrid <- reg_prdctr_init[,-c(4)]

# put histograms on the diagonal panel	
panel.hist <- function (x,...)					# define a function that says what we want to plot in the diagonal
{
  usr <- par("usr"); on.exit(par(usr))			# not sure what usr is for?
  par(usr = c(usr[1:2],0,1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)			# make the hist 
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="grey", ...)  # defines what the histogram is going to look like
}

# put correlations on the upper panels,
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y,use="complete.obs")				
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  prefix <- "r = "
  rc <- cor.test(x,y,method = c("pearson"))				## calculate pearsons rho for upper grid
  txt <- paste(prefix,txt,sep="")
  text(0.5, 0.5, txt, cex = 1)
}

pairs(PredictAndResponsePixel,lower.panel = panel.smooth, diag.panel=panel.hist,upper.panel=panel.cor)

## Looks like x1 and x2 are highly correlated 
## Lets remove x1 because it has a high correlation with x3 



# Re-running model 
lm5b <- lm(y ~  x2 + x3, data = prob5)
summary(lm5b) ## Nothing is significant but the R-squared values are very high 

vif(lm5b) #Values are normal now

#We can remove x3 and check the final model 

lm5c <- lm(y ~  x2, data = prob5)
summary(lm5c) ## Nothing is significant but the R-squared values are very high 

plot(lm5c)




####### 
### ### ### Problem 6 b 


# rate is response variable (first column)

x <- as.matrix(prob6[,-1])
x <- scale(x)
e <- eigen(t(x) %*% x)
e$val
sqrt(max(e$val)/min(e$val)) #conditional number


# Now check out VIFs


a <- summary(lm(x[,1] ~ x[,-1]))$r.squared
# 0.99262
1/(1-a )

diag(solve(cor(x)))
