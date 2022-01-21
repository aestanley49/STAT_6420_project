##############################################################################
### ### ###
### ### ###    STAT 6420 Homework no 10
### ### ###
#########################################################################


####### 
### ### ### Set up 

### Load in packages

library(here)
library(car)
library(MASS)
library(tidyverse)
library(faraway)

### Load in data files 

df2 <- read.table(here("HW10", "data10.2.txt"))
colnames(df2) <- c("dose", "cure")
df3 <- read.table(here("HW10", "data10.3.txt"))
colnames(df3) <-  c("flight_no", "temp", "y")
df4 <- read.table(here("HW10", "data10.4.txt"), header = T)
df5 <- read.table(here("HW10", "data10.5.txt"), header = T)

####### 
### ### ### Problem 2

  ### (a) Fit a probit model to the data.

#create table 
df2_a <- df2 %>% group_by(dose) %>% summarise(count = sum(cure)) #setting up data similar to page 7 of notes
#find proportion for each group (need denominator first so.. )
count <- df2 %>% group_by(dose) %>% summarize(count=n()) # so 20 per group is denominator 


#glm1 <- glm(Y ~ x, family = binomial(link = probit), data = df2)
glm1 <- glm(formula = cbind(df2_a$count, 20 - df2_a$count) ~ df2_a$dose, family = binomial(link = probit), data = df2_a)

summary(glm1)


glm1b <- glm(cure ~ dose, family = binomial(link = probit), data = df2)
summary(glm1b)
# Code done with Mandal: 
dx = seq(-10, 20, by = .1) ; a = glm1b$coef[1]  ; b = glm1b$coefficients[2]
eta = a + b*dx ; p = (exp(eta))/(1 + exp(eta))
plot(dx, p, type = "l", col = 2)
points(dose, cure)



  ### (b) Get a scatterplot of the proportion of successes at each distinct dose, and superimpose the estimated probability function on the scatterplot.

##df2 %>%
#  ggplot(aes(x=x, y=y)) +
 # labs(x = "Group Size", y = "Spatial Memory", title = "Magpie Spatial Memory as a function of Group Size") +
  #geom_point(size=2) +
  #geom_smooth(method="glm", formula = glm1)

plot(df2_a$dose, df2_a$count/20)
lines(df2_a$dose, pnorm(glm1$coef[1]+df2_a$dose*glm1$coef[2]), lwd=3, col = 2)



### (c) Suppose we do not know whether we prefer a probit or a logit link. 
  # Fit the simple logistic regression model and superimpose the estimated probability function on the scatterplot.

glm1c <- glm(formula = cbind(df2_a$count, 20 - df2_a$count) ~ df2_a$dose, family = binomial(link = logit), data = df2_a)

summary(glm1c)

plot(df2_a$dose, df2_a$count/20)
lines(df2_a$dose, pnorm(glm1c$coef[1]+df2_a$dose*glm1c$coef[2]), lwd=3, col = 3)


### (d) Just from comparing the plots do you think there is any strong evidence that one fit is better than the other?

plot(df2_a$dose, df2_a$count/20)
lines(df2_a$dose, pnorm(glm1c$coef[1]+df2_a$dose*glm1c$coef[2]), lwd=3, col = 3)
lines(df2_a$dose, pnorm(glm1$coef[1]+df2_a$dose*glm1$coef[2]), lwd=3, col = 2)

# The probability plots for logit and probit look the same. 
# But as seen in the class notes, if we plot ***, then they differ in the tails



####### 
### ### ### Problem 3

### (a) Using logistic regression, model the effect of temperature on the probability of thermal distress. 
  #Interpret the model fit. Get a scatterplot of the data and superimpose the estimated probability curve.



glm3 <- glm(formula = df3$y ~ df3$temp, family = binomial(link = logit), data = df3)

summary(glm3)


##################### STILL HAVING ISSUES WITH PLOT 
plot(df3$temp,df3$y, main="Fit of data", xlim = c(30,100))
points(df3$temp,ilogit(glm3$coef[1]+df3$temp*glm3$coef[2]), col = 2, pch = 20, type = "l")
#x1 <- sort(df3$temp)
x1 <- seq(30, 100, by = .25)
#y1 <- ilogit(glm3$coef[1]+x1*glm3$coef[2])
points(x1, y1, col = 5, pch = 20, type = "l")
#points(x1, y1, col = 4, pch = 20, type = "p")


lines(df3$temp,ilogit(glm3$coef[1]+df3$temp*glm3$coef[2]),lwd=4,col=3)
lines(df3$temp,pnorm(glm3$coef[1]+df3$temp*glm3$coef[2]),lwd=4,col=4)
abline(glm3)

dx = sort(df3$temp) ; a = glm3$coef[1]  ; b = glm3$coefficients[2]
eta = a + b*dx ; p =  1 / (1 + exp(-eta))
plot(dx, p, type = "b", col = 2, ylim= c(0,1))
points(df3$temp, df3$y)

# Code from Mandal - 
data = df3
flight = data[,1]
temp = data[,2]
td = data[,3]
x = c(53,55,57,63,66,67,68,69,70,72,73,75,76,78,79,81)
y = m = 0
for ( i in 1:length(x))
{
  y[i] = sum( td[temp==x[i]] )
  m[i] = length(temp[temp==x[i]])
}
model = glm(cbind(y,m-y)~x,family=binomial(link=logit))
summary(model)
plot(x,y/m,xlab="Temperature",ylab="Prportion of Distaster",ylim=c(0,1),
     main="Fit of data")
lines(x,ilogit(model$coef[1]+x*model$coef[2]),lwd=4,col=4)





### (b) Calculate the predicted probability of thermal distress at 31 degrees F
 # At what temperature does the predicted probability equal 0.5?

newdata = data.frame(temp = 31, 0)
df <- predict(glm3, temp = 31, type="response")


ilogit(glm3$coef[1] + glm3$coef[2]*31)  # source: https://gist.github.com/qingqingqing/9399694
pnorm(glm3$coef[1] + glm3$coef[2]*31)


probs = predict(glm3,df3, type="response")
value <- table(df3$temp, probs = .5)


### (c) Interpret the effect of temperature on the odds of thermal distress. 
 # Test the null hy- pothesis that the temperature has no effect, at α = 0.10.


anova(glm3,test="Chi")




####### 
### ### ### Problem 4

### (a) First analyze the data using lm() command. 
  # Do the residual analysis and transform the response as needed. 
  # Explain why you cannot do a Box-Cox transformation plot here? Clearly write the final model.


lm4 <- lm(Sa ~ factor(C) + factor(S) + Wt + W, data = df4)

plot(lm4)


r <- lm4$residuals
f <- lm4$fitted.values
plot(f ~ r) # so this has the fanning out shape

# Explain why you cannot do a Box-Cox transformation plot here? Clearly write the final model.
  # cannot do Box-cox because response variable must be positive (some of the response values are zero)

# the variance stabilizing transformation is the squareroot .. 

lm4a <- lm(sqrt(Sa) ~ W + factor(C) + factor(S) + Wt + W, data = df4)
## !!!!!!!!!! question here - in part d, W is in model twice, do we want to do the same here and if so why??


r <- lm4a$residuals
f <- lm4a$fitted.values
plot(f ~ r)

# Which seems to improve things some



### (b) Now fit a Poisson regression model with a log link.

lm4b <- glm(Sa ~ W + factor(C) + factor(S) + Wt + W, family = poisson(link = log), data = df4)

summary(lm4b)


### (c) Plot the response variable (Sa) on the x-axis and the appropriate fitted responses from (a) and (b) above. \
 # Compare these two plots.

#Not sure how to adapt this code.. ask Mandal? Not understanding seq function.. 
l_cut <- cut(df4$length, seq(40, 120, 10))
y_bin <- by(df$age, l_cut, mean)
y_cut <- cut(fitted(lm4b), quantile(fitted(lm4b), probs = seq(0, 1, 1/8)))
p_bin <- by(fitted(lm4b), y_cut, mean)

plot(y_bin, rev(p_bin), las = 1, pch = 16, cex = 1.3,
     ylab = "Observed", xlab = "Predicted")
abline(a = 0, b = 1, col = "gray")

# Plot for linear model: 
plot(df4$Sa, lm4a$fit^2, main = "Observed v Fitted for LM")
abline(a = 0, b = 1, col = "gray")


# Plot for generalized linear model: 
plot(df4$Sa,lm4b$fit, main = "Observed v Fitted for GLM")
abline(a = 0, b = 1, col = "gray")



### (d) Now do variable selection for the Poisson regression model. 
  # Then draw a scatter plot of response and fitted values from this model and compare with the plots in (c).



g2 = glm(Sa~W+factor(C)+factor(S)+Wt+W,family=poisson(link=log), data = df4)
g3 = update(g2,.~.-factor(S), data = df4)
g4 = update(g2,.~.-W, data = df4)


# Plot for generalized linear model: 
plot(df4$Sa,g4$fit, main = "Observed v Fitted for GLM after variable selection")
abline(a = 0, b = 1, col = "gray")


### (e) For the final model in (d), draw the index plot of residuals for the three types of residuals we mentioned in the lecture notes.
 # So the standard residuals, pearson residuals, deviance residuals 

standard = df4$Sa - g4$fit #observed - predicted 
pearson <-  residuals(g4, "pearson")
deviance <-  residuals(g4, "deviance")

plot(standard, col = 1, main = "Standard Residuals")
abline(h=0,col=1)

plot(pearson, col = 2, main = "Pearson Residuals")
abline(h=0,col=2)

plot(deviance, col = 3, main = "Deviance Residuals")
abline(h=0,col=3)



####### 
### ### ### Problem 5

install.packages("GLMsData")
library(GLMsData)
data(cins)
summary(cins)
g1 = glm(Cost~., data=cins, family=Gamma(link="inverse"))
g2 = glm(Cost~., data=cins, family=Gamma(link="log"))

### (a)  First analyze the data using lm() command. Do the residual analysis and transform the response as needed. 
 # Clearly write the final model.

lm5ai <- lm(Cost~., data=cins)

r <- lm5ai$residuals
f <- lm5ai$fitted.values
plot(f ~ r)  # One point is very different from the others.. 
identify(1:20,lm5ai$res)


# Leaverge: 
x <- model.matrix(lm5ai)
lev <- hat(x)
plot(lev)
abline(h = 2*5/20) #dim(cins)


# Influential observations
cook <- cooks.distance(lm5ai)
plot(cook)


# So we can address the influential point with a log transformation 

lm5aii <- lm(log(Cost)~., data=cins)

summary(lm5aii)

r <- lm5aii$residuals
f <- lm5aii$fitted.values
plot(f ~ r) 


### (b) Now fit a Gamma regression model with the log link.

glm5b <- glm(Cost~., family=Gamma(link="log"), data=cins)
summary(glm5b)



### (c) Fit a Gamma regression model with the canonical inverse link.

glm5c <- glm(Cost~., family=Gamma(link="inverse"), data=cins)
summary(glm5c)


### (d) Compare your results in (a, (b) and (c) above. Do all the predictors remain significant in all the models?
  # No, the Gamma regression model with the canonical inverse link has all predictors as significant where as the other two
  # models have all predictors but 3.








####### 
### ### ### Problem 6


### (f) conduct chi squared test 

a <- c(17, 33)
b <- c(26, 24)
d <- rbind(a,b)

chisq.test(d)
e <- chisq.test(d)
e$statistic



### (h) 
Model <- c("null", "A",  "B", "C", "D", "g")
Deviance <- c(136.663, 118.124, 134.002, 96.834, 95.409, 133.338)
parameters <- c(0,1,2,2,3,5)

h <- as.data.frame(rbind(Model, Deviance, parameters))
h <- t(h)
h <- as.data.frame(h)

h[,c(2:3)] <- sapply(h[ ,c(2:3)], as.numeric)

h2 <- h %>% mutate(AIC = Deviance + 2*parameters) %>% mutate(BIC = Deviance + log(100)*parameters)






set.seed(514)
## sample size
nn <- 80
## intercept
b0 <- 16
## slope
b1 <- -0.2
## lengths
sl <- seq(40, 120)
ll <- sample(sl, nn, replace = TRUE)

## probability as function of length
pp <- 1 / (1 + exp(-(b0 + b1*ll)))
## sim smolt age {0,1}
yy <- rep(NA, nn)
for(i in 1:nn) {
  yy[i] <- rbinom(1, 1, pp[i])
}

## make data frame for model fitting
df <- data.frame(length = ll, age = yy)
clr <- viridis::plasma(1, 0.8, 0.5, 0.5)

## set plot area
par(mai = c(0.9, 0.9, 0.1, 0.1),
    omi = c(0, 0, 0, 0),
    cex.lab = 1.5)
## plot age v
plot(ll, yy, las = 1, pch = 16, cex = 1.3, col = clr,
     yaxt = "n", ylab = "Smolt age", xlab = "Length (mm)")
axis(2, at = c(0,1), labels = c(1,2), las = 1)

fit_mod <- glm(age ~ length, data = df,
               family = binomial(link = "logit"))
faraway::sumary(fit_mod)


## get fitted values
newdata <- data.frame(length = seq(40, 120))
eta <- predict(fit_mod, newdata)
p_hat <- 1 / (1 + exp(-eta))

plot(ll, yy, pch = 16, cex = 1.3, col = clr,
     yaxt = "n", ylab = "Smolt age", xlab = "Length (mm)")
axis(2, at = c(0,1), labels = c(1,2), las = 1)
## add model fit
lines(sl, p_hat, lwd = 2)


old.par <- par(mar = c(0, 0, 0, 0))
par(old.par)


