##############################################################################
### ### ###
### ### ###    STAT 6420 Homework no 3 
### ### ###
#########################################################################

####### 
### ### ### Set up 

### Load in packages
library(here)
library(faraway) #for prob 4

### Load in data files 

one <- read.table(here("HW3", "data3.1.txt"), header = T)
three <- read.table(here("HW3", "data3.3.txt"))
four <- read.table(here("HW3", "data3.4.txt"))


### ### ### ### ### ### ### ###  Problems

### ### ### ### Problems 1... 

### ### 1a

reg1a <- lm(y ~ x, data = one)

summary(reg1a)

plot(one$y ~ one$x, xlab="Elapsed Time", ylab = "hardness (in Brinell units)")
abline(reg1a)


### ### 1c
var(one$x, one$y)

### ### 1d

#Get average values 
mean(one$x)

ave40 <- predict(reg1a, data.frame(x = 40))
ave40

### ### 1e

cf <- reg1a$coef
Slope <- cf[2]

### ### ### ### Problems 3... 

### ### 3 a

reg2 <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent , data = three)
summary(reg2)


### ### 3 b 

  ## Solve for Beta hat 
n <-  length(three$Species)
X <- cbind(rep(1,n), three$Area, three$Elevation, three$Nearest, three$Scruz, three$Adjacent)

X <- as.matrix(X)

#X <- model.matrix(reg2)
Y <- three$Species

XtXi <- solve(t(X)%*%X)
betahat <- XtXi %*%t(X)%*% three$Species


beta = solve(t(X) %*% X) %*% (t(X) %*% Y) ; beta #beta = betahat

print(t(round(beta,3)))


  ## solve for t - statistic where t = beta hat / SE beta hat 
res <- Y - X%*%beta
SSE = sum(res^2)
p = length(X[1,])
sigmahat = sqrt(SSE/n-p)
sigmahat

coveps <- sigmahat^2*XtXi

se <- sqrt(diag(XtXi))*sigmahat

t = betahat/se
print(t(round(t,3)))

### ### 3 c 

twoway <- aov(Species ~ Area + Elevation + Nearest + Scruz + Adjacent , data = three)
summary(twoway)


### ### ### ### Problems 4... 


### ### 4 a
data(savings)
savings

head(savings)


### ### 4 b

reg4 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)

summary(reg4)
