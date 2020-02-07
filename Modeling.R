####################
### Prepare Data ###
####################

### Prepare Data ###

setwd("D:/DUKE/Courses/STA 290 Bayesian and Modern Statistics/Final Project")

raw <- read.fwf("usa_00006.dat", width = c(4, 2, 8, 10, 4, 1, 4, 10, 1, 3, 1, 1, 2, 1, 2, 7, 7, 4, 2, 3, 4, 4))
raw <- cbind(raw[ , 5], raw[ , 8:22])

colnames(raw) = c("LivingCity", "PersonalWeight", "Children<5", "Age", "Sex",
                  "Employment", "EmploymentD", "ClassWork", "ClassWorkD", "IncomeP", 
                  "IncomeF", "WorkingCity", "Transportation", "Time", "Depart", "Arrive")

raw <- cbind(raw[ , 1:6], raw[ , 8], raw[ , 10:16])
attach(raw)

data <- subset(raw, 
               ( LivingCity== 4610 | LivingCity== 1190 | LivingCity == 5330 | LivingCity == 6290) 
               & (WorkingCity == LivingCity) & Employment == 1 & ClassWork == 2 
               & Time > 0 & Time < 120 & (Transportation == 31 | Transportation == 33
                                          |Transportation == 34 | Transportation == 35))
# the above: public transportation only
save(data, file = "data.Rdata")
detach(raw)

# Restrict to morning hours

data <- cbind(data[ , 2:5], data[ , 8:14])
data1 <- subset(data, data$Depart>659 & data$Depart<931 & data$Arrive > 729 & data$Arrive <1001)

# Randomly select
NYC <- subset(data1, data1$"WorkingCity" == 4610)
Chi <- subset(data1, data1$"WorkingCity" == 1190)
Phi <- subset(data1, data1$"WorkingCity" == 5330)
SF  <- subset(data1, data1$"WorkingCity" == 6290)

NYC <- NYC[sample(nrow(NYC),1000), ]
Chi <- Chi[sample(nrow(Chi),1000), ]
# Phi <- Phi[sample(nrow(Phi),300), ]
# SF  <- SF[sample(nrow(SF),300), ]

alldata <- rbind(NYC, Chi, Phi, SF)
save(alldata, file = "clear data.Rdata")

# Histograms

hist((alldata[ , 9]), breaks=20)
hist(log(alldata[ , 9]), breaks=20)

par(mfrow = c(2,2))
hist((NYC[ , 9]), breaks=15, main = "NYC")
hist((Chi[ , 9]), breaks=15, main = "Chicago")
hist((Phi[ , 9]), breaks=15, main = "Philadelphia")
hist((SF[ , 9]), breaks=15, main = "San Francisco")


hist(log(NYC[ , 9]), breaks=20, main = "NYC")
hist(log(Chi[ , 9]), breaks=20, main = "Chicago")
hist(log(Phi[ , 9]), breaks=20, main = "Philadelphia")
hist(log(SF[ , 9]), breaks=20, main = "San Francisco")

dev.off()

# Other EDAs

library(corpcor)
library(mvtnorm)
library(lattice)
library(coda)
library(MCMCpack)

par(mfrow = c(2,2))
boxplot(log(alldata$Time) ~ alldata$WorkingCity, ylab="logtime", xlab="1190=Chicago, 4610=NYC, 5330=Phi., 6290=SF")
boxplot(log(alldata$Time) ~ alldata$Sex, ylab="logtime", xlab="1=Male, 2=Female")
boxplot(log(alldata$Time) ~ alldata$Transportation, ylab = "logtime", xlab = "31=Bus, 33=Subway, 34=Railroad, 35=Taxi")
boxplot(log(alldata$Time) ~ alldata$Children, ylab="logtime", xlab="Number of Children under 5 years old")

attach(alldata)
xyplot(log(Time)~log(IncomeP) | WorkingCity, main = "log(Time)~log(IncomeP) XYPlot by WorkingCity")
xyplot(log(Time)~log(IncomeP) | Transportation, main = "log(Time)~log(IncomeP) XYPlot by Transportation")
xyplot(log(IncomeP)~Transportation | WorkingCity, main = "log(IncomeP)~Transportation XYPlot by WorkingCity")

# Some changes

Transportation=factor(Transportation)
WorkingCity=factor(WorkingCity)
Sex=factor(Sex)
levels(WorkingCity) = c("Chi", "NYC", "Phi","SF")  
levels(Transportation)=c("Bus", "Subway", "Railroad", "Taxi")
levels(Sex)=c("Male", "Female")

##creating dummy variables and other preparations
SexD = model.matrix(~Sex-1)
alldata[11:12, "SexD"] <- NA
alldata$SexD <- SexD
TransportationD = model.matrix(~Transportation-1)
alldata[12:13, "TransportationD"] <- NA
alldata$TransportationD<-TransportationD

Tran1 <- TransportationD[, 2]
Tran1 <- TransportationD[, 1]
Tran2 <- TransportationD[, 2]
Tran3 <- TransportationD[, 3]
Tran4 <- TransportationD[, 4]
Sex <- SexD[ , 1]
data2 <- cbind(alldata[,2:3], Sex, alldata[,5:7], Tran1, Tran2, Tran3, Tran4, alldata[, 9:11])
alldata <- data2 [ , 1:11]
alldata <- cbind(alldata[ , 1:4], alldata[ , 6:11])
save(alldata, file = "clear data.Rdata")

####################
### MCMC Process ###
####################

### Prepare Prior ###

aa=log(alldata$IncomeP)
age=Age-mean(Age)
alldata=cbind(alldata, aa, age)
attach(alldata)

##find coeff for OLS 

ols=lm(log(Time)~alldata$Children+age+Sex+log(IncomeP)+Tran1+Tran2+Tran4)

Y <- list()
X <- list()
N <- NULL
Y[[1]] <- alldata[alldata$WorkingCity == 1190, 10]
Y[[2]] <- alldata[alldata$WorkingCity == 4610, 10]
Y[[3]] <- alldata[alldata$WorkingCity == 5330, 10]
Y[[4]] <- alldata[alldata$WorkingCity == 6290, 10]
N[1] <- sum(alldata$WorkingCity == 1190)
N[2] <- sum(alldata$WorkingCity == 4610) 
N[3] <- sum(alldata$WorkingCity == 5330)
N[4] <- sum(alldata$WorkingCity == 6290)
x11 <- alldata[alldata$WorkingCity == 1190, 1] #Children number
x21 <- alldata[alldata$WorkingCity == 1190, 12] #age
x31 <- alldata[alldata$WorkingCity == 1190, 3] #Sex
x41 <- alldata[alldata$WorkingCity == 1190, 11] #log Personal Income
x51 <- alldata[alldata$WorkingCity == 1190, 6] #Transportation--Bus
x61 <- alldata[alldata$WorkingCity == 1190, 7] #Transportation--Subway
x71 <- alldata[alldata$WorkingCity == 1190, 8] #Transportation--Railroad
x81 <- alldata[alldata$WorkingCity == 1190, 9] #Transportation--Taxi

x12 <- alldata[alldata$WorkingCity == 4610, 1] #Children number
x22 <- alldata[alldata$WorkingCity == 4610, 12] #age
x32 <- alldata[alldata$WorkingCity == 4610, 3] #Sex
x42 <- alldata[alldata$WorkingCity == 4610, 11] #log Personal Income
x52 <- alldata[alldata$WorkingCity == 4610, 6] #Transportation--Bus
x62 <- alldata[alldata$WorkingCity == 4610, 7] #Transportation--Subway
x72 <- alldata[alldata$WorkingCity == 4610, 8] #Transportation--Railroad
x82 <- alldata[alldata$WorkingCity == 4610, 9] #Transportation¡ªTaxi

x13 <- alldata[alldata$WorkingCity == 5330, 1] #Children number
x23 <- alldata[alldata$WorkingCity == 5330, 12] #age
x33 <- alldata[alldata$WorkingCity == 5330, 3] #Sex
x43 <- alldata[alldata$WorkingCity == 5330, 11] #log Personal Income
x53 <- alldata[alldata$WorkingCity == 5330, 6] #Transportation--Bus
x63 <- alldata[alldata$WorkingCity == 5330, 7] #Transportation--Subway
x73 <- alldata[alldata$WorkingCity == 5330, 8] #Transportation--Railroad
x83 <- alldata[alldata$WorkingCity == 5330, 9] #Transportation¡ªTaxi

x14 <- alldata[alldata$WorkingCity == 6290, 1] #Children number
x24 <- alldata[alldata$WorkingCity == 6290, 12] #age
x34 <- alldata[alldata$WorkingCity == 6290, 3] #Sex
x44 <- alldata[alldata$WorkingCity == 6290, 11] #log Personal Income
x54 <- alldata[alldata$WorkingCity == 6290, 6] #Transportation--Bus
x64 <- alldata[alldata$WorkingCity == 6290, 7] #Transportation--Subway
x74 <- alldata[alldata$WorkingCity == 6290, 8] #Transportation--Railroad
x84 <- alldata[alldata$WorkingCity == 6290, 9] #Transportation--Taxi

X[[1]] <- cbind(rep(1, N[1]),  x11, x21, x31, x41, x51, x61, x81)
X[[2]] <- cbind(rep(1, N[2]),  x12, x22, x32, x42, x52, x62, x82)
X[[3]] <- cbind(rep(1, N[3]),  x13, x23, x33, x43, x53, x63, x83)
X[[4]] <- cbind(rep(1, N[4]),  x14, x24, x34, x44, x54, x64, x84)

group1=subset(alldata, alldata$WorkingCity==1190)
group2=subset(alldata, alldata$WorkingCity==4610)
group3=subset(alldata, alldata$WorkingCity==5330)
group4=subset(alldata, alldata$WorkingCity==6290)

reg1 <- lm(log(Y[[1]])~-1+X[[1]])
reg2 <- lm(log(Y[[2]])~-1+X[[2]])
reg3 <- lm(log(Y[[3]])~-1+X[[3]])
reg4 <- lm(log(Y[[4]])~-1+X[[4]])
group1=subset(alldata, alldata$WorkingCity==1190)
group2=subset(alldata, alldata$WorkingCity==4610)
group3=subset(alldata, alldata$WorkingCity==5330)
group4=subset(alldata, alldata$WorkingCity==6290)


m=4

BETA.prior = matrix(NA, nrow=4, ncol=8)
BETA.prior[,1] = 4.2
BETA.prior[,2] = 0.06
BETA.prior[,3] = 0.004
BETA.prior[,4] = -0.012
BETA.prior[,5] = -0.069
BETA.prior[,6] = -0.21
BETA.prior[,7] = -0.07
BETA.prior[,8] = -1.0

mu0 = c(4.2, 0.06, 0.004, -0.012, -0.069, -0.21, -0.07, -1.0)

S0 = diag(8)
S0[3,3]=0.1

s2=1/(nrow(alldata)-1)*(sum((reg1$resid)^2)+sum((reg2$resid)^2)+sum((reg3$resid)^2)+sum((reg4$resid)^2))
eta0 = 4
nu0 = 2
sigma20 = s2
iL0 = iSigma = solve(S0)


### Prepare MCMC ###

S=20000
n = c(nrow(group1),nrow(group2),nrow(group3),nrow(group4))
a=2
b = 2
THETA.post = NULL
SIGMA.post = array(NA, dim = c(8,8,S))
sigma20.post = matrix(NA, nrow=S+1, ncol=1)
sigma2.post = matrix(NA, nrow=S, ncol=m)
BETA.post = array(NA, dim = c(m,8,S+1))
X = matrix(NA, nrow=2923, ncol=8)
X[,1] = 1
X[,2] = alldata$Children
X[,3] = age
X[,4] = Sex
X[,5] = aa
X[,6] = Tran1
X[,7] = Tran2
X[,8] = Tran4

XX1 = t( X[1:n[1],]) %*% ( X[1:n[1],])
XY1 = t( X[1:n[1],]) %*% as.matrix(log(Time[1:n[1]]))
XX2 = t( X[(n[1]+1):(n[1]+n[2]),]) %*% ( X[(n[1]+1):(n[1]+n[2]),])
XY2 = t( X[(n[1]+1):(n[1]+n[2]),]) %*% as.matrix(log(Time[(n[1]+1):(n[1]+n[2])]))
XX3 = t( X[(n[1]+n[2]+1):(n[1]+n[2]+n[3]),]) %*% ( X[(n[1]+n[2]+1):(n[1]+n[2]+n[3]),])
XY3 = t(X[(n[1]+n[2]+1):(n[1]+n[2]+n[3]),])%*%as.matrix(log(Time[(n[1]+n[2]+1):
  (n[1] +n[2]+n[3])]))
XX4 = t( X[(n[1]+n[2]+n[3]+1):2923,]) %*% ( X[(n[1]+n[2]+n[3]+1):2923,])
XY4 = t(X[(n[1]+n[2]+n[3]+1):2923,])%*%as.matrix(log(Time[(n[1]+n[2]+n[3]+1):
  2923]))
SSR1 = sum((reg1$resid)^2)
SSR2 = sum((reg2$resid)^2)
SSR3 = sum((reg3$resid)^2)
SSR4 = sum((reg4$resid)^2)
sigma20.post[1] = s2
BETA.post[,,1] =BETA.prior

### start MCMC ###

for (s in 1:S){

  Lm = solve(iL0+m*iSigma)
  mum = Lm%*%(iL0%*%mu0 + iSigma%*%apply(BETA.post[,,s],2,sum))
  theta = t(rmvnorm(1,mum,Lm))

  mtheta = matrix(theta, m,8,byrow = TRUE)
  iSigma = rwish(8+m,
                 solve(S0+t(BETA.post[,,s]-mtheta)%*%(BETA.post[,,s]-mtheta))  )

  sigma2.post[s,1] = 1/rgamma(1, (nu0+n[1])/2, (nu0*sigma20.post[s]+SSR1)/2)
  sigma2.post[s,2] = 1/rgamma(1, (nu0+n[2])/2, (nu0*sigma20.post[s]+SSR2)/2)
  sigma2.post[s,3] = 1/rgamma(1, (nu0+n[3])/2, (nu0*sigma20.post[s]+SSR3)/2)
  sigma2.post[s,4] = 1/rgamma(1, (nu0+n[4])/2, (nu0*sigma20.post[s]+SSR4)/2)
  sigma20.post[s+1] = rgamma(1, (m*nu0/2+a), (nu0/2*sum(1/sigma2.post[s,])+b))

  beta.posterior = matrix(NA, nrow = m, ncol = 8)
  beta1.variance= solve(iSigma + XX1/sigma2.post[s,1])
  beta1.mean = beta1.variance %*% (iSigma %*% theta + XY1/sigma2.post[s,1])
  beta.posterior[1,] = rmvnorm(1,beta1.mean, beta1.variance)
  beta2.variance= solve(iSigma + XX2/sigma2.post[s,2])
  beta2.mean = beta2.variance %*% (iSigma %*% theta + XY2/sigma2.post[s,2])
  beta.posterior[2,] = rmvnorm(1,beta2.mean, beta2.variance)
  beta3.variance= solve(iSigma + XX3/sigma2.post[s,3])
  beta3.mean = beta3.variance %*% (iSigma %*% theta + XY3/sigma2.post[s,3])
  beta.posterior[3,] = rmvnorm(1,beta3.mean, beta3.variance)
  beta4.variance= solve(iSigma + XX4/sigma2.post[s,4])
  beta4.mean = beta4.variance %*% (iSigma %*% theta + XY4/sigma2.post[s,4])
  beta.posterior[4,] = rmvnorm(1,beta4.mean, beta4.variance)

  SSR1 = sum((log(Time[1:n[1]])-X[1:n[1],] * beta.posterior[1,])^2)
  SSR2 = sum((log(Time[(n[1]+1):(n[1]+n[2])])-X[(n[1]+1):(n[1]+n[2]),] *
    beta.posterior[2,])^2)
  SSR3 = sum((log(Time[(n[1]+n[2]+1):(n[1]+n[2]+n[3])])-X[(n[1]+n[2]+1):
    (n[1]+n[2]+n[3]),] * beta.posterior[3,])^2)
  SSR4 = sum((log(Time[(n[1]+n[2]+n[3]+1):2923])-X[(n[1]+n[2]+n[3]+1):2923,]*
    beta.posterior[4,])^2)

  THETA.post = rbind(THETA.post, t(theta))
  SIGMA.post [,,s]= solve(iSigma)
  BETA.post[,,s+1] = beta.posterior
  
}


########################
### Plots and Checks ###
########################

# Density of Posterior BETA for Group 1 - Chicago#

par(mfrow = c(3,3))

plot(density(BETA.post[1,1,]), xlab = expression(beta[11]), xlim = c(0, 10), ylim = c(0,0.4), main =
  expression(paste("Density plot of ", beta[11])))
x1 = seq(-5, 15, length =1000)
lines(x=x1, y=dnorm(x1, BETA.prior[,1],sqrt(S0[1,1])), col ="gray")

plot(density(BETA.post[1,2,]), xlab = expression(beta[12]), xlim = c(-6, 6), ylim = c(0,0.4), main =
  expression(paste("Density plot of ", beta[12])))
x2 = seq(-6, 6, length =1000)
lines(x=x2, y=dnorm(x2, BETA.prior[,2],sqrt(S0[2,2])), col ="gray")

plot(density(BETA.post[1,3,]), xlab = expression(beta[13]), xlim = c(-1, 1), main =
  expression(paste("Density plot of ", beta[13])))
x3 = seq(-1, 1, length =1000)
lines(x=x3, y=dnorm(x3, BETA.prior[,3],sqrt(S0[3,3])), col ="gray")

plot(density(BETA.post[1,4,]), xlab = expression(beta[14]), xlim = c(-6, 6), ylim = c(0,0.4),main =
  expression(paste("Density plot of ", beta[14])))
x4 = seq(-6, 6, length =1000)
lines(x=x4, y=dnorm(x4, BETA.prior[,4],sqrt(S0[4,4])), col ="gray")

plot(density(BETA.post[1,5,]), xlab = expression(beta[15]), xlim = c(-3, 3), main =
  expression(paste("Density plot of ", beta[15])))
x5 = seq(-3, 3, length =1000)
lines(x=x5, y=dnorm(x5, BETA.prior[,5],sqrt(S0[5,5])), col ="gray")

plot(density(BETA.post[1,6,]), xlab = expression(beta[16]), xlim = c(-6, 6), ylim = c(0, 0.4), main =
  expression(paste("Density plot of ", beta[16])))
x6 = seq(-6, 6, length =1000)
lines(x=x6, y=dnorm(x6, BETA.prior[,6],sqrt(S0[6,6])), col ="gray")

plot(density(BETA.post[1,7,]), xlab = expression(beta[17]), xlim = c(-5, 5), ylim = c(0, 0.4), main =
  expression(paste("Density plot of ", beta[17])))
x7 = seq(-5, 5, length =1000)
lines(x=x7, y=dnorm(x7, BETA.prior[,7],sqrt(S0[7,7])), col ="gray")

plot(density(BETA.post[1,8,]), xlab = expression(beta[18]), xlim = c(-7, 5), ylim = c(0, 0.4), main =
  expression(paste("Density plot of ", beta[18])))
x8 = seq(-5, 5, length =1000)
lines(x=x8, y=dnorm(x8, BETA.prior[,8],sqrt(S0[8,8])), col ="gray")

# Density of Posterior BETA for Group 2 - NYC #

par(mfrow = c(3,3))

plot(density(BETA.post[2,1,]), xlab = expression(beta[21]), xlim = c(0,10), ylim = c(0, 0.4), main =
  expression(paste("Density plot of ", beta[21])))
x1 = seq(-5, 15, length =1000)
lines(x=x1, y=dnorm(x1, BETA.prior[,1],sqrt(S0[1,1])), col ="gray")

plot(density(BETA.post[2,2,]), xlab = expression(beta[22]), main =
  expression(paste("Density plot of ", beta[22])))
x2 = seq(-6, 6, length =1000)
lines(x=x2, y=dnorm(x2, BETA.prior[,2],sqrt(S0[2,2])), col ="gray")

plot(density(BETA.post[2,3,]), xlab = expression(beta[23]), main =
  expression(paste("Density plot of ", beta[23])))
x3 = seq(-1, 1, length =1000)
lines(x=x3, y=dnorm(x3, BETA.prior[,3],sqrt(S0[3,3])), col ="gray")

plot(density(BETA.post[2,4,]), xlab = expression(beta[24]), main =
  expression(paste("Density plot of ", beta[24])))
x4 = seq(-6, 6, length =1000)
lines(x=x4, y=dnorm(x4, BETA.prior[,4],sqrt(S0[4,4])), col ="gray")

plot(density(BETA.post[2,5,]), xlab = expression(beta[25]), main =
  expression(paste("Density plot of ", beta[25])))
x5 = seq(-3, 3, length =1000)
lines(x=x5, y=dnorm(x5, BETA.prior[,5],sqrt(S0[5,5])), col ="gray")

plot(density(BETA.post[2,6,]), xlab = expression(beta[26]), main =
  expression(paste("Density plot of ", beta[26])))
x6 = seq(-6, 6, length =1000)
lines(x=x6, y=dnorm(x6, BETA.prior[,6],sqrt(S0[6,6])), col ="gray")

plot(density(BETA.post[2,7,]), xlab = expression(beta[27]), main =
  expression(paste("Density plot of ", beta[27])))
x7 = seq(-5, 5, length =1000)
lines(x=x7, y=dnorm(x7, BETA.prior[,7],sqrt(S0[7,7])), col ="gray")

plot(density(BETA.post[2,8,]), xlab = expression(beta[28]), xlim = c(-7, 5), ylim = c(0, 0.4), main =
  expression(paste("Density plot of ", beta[28])))
x8 = seq(-5, 5, length =1000)
lines(x=x8, y=dnorm(x8, BETA.prior[,8],sqrt(S0[8,8])), col ="gray")

# Density of Posterior BETA for Group 3 - Philadelphia #

par(mfrow = c(3,3))

plot(density(BETA.post[3,1,]), xlab = expression(beta[31]), xlim = c(0,10), ylim = c(0, 0.4), main =
  expression(paste("Density plot of ", beta[31])))
x1 = seq(-5, 15, length =1000)
lines(x=x1, y=dnorm(x1, BETA.prior[,1],sqrt(S0[1,1])), col ="gray")

plot(density(BETA.post[3,2,]), xlab = expression(beta[32]), main =
  expression(paste("Density plot of ", beta[32])))
x2 = seq(-6, 6, length =1000)
lines(x=x2, y=dnorm(x2, BETA.prior[,2],sqrt(S0[2,2])), col ="gray")

plot(density(BETA.post[3,3,]), xlab = expression(beta[33]), main =
  expression(paste("Density plot of ", beta[33])))
x3 = seq(-1, 1, length =1000)
lines(x=x3, y=dnorm(x3, BETA.prior[,3],sqrt(S0[3,3])), col ="gray")

plot(density(BETA.post[3,4,]), xlab = expression(beta[34]), main =
  expression(paste("Density plot of ", beta[34])))
x4 = seq(-6, 6, length =1000)
lines(x=x4, y=dnorm(x4, BETA.prior[,4],sqrt(S0[4,4])), col ="gray")

plot(density(BETA.post[3,5,]), xlab = expression(beta[35]), main =
  expression(paste("Density plot of ", beta[35])))
x5 = seq(-3, 3, length =1000)
lines(x=x5, y=dnorm(x5, BETA.prior[,5],sqrt(S0[5,5])), col ="gray")

plot(density(BETA.post[3,6,]), xlab = expression(beta[36]), main =
  expression(paste("Density plot of ", beta[36])))
x6 = seq(-6, 6, length =1000)
lines(x=x6, y=dnorm(x6, BETA.prior[,6],sqrt(S0[6,6])), col ="gray")

plot(density(BETA.post[3,7,]), xlab = expression(beta[37]), main =
  expression(paste("Density plot of ", beta[37])))
x7 = seq(-5, 5, length =1000)
lines(x=x7, y=dnorm(x7, BETA.prior[,7],sqrt(S0[7,7])), col ="gray")

plot(density(BETA.post[3,8,]), xlab = expression(beta[38]), xlim = c(-7, 5), ylim = c(0, 0.4), main =
  expression(paste("Density plot of ", beta[38])))
x8 = seq(-5, 5, length =1000)
lines(x=x8, y=dnorm(x8, BETA.prior[,8],sqrt(S0[8,8])), col ="gray")

# Density of Posterior BETA for Group 4 - San Francisco #

par(mfrow = c(3,3))

plot(density(BETA.post[4,1,]), xlab = expression(beta[41]), xlim = c(0,10), ylim = c(0, 0.4), main =
  expression(paste("Density plot of ", beta[41])))
x1 = seq(-5, 15, length =1000)
lines(x=x1, y=dnorm(x1, BETA.prior[,1],sqrt(S0[1,1])), col ="gray")

plot(density(BETA.post[4,2,]), xlab = expression(beta[42]), main =
  expression(paste("Density plot of ", beta[42])))
x2 = seq(-6, 6, length =1000)
lines(x=x2, y=dnorm(x2, BETA.prior[,2],sqrt(S0[2,2])), col ="gray")

plot(density(BETA.post[4,3,]), xlab = expression(beta[43]), main =
  expression(paste("Density plot of ", beta[43])))
x3 = seq(-1, 1, length =1000)
lines(x=x3, y=dnorm(x3, BETA.prior[,3],sqrt(S0[3,3])), col ="gray")

plot(density(BETA.post[4,4,]), xlab = expression(beta[44]), main =
  expression(paste("Density plot of ", beta[44])))
x4 = seq(-6, 6, length =1000)
lines(x=x4, y=dnorm(x4, BETA.prior[,4],sqrt(S0[4,4])), col ="gray")

plot(density(BETA.post[4,5,]), xlab = expression(beta[45]), main =
  expression(paste("Density plot of ", beta[45])))
x5 = seq(-3, 3, length =1000)
lines(x=x5, y=dnorm(x5, BETA.prior[,5],sqrt(S0[5,5])), col ="gray")

plot(density(BETA.post[4,6,]), xlab = expression(beta[46]), main =
  expression(paste("Density plot of ", beta[46])))
x6 = seq(-6, 6, length =1000)
lines(x=x6, y=dnorm(x6, BETA.prior[,6],sqrt(S0[6,6])), col ="gray")

plot(density(BETA.post[4,7,]), xlab = expression(beta[47]), main =
  expression(paste("Density plot of ", beta[47])))
x7 = seq(-5, 5, length =1000)
lines(x=x7, y=dnorm(x7, BETA.prior[,7],sqrt(S0[7,7])), col ="gray")

plot(density(BETA.post[4,8,]), xlab = expression(beta[48]), xlim = c(-7, 5), ylim = c(0, 0.4), main =
  expression(paste("Density plot of ", beta[48])))
x8 = seq(-5, 5, length =1000)
lines(x=x8, y=dnorm(x8, BETA.prior[,8],sqrt(S0[8,8])), col ="gray")

### Check Convergence ###

# Group 1 - Chi. #

par(mfrow = c(3,3))
traceplot(as.mcmc(BETA.post[1,1,10001:20000]),
          ylab = expression(beta[11]))
traceplot(as.mcmc(BETA.post[1,2,10001:20000]),
          ylab = expression(beta[12]))
traceplot(as.mcmc(BETA.post[1,3,10001:20000]),
          ylab = expression(beta[13]))
traceplot(as.mcmc(BETA.post[1,4,10001:20000]),
          ylab = expression(beta[14]))
traceplot(as.mcmc(BETA.post[1,5,10001:20000]),
          ylab = expression(beta[15]))
traceplot(as.mcmc(BETA.post[1,6,10001:20000]),
          ylab = expression(beta[16]))
traceplot(as.mcmc(BETA.post[1,7,10001:20000]),
          ylab = expression(beta[17]))
traceplot(as.mcmc(BETA.post[1,8,10001:20000]),
          ylab = expression(beta[18]))

# Group 2 - NYC #

par(mfrow = c(3,3))
traceplot(as.mcmc(BETA.post[2,1,10001:20000]),
          ylab = expression(beta[21]))
traceplot(as.mcmc(BETA.post[2,2,10001:20000]),
          ylab = expression(beta[22]))
traceplot(as.mcmc(BETA.post[2,3,10001:20000]),
          ylab = expression(beta[23]))
traceplot(as.mcmc(BETA.post[2,4,10001:20000]),
          ylab = expression(beta[24]))
traceplot(as.mcmc(BETA.post[2,5,10001:20000]),
          ylab = expression(beta[25]))
traceplot(as.mcmc(BETA.post[2,6,10001:20000]),
          ylab = expression(beta[26]))
traceplot(as.mcmc(BETA.post[2,7,10001:20000]),
          ylab = expression(beta[27]))
traceplot(as.mcmc(BETA.post[2,8,10001:20000]),
          ylab = expression(beta[28]))

# Group 3 - Phi. #

par(mfrow = c(3,3))
traceplot(as.mcmc(BETA.post[3,1,10001:20000]),
          ylab = expression(beta[31]))
traceplot(as.mcmc(BETA.post[3,2,10001:20000]),
          ylab = expression(beta[32]))
traceplot(as.mcmc(BETA.post[3,3,10001:20000]),
          ylab = expression(beta[33]))
traceplot(as.mcmc(BETA.post[3,4,10001:20000]),
          ylab = expression(beta[34]))
traceplot(as.mcmc(BETA.post[3,5,10001:20000]),
          ylab = expression(beta[35]))
traceplot(as.mcmc(BETA.post[3,6,10001:20000]),
          ylab = expression(beta[36]))
traceplot(as.mcmc(BETA.post[3,7,10001:20000]),
          ylab = expression(beta[37]))
traceplot(as.mcmc(BETA.post[3,8,10001:20000]),
          ylab = expression(beta[38]))

# Group 4 - SF. #

par(mfrow = c(3,3))
traceplot(as.mcmc(BETA.post[4,1,10001:20000]),
          ylab = expression(beta[41]))
traceplot(as.mcmc(BETA.post[4,2,10001:20000]),
          ylab = expression(beta[42]))
traceplot(as.mcmc(BETA.post[4,3,10001:20000]),
          ylab = expression(beta[43]))
traceplot(as.mcmc(BETA.post[4,4,10001:20000]),
          ylab = expression(beta[44]))
traceplot(as.mcmc(BETA.post[4,5,10001:20000]),
          ylab = expression(beta[45]))
traceplot(as.mcmc(BETA.post[4,6,10001:20000]),
          ylab = expression(beta[46]))
traceplot(as.mcmc(BETA.post[4,7,10001:20000]),
          ylab = expression(beta[47]))
traceplot(as.mcmc(BETA.post[4,8,10001:20000]),
          ylab = expression(beta[48]))

### Check AutoCorrelation ###

autocorr(as.mcmc(BETA.post[1,1,10000:20000]))
autocorr(as.mcmc(BETA.post[1,2,10000:20000]))
autocorr(as.mcmc(BETA.post[1,3,10000:20000]))
autocorr(as.mcmc(BETA.post[1,4,10000:20000]))
autocorr(as.mcmc(BETA.post[1,5,10000:20000]))
autocorr(as.mcmc(BETA.post[1,6,10000:20000]))
autocorr(as.mcmc(BETA.post[1,7,10000:20000]))
autocorr(as.mcmc(BETA.post[1,8,10000:20000]))

autocorr(as.mcmc(BETA.post[2,1,10000:20000]))
autocorr(as.mcmc(BETA.post[2,2,10000:20000]))
autocorr(as.mcmc(BETA.post[2,3,10000:20000]))
autocorr(as.mcmc(BETA.post[2,4,10000:20000]))
autocorr(as.mcmc(BETA.post[2,5,10000:20000]))
autocorr(as.mcmc(BETA.post[2,6,10000:20000]))
autocorr(as.mcmc(BETA.post[2,7,10000:20000]))
autocorr(as.mcmc(BETA.post[2,8,10000:20000]))

autocorr(as.mcmc(BETA.post[3,1,10000:20000]))
autocorr(as.mcmc(BETA.post[3,2,10000:20000]))
autocorr(as.mcmc(BETA.post[3,3,10000:20000]))
autocorr(as.mcmc(BETA.post[3,4,10000:20000]))
autocorr(as.mcmc(BETA.post[3,5,10000:20000]))
autocorr(as.mcmc(BETA.post[3,6,10000:20000]))
autocorr(as.mcmc(BETA.post[3,7,10000:20000]))
autocorr(as.mcmc(BETA.post[3,8,10000:20000]))

autocorr(as.mcmc(BETA.post[4,1,10000:20000]))
autocorr(as.mcmc(BETA.post[4,2,10000:20000]))
autocorr(as.mcmc(BETA.post[4,3,10000:20000]))
autocorr(as.mcmc(BETA.post[4,4,10000:20000]))
autocorr(as.mcmc(BETA.post[4,5,10000:20000]))
autocorr(as.mcmc(BETA.post[4,6,10000:20000]))
autocorr(as.mcmc(BETA.post[4,7,10000:20000]))
autocorr(as.mcmc(BETA.post[4,8,10000:20000]))

###plot acf

par(mfrow = c(3,3))
acf(as.mcmc(BETA.post[1,1,10000:20000]), main = expression(beta[11]))
acf(as.mcmc(BETA.post[1,2,10000:20000]), main = expression(beta[12]))
acf(as.mcmc(BETA.post[1,3,10000:20000]), main = expression(beta[13]))
acf(as.mcmc(BETA.post[1,4,10000:20000]), main = expression(beta[14]))
acf(as.mcmc(BETA.post[1,5,10000:20000]), main = expression(beta[15]))
acf(as.mcmc(BETA.post[1,6,10000:20000]), main = expression(beta[16]))
acf(as.mcmc(BETA.post[1,7,10000:20000]), main = expression(beta[17]))
acf(as.mcmc(BETA.post[1,8,10000:20000]), main = expression(beta[18]))

par(mfrow = c(3,3))
acf(as.mcmc(BETA.post[2,1,10000:20000]), main = expression(beta[21]))
acf(as.mcmc(BETA.post[2,2,10000:20000]), main = expression(beta[22]))
acf(as.mcmc(BETA.post[2,3,10000:20000]), main = expression(beta[23]))
acf(as.mcmc(BETA.post[2,4,10000:20000]), main = expression(beta[24]))
acf(as.mcmc(BETA.post[2,5,10000:20000]), main = expression(beta[25]))
acf(as.mcmc(BETA.post[2,6,10000:20000]), main = expression(beta[26]))
acf(as.mcmc(BETA.post[2,7,10000:20000]), main = expression(beta[27]))
acf(as.mcmc(BETA.post[2,8,10000:20000]), main = expression(beta[28]))

par(mfrow = c(3,3))
acf(as.mcmc(BETA.post[3,1,10000:20000]), main = expression(beta[31]))
acf(as.mcmc(BETA.post[3,2,10000:20000]), main = expression(beta[32]))
acf(as.mcmc(BETA.post[3,3,10000:20000]), main = expression(beta[33]))
acf(as.mcmc(BETA.post[3,4,10000:20000]), main = expression(beta[34]))
acf(as.mcmc(BETA.post[3,5,10000:20000]), main = expression(beta[35]))
acf(as.mcmc(BETA.post[3,6,10000:20000]), main = expression(beta[36]))
acf(as.mcmc(BETA.post[3,7,10000:20000]), main = expression(beta[37]))
acf(as.mcmc(BETA.post[3,8,10000:20000]), main = expression(beta[38]))

par(mfrow = c(3,3))
acf(as.mcmc(BETA.post[4,1,10000:20000]), main = expression(beta[41]))
acf(as.mcmc(BETA.post[4,2,10000:20000]), main = expression(beta[42]))
acf(as.mcmc(BETA.post[4,3,10000:20000]), main = expression(beta[43]))
acf(as.mcmc(BETA.post[4,4,10000:20000]), main = expression(beta[44]))
acf(as.mcmc(BETA.post[4,5,10000:20000]), main = expression(beta[45]))
acf(as.mcmc(BETA.post[4,6,10000:20000]), main = expression(beta[46]))
acf(as.mcmc(BETA.post[4,7,10000:20000]), main = expression(beta[47]))
acf(as.mcmc(BETA.post[4,8,10000:20000]), main = expression(beta[48]))


### Gelman-Rubin Diagnostic ###
### Start from different value
###BETA.post2 = array(NA, dim = c(m,8,S+1))
###BETA.post2[,,1] = 1

Y <- list()
X <- list()
N <- NULL
Y[[1]] <- alldata[alldata$WorkingCity == 1190, 10]
Y[[2]] <- alldata[alldata$WorkingCity == 4610, 10]
Y[[3]] <- alldata[alldata$WorkingCity == 5330, 10]
Y[[4]] <- alldata[alldata$WorkingCity == 6290, 10]
N[1] <- sum(alldata$WorkingCity == 1190)
N[2] <- sum(alldata$WorkingCity == 4610) 
N[3] <- sum(alldata$WorkingCity == 5330)
N[4] <- sum(alldata$WorkingCity == 6290)
x11 <- alldata[alldata$WorkingCity == 1190, 1] #Children number
x21 <- alldata[alldata$WorkingCity == 1190, 12] #age
x31 <- alldata[alldata$WorkingCity == 1190, 3] #Sex
x41 <- alldata[alldata$WorkingCity == 1190, 11] #log Personal Income
x51 <- alldata[alldata$WorkingCity == 1190, 6] #Transportation--Bus
x61 <- alldata[alldata$WorkingCity == 1190, 7] #Transportation--Subway
x71 <- alldata[alldata$WorkingCity == 1190, 8] #Transportation--Railroad
x81 <- alldata[alldata$WorkingCity == 1190, 9] #Transportation--Taxi

x12 <- alldata[alldata$WorkingCity == 4610, 1] #Children number
x22 <- alldata[alldata$WorkingCity == 4610, 12] #age
x32 <- alldata[alldata$WorkingCity == 4610, 3] #Sex
x42 <- alldata[alldata$WorkingCity == 4610, 11] #log Personal Income
x52 <- alldata[alldata$WorkingCity == 4610, 6] #Transportation--Bus
x62 <- alldata[alldata$WorkingCity == 4610, 7] #Transportation--Subway
x72 <- alldata[alldata$WorkingCity == 4610, 8] #Transportation--Railroad
x82 <- alldata[alldata$WorkingCity == 4610, 9] #Transportation¡ªTaxi

x13 <- alldata[alldata$WorkingCity == 5330, 1] #Children number
x23 <- alldata[alldata$WorkingCity == 5330, 12] #age
x33 <- alldata[alldata$WorkingCity == 5330, 3] #Sex
x43 <- alldata[alldata$WorkingCity == 5330, 11] #log Personal Income
x53 <- alldata[alldata$WorkingCity == 5330, 6] #Transportation--Bus
x63 <- alldata[alldata$WorkingCity == 5330, 7] #Transportation--Subway
x73 <- alldata[alldata$WorkingCity == 5330, 8] #Transportation--Railroad
x83 <- alldata[alldata$WorkingCity == 5330, 9] #Transportation¡ªTaxi

x14 <- alldata[alldata$WorkingCity == 6290, 1] #Children number
x24 <- alldata[alldata$WorkingCity == 6290, 12] #age
x34 <- alldata[alldata$WorkingCity == 6290, 3] #Sex
x44 <- alldata[alldata$WorkingCity == 6290, 11] #log Personal Income
x54 <- alldata[alldata$WorkingCity == 6290, 6] #Transportation--Bus
x64 <- alldata[alldata$WorkingCity == 6290, 7] #Transportation--Subway
x74 <- alldata[alldata$WorkingCity == 6290, 8] #Transportation--Railroad
x84 <- alldata[alldata$WorkingCity == 6290, 9] #Transportation--Taxi

X[[1]] <- cbind(rep(1, N[1]),  x11, x21, x31, x41, x51, x61, x81)
X[[2]] <- cbind(rep(1, N[2]),  x12, x22, x32, x42, x52, x62, x82)
X[[3]] <- cbind(rep(1, N[3]),  x13, x23, x33, x43, x53, x63, x83)
X[[4]] <- cbind(rep(1, N[4]),  x14, x24, x34, x44, x54, x64, x84)

group1=subset(alldata, alldata$WorkingCity==1190)
group2=subset(alldata, alldata$WorkingCity==4610)
group3=subset(alldata, alldata$WorkingCity==5330)
group4=subset(alldata, alldata$WorkingCity==6290)

reg1 <- lm(log(Y[[1]])~-1+X[[1]])
reg2 <- lm(log(Y[[2]])~-1+X[[2]])
reg3 <- lm(log(Y[[3]])~-1+X[[3]])
reg4 <- lm(log(Y[[4]])~-1+X[[4]])
group1=subset(alldata, alldata$WorkingCity==1190)
group2=subset(alldata, alldata$WorkingCity==4610)
group3=subset(alldata, alldata$WorkingCity==5330)
group4=subset(alldata, alldata$WorkingCity==6290)
m=4

BETA.prior = matrix(NA, nrow=4, ncol=8)
BETA.prior[,1] = 4.2
BETA.prior[,2] = 0.06
BETA.prior[,3] = 0.004
BETA.prior[,4] = -0.012
BETA.prior[,5] = -0.069
BETA.prior[,6] = -0.21
BETA.prior[,7] = -0.07
BETA.prior[,8] = -1.0

mu0 = c(4.2, 0.06, 0.004, -0.012, -0.069, -0.21, -0.07, -1.0)

S0 = diag(8)
S0[3,3]=0.1

s2=1/(nrow(alldata)-1)*(sum((reg1$resid)^2)+sum((reg2$resid)^2)+sum((reg3$resid)^2)+sum((reg4$resid)^2))
eta0 = 4
nu0 = 2
sigma20 = s2
iL0 = iSigma = solve(S0)

S=20000
n = c(nrow(group1),nrow(group2),nrow(group3),nrow(group4))
a=2
b = 2
THETA.post = NULL
SIGMA.post = array(NA, dim = c(8,8,S))
sigma20.post = matrix(NA, nrow=S+1, ncol=1)
sigma2.post = matrix(NA, nrow=S, ncol=m)
BETA.post2 = array(NA, dim = c(m,8,S+1))
BETA.post2[,,1] = 1
X = matrix(NA, nrow=2923, ncol=8)
X[,1] = 1
X[,2] = alldata$Children
X[,3] = age
X[,4] = Sex
X[,5] = aa
X[,6] = Tran1
X[,7] = Tran2
X[,8] = Tran4

XX1 = t( X[1:n[1],]) %*% ( X[1:n[1],])
XY1 = t( X[1:n[1],]) %*% as.matrix(log(Time[1:n[1]]))
XX2 = t( X[(n[1]+1):(n[1]+n[2]),]) %*% ( X[(n[1]+1):(n[1]+n[2]),])
XY2 = t( X[(n[1]+1):(n[1]+n[2]),]) %*% as.matrix(log(Time[(n[1]+1):(n[1]+n[2])]))
XX3 = t( X[(n[1]+n[2]+1):(n[1]+n[2]+n[3]),]) %*% ( X[(n[1]+n[2]+1):(n[1]+n[2]+n[3]),])
XY3 = t(X[(n[1]+n[2]+1):(n[1]+n[2]+n[3]),])%*%as.matrix(log(Time[(n[1]+n[2]+1):
(n[1] +n[2]+n[3])]))
XX4 = t( X[(n[1]+n[2]+n[3]+1):2923,]) %*% ( X[(n[1]+n[2]+n[3]+1):2923,])
XY4 = t(X[(n[1]+n[2]+n[3]+1):2923,])%*%as.matrix(log(Time[(n[1]+n[2]+n[3]+1):
2923]))
SSR1 = sum((reg1$resid)^2)
SSR2 = sum((reg2$resid)^2)
SSR3 = sum((reg3$resid)^2)
SSR4 = sum((reg4$resid)^2)
sigma20.post[1] = s2
BETA.post2[,,1] =BETA.prior

for (s in 1:S){
	
	Lm = solve(iL0+m*iSigma)
	mum = Lm%*%(iL0%*%mu0 + iSigma%*%apply(BETA.post2[,,s],2,sum))
	theta = t(rmvnorm(1,mum,Lm))
	
	mtheta = matrix(theta, m,8,byrow = TRUE)
	iSigma = rwish(8+m,
				   solve(S0+t(BETA.post2[,,s]-mtheta)%*%(BETA.post2[,,s]-mtheta))  )
	
	sigma2.post[s,1] = 1/rgamma(1, (nu0+n[1])/2, (nu0*sigma20.post[s]+SSR1)/2)
	sigma2.post[s,2] = 1/rgamma(1, (nu0+n[2])/2, (nu0*sigma20.post[s]+SSR2)/2)
	sigma2.post[s,3] = 1/rgamma(1, (nu0+n[3])/2, (nu0*sigma20.post[s]+SSR3)/2)
	sigma2.post[s,4] = 1/rgamma(1, (nu0+n[4])/2, (nu0*sigma20.post[s]+SSR4)/2)
	sigma20.post[s+1] = rgamma(1, (m*nu0/2+a), (nu0/2*sum(1/sigma2.post[s,])+b))
	
	beta.posterior = matrix(NA, nrow = m, ncol = 8)
	beta1.variance= solve(iSigma + XX1/sigma2.post[s,1])
	beta1.mean = beta1.variance %*% (iSigma %*% theta + XY1/sigma2.post[s,1])
	beta.posterior[1,] = rmvnorm(1,beta1.mean, beta1.variance)
	beta2.variance= solve(iSigma + XX2/sigma2.post[s,2])
	beta2.mean = beta2.variance %*% (iSigma %*% theta + XY2/sigma2.post[s,2])
	beta.posterior[2,] = rmvnorm(1,beta2.mean, beta2.variance)
	beta3.variance= solve(iSigma + XX3/sigma2.post[s,3])
	beta3.mean = beta3.variance %*% (iSigma %*% theta + XY3/sigma2.post[s,3])
	beta.posterior[3,] = rmvnorm(1,beta3.mean, beta3.variance)
	beta4.variance= solve(iSigma + XX4/sigma2.post[s,4])
	beta4.mean = beta4.variance %*% (iSigma %*% theta + XY4/sigma2.post[s,4])
	beta.posterior[4,] = rmvnorm(1,beta4.mean, beta4.variance)
	
	SSR1 = sum((log(Time[1:n[1]])-X[1:n[1],] * beta.posterior[1,])^2)
	SSR2 = sum((log(Time[(n[1]+1):(n[1]+n[2])])-X[(n[1]+1):(n[1]+n[2]),] *
				beta.posterior[2,])^2)
	SSR3 = sum((log(Time[(n[1]+n[2]+1):(n[1]+n[2]+n[3])])-X[(n[1]+n[2]+1):
				(n[1]+n[2]+n[3]),] * beta.posterior[3,])^2)
	SSR4 = sum((log(Time[(n[1]+n[2]+n[3]+1):2923])-X[(n[1]+n[2]+n[3]+1):2923,]*
				beta.posterior[4,])^2)
	
	THETA.post = rbind(THETA.post, t(theta))
  SIGMA.post [,,s]= solve(iSigma)
  BETA.post2[,,s+1] = beta.posterior
  
}


GR.BETA11 = mcmc.list(as.mcmc(BETA.post[1,1,]), as.mcmc(BETA.post2[1,1,]))
GR.BETA12 = mcmc.list(as.mcmc(BETA.post[1,2,]), as.mcmc(BETA.post2[1,2,]))
GR.BETA13 = mcmc.list(as.mcmc(BETA.post[1,3,]), as.mcmc(BETA.post2[1,3,]))
GR.BETA14 = mcmc.list(as.mcmc(BETA.post[1,4,]), as.mcmc(BETA.post2[1,4,]))
GR.BETA15 = mcmc.list(as.mcmc(BETA.post[1,5,]), as.mcmc(BETA.post2[1,5,]))
GR.BETA16 = mcmc.list(as.mcmc(BETA.post[1,6,]), as.mcmc(BETA.post2[1,6,]))
GR.BETA17 = mcmc.list(as.mcmc(BETA.post[1,7,]), as.mcmc(BETA.post2[1,7,]))
GR.BETA18 = mcmc.list(as.mcmc(BETA.post[1,8,]), as.mcmc(BETA.post2[1,8,]))

gelman.diag(GR.BETA11)
gelman.diag(GR.BETA12)
gelman.diag(GR.BETA13)
gelman.diag(GR.BETA14)
gelman.diag(GR.BETA15)
gelman.diag(GR.BETA16)
gelman.diag(GR.BETA17)
gelman.diag(GR.BETA18)

GR.BETA21 = mcmc.list(as.mcmc(BETA.post[2,1,]), as.mcmc(BETA.post2[2,1,]))
GR.BETA22 = mcmc.list(as.mcmc(BETA.post[2,2,]), as.mcmc(BETA.post2[2,2,]))
GR.BETA23 = mcmc.list(as.mcmc(BETA.post[2,3,]), as.mcmc(BETA.post2[2,3,]))
GR.BETA24 = mcmc.list(as.mcmc(BETA.post[2,4,]), as.mcmc(BETA.post2[2,4,]))
GR.BETA25 = mcmc.list(as.mcmc(BETA.post[2,5,]), as.mcmc(BETA.post2[2,5,]))
GR.BETA26 = mcmc.list(as.mcmc(BETA.post[2,6,]), as.mcmc(BETA.post2[2,6,]))
GR.BETA27 = mcmc.list(as.mcmc(BETA.post[2,7,]), as.mcmc(BETA.post2[2,7,]))
GR.BETA28 = mcmc.list(as.mcmc(BETA.post[2,8,]), as.mcmc(BETA.post2[2,8,]))

gelman.diag(GR.BETA21)
gelman.diag(GR.BETA22)
gelman.diag(GR.BETA23)
gelman.diag(GR.BETA24)
gelman.diag(GR.BETA25)
gelman.diag(GR.BETA26)
gelman.diag(GR.BETA27)
gelman.diag(GR.BETA28)

GR.BETA31 = mcmc.list(as.mcmc(BETA.post[3,1,]), as.mcmc(BETA.post2[3,1,]))
GR.BETA32 = mcmc.list(as.mcmc(BETA.post[3,2,]), as.mcmc(BETA.post2[3,2,]))
GR.BETA33 = mcmc.list(as.mcmc(BETA.post[3,3,]), as.mcmc(BETA.post2[3,3,]))
GR.BETA34 = mcmc.list(as.mcmc(BETA.post[3,4,]), as.mcmc(BETA.post2[3,4,]))
GR.BETA35 = mcmc.list(as.mcmc(BETA.post[3,5,]), as.mcmc(BETA.post2[3,5,]))
GR.BETA36 = mcmc.list(as.mcmc(BETA.post[3,6,]), as.mcmc(BETA.post2[3,6,]))
GR.BETA37 = mcmc.list(as.mcmc(BETA.post[3,7,]), as.mcmc(BETA.post2[3,7,]))
GR.BETA38 = mcmc.list(as.mcmc(BETA.post[3,8,]), as.mcmc(BETA.post2[3,8,]))

gelman.diag(GR.BETA31)
gelman.diag(GR.BETA32)
gelman.diag(GR.BETA33)
gelman.diag(GR.BETA34)
gelman.diag(GR.BETA35)
gelman.diag(GR.BETA36)
gelman.diag(GR.BETA37)
gelman.diag(GR.BETA38)

GR.BETA41 = mcmc.list(as.mcmc(BETA.post[4,1,]), as.mcmc(BETA.post2[4,1,]))
GR.BETA42 = mcmc.list(as.mcmc(BETA.post[4,2,]), as.mcmc(BETA.post2[4,2,]))
GR.BETA43 = mcmc.list(as.mcmc(BETA.post[4,3,]), as.mcmc(BETA.post2[4,3,]))
GR.BETA44 = mcmc.list(as.mcmc(BETA.post[4,4,]), as.mcmc(BETA.post2[4,4,]))
GR.BETA45 = mcmc.list(as.mcmc(BETA.post[4,5,]), as.mcmc(BETA.post2[4,5,]))
GR.BETA46 = mcmc.list(as.mcmc(BETA.post[4,6,]), as.mcmc(BETA.post2[4,6,]))
GR.BETA47 = mcmc.list(as.mcmc(BETA.post[4,7,]), as.mcmc(BETA.post2[4,7,]))
GR.BETA48 = mcmc.list(as.mcmc(BETA.post[4,8,]), as.mcmc(BETA.post2[4,8,]))

gelman.diag(GR.BETA41)
gelman.diag(GR.BETA42)
gelman.diag(GR.BETA43)
gelman.diag(GR.BETA44)
gelman.diag(GR.BETA45)
gelman.diag(GR.BETA46)
gelman.diag(GR.BETA47)
gelman.diag(GR.BETA48)

##################
### Prediction ###
##################

time.predict = matrix(NA,nrow = 10000,ncol = 12)
logtime.predict = matrix(NA,nrow = 10000,ncol = 12)
X.predict = matrix(NA,nrow = 4,ncol = 8)
#X.predict for male (age 39, no children, log(personal income) 10.57) taking bus to work living in different cities
X.predict[1,]= c(1,0, mean(age),1, mean(aa),1,0,0)
#X.predict for male (age 39, no children, log(personal income) 10.57) taking subway to work living in different cities
X.predict[2,]= c(1,0, mean(age),1, mean(aa),0,1,0)
#X.predict for male (age 39, no children, log(personal income) 10.57) taking taxi to work living in different cities
X.predict[3,]= c(1,0, mean(age),1, mean(aa),0,0,1)


for(i in 1:10000)
{
  #group Chi.
  for(j in 1:3)
  {
    logtime.predict[i,j]=t(as.matrix(X.predict[j,]))%*%as.matrix(BETA.post[1,,i+10000])
    time.predict[i,j] = exp(logtime.predict[i,j])
  }
  
  #group NYC
  for(j in 4:6)
  {
    logtime.predict[i,j]=t(as.matrix(X.predict[j-3,]))%*%as.matrix(BETA.post[2,,i+10000])
    time.predict[i,j] = exp(logtime.predict[i,j])
  }
  
  #group Phi.
  for(j in 7:9)
  {
    logtime.predict[i,j]=t(as.matrix(X.predict[j-6,]))%*%as.matrix(BETA.post[3,,i+10000])
    time.predict[i,j] = exp(logtime.predict[i,j])
  }
  
  #group SF.
  for(j in 10:12)
  {
    logtime.predict[i,j]=t(as.matrix(X.predict[j-9,]))%*%as.matrix(BETA.post[4,,i+10000])
    time.predict[i,j] = exp(logtime.predict[i,j])
  }
}

### prediction for taking bus ###

plot(density(logtime.predict[,1]), xlab="predicted logtime", main="Density Plot - 
     Predicted Logtime (Male, Bus)", ylim=c(0, 0.33))
lines(density(logtime.predict[,4]), col = "red", lty = 2)
lines(density(logtime.predict[,7]), lty = 3)
lines(density(logtime.predict[,10]), col = "blue", lty = 6)
legend("topright", text.width = 4, x.intersp = 0.5, y.intersp = 0.3, legend=c("Chicago", "NYC", "Philadelphia", "San Francisco"),
       lty=c(1,2,3,6), col=c("black","red", "black","blue"))

### prediction for taking subway ###

plot(density(logtime.predict[,2]), xlab="predicted logtime", main="Density Plot - 
     Predicted Logtime (Male, Subway)", ylim=c(0, 0.4))
lines(density(logtime.predict[,5]), col = "red", lty = 2)
lines(density(logtime.predict[,8]), lty = 3)
lines(density(logtime.predict[,11]), col = "blue", lty = 6)
legend("topright", text.width = 3, x.intersp = 0.5, y.intersp = 0.3, legend=c("Chicago", "NYC", "Philadelphia", "San Francisco"),
       lty=c(1,2,3,6), col=c("black","red", "black","blue"))

### prediction for taking taxi ###

plot(density(logtime.predict[,3]), xlab="predicted logtime", main="Density Plot - 
     Predicted Logtime (Male, Taxi)", ylim=c(0, 0.25), xlim=c(-7,15))
lines(density(logtime.predict[,6]), col = "red", lty = 2)
lines(density(logtime.predict[,9]), lty = 3)
lines(density(logtime.predict[,12]), col = "blue", lty = 6)
legend("topright", x.intersp = 0.5, y.intersp = 0.3, legend=c("Chicago", "NYC", "Philadelphia", "San Francisco"),
       lty=c(1,2,3,6), col=c("black","red", "black","blue"))


### prediction for taking all 3 kinds of transportation in Chicago ###

plot(density(logtime.predict[,1]), xlab="predicted logtime", main="Density plot - All 3 Transportations", ylim=c(0, 0.4))
lines(density(logtime.predict[,2]), lty = 2)
lines(density(logtime.predict[,3]), lty = 3)
legend("topright", legend=c("Bus","Subway", "Taxi"), lty=c(1,2,3))

### Boxplot ###

# boxplot of all transportations in all cities
boxplot(time.predict, outline=F, ylab="time", main="Boxplot - All 3 Transportations in 4 Cities")

# boxplot of male taking bus to work 
time.predict.wf = matrix(NA, nrow = 10000, ncol = 4)
time.predict.wf[,1] = time.predict[,1]
time.predict.wf[,2] = time.predict[,4]
time.predict.wf[,3] = time.predict[,7]
time.predict.wf[,4] = time.predict[,10]
boxplot(time.predict.wf, xlab = "1=Chicago, 2=NYC, 3=Phi, 4=SF", main = "Boxplot - Male, Bus", outline = F)

# boxplot of male taking subway to work 
time.predict.wf = matrix(NA, nrow = 10000, ncol = 4)
time.predict.wf[,1] = time.predict[,2]
time.predict.wf[,2] = time.predict[,5]
time.predict.wf[,3] = time.predict[,8]
time.predict.wf[,4] = time.predict[,11]
boxplot(time.predict.wf, xlab = "1=Chicago, 2=NYC, 3=Phi, 4=SF", main = "Boxplot - Male, Subway", outline = F)


# boxplot of male taking taxi to work 
time.predict.wf = matrix(NA, nrow = 10000, ncol = 4)
time.predict.wf[,1] = time.predict[,3]
time.predict.wf[,2] = time.predict[,6]
time.predict.wf[,3] = time.predict[,9]
time.predict.wf[,4] = time.predict[,12]
boxplot(time.predict.wf, xlab = "1=Chicago, 2=NYC, 3=Phi, 4=SF", main = "Boxplot - Male, Taxi", outline = F)


### mean of all predict logtime ###

#logtime.predict.mean = matrix(NA, nrow=1, ncol=12)
#for (i in 1:12)
#{
#  logtime.predict.mean[i] = mean(logtime.predict[,i])
#}
#logtime.predict.mean


#time.predict.mean = exp(logtime.predict.mean)
#time.predict.mean


#1 chi bus
#2 chi subway
#3 chi taxi
#4 NYC bus
#5 NYC subway
#6 NYC taxi
#7 phi bus
#8 phi subway
#9 phi taxi
#10 sf bus
#11 sf subway
#12 sf taxi

###Prediction for male and female taking subway to work in four cities
time.predict2 = matrix(NA,nrow = 10000,ncol = 8)
logtime.predict2 = matrix(NA,nrow = 10000,ncol = 8)
X.predict2 = matrix(NA,nrow = 4,ncol = 8)
#X.predict for male (age 39, no children, log(personal income) 10.57) taking subway to work living in different cities
X.predict2[1,]= c(1,0, mean(age),1, mean(aa),0,1,0)
#X.predict for female (age 39, no children, log(personal income) 10.57) taking subway to work living in different cities
X.predict2[2,]= c(1,0, mean(age),0, mean(aa),0,1,0)


for(i in 1:10000)
{
#group Chi.
	for(j in 1:2)
	{
		logtime.predict2[i,j]=t(as.matrix(X.predict2[j,]))%*%as.matrix(BETA.post[1,,i+10000])
		time.predict2[i,j] = exp(logtime.predict2[i,j])
	}
	
#group NYC
	for(j in 3:4)
	{
		logtime.predict2[i,j]=t(as.matrix(X.predict2[j-2,]))%*%as.matrix(BETA.post[2,,i+10000])
		time.predict2[i,j] = exp(logtime.predict2[i,j])
	}
	
#group Phi.
	for(j in 5:6)
	{
		logtime.predict2[i,j]=t(as.matrix(X.predict2[j-4,]))%*%as.matrix(BETA.post[3,,i+10000])
		time.predict2[i,j] = exp(logtime.predict2[i,j])
	}
	
#group SF.
	for(j in 7:8)
	{
		logtime.predict2[i,j]=t(as.matrix(X.predict2[j-6,]))%*%as.matrix(BETA.post[4,,i+10000])
		time.predict2[i,j] = exp(logtime.predict2[i,j])
	}
}


boxplot(time.predict2, outline=F, ylab="time", main="Boxplot Ð Male and Female taking subway in 4 Cities")


###prediction for male having different number of children taking subway to work in four cities
time.predict3 = matrix(NA,nrow = 10000,ncol = 16)
logtime.predict3 = matrix(NA,nrow = 10000,ncol = 16)
X.predict3 = matrix(NA,nrow = 4,ncol = 8)
#X.predict for male (age 39, no children, log(personal income) 10.57) taking subway to work living in different cities
X.predict3[1,]= c(1,0, mean(age),1, mean(aa),0,1,0)
#X.predict for male (age 39, 1 child, log(personal income) 10.57) taking subway to work living in different cities
X.predict3[2,]= c(1,1, mean(age),0, mean(aa),0,1,0)
#X.predict for male (age 39, 2 children, log(personal income) 10.57) taking subway to work living in different cities
X.predict3[3,]= c(1,2, mean(age),1, mean(aa),0,1,0)
#X.predict for male (age 39, 3 children, log(personal income) 10.57) taking subway to work living in different cities
X.predict3[4,]= c(1,3, mean(age),0, mean(aa),0,1,0)



for(i in 1:10000)
{
#group Chi.
	for(j in 1:4)
	{
		logtime.predict3[i,j]=t(as.matrix(X.predict3[j,]))%*%as.matrix(BETA.post[1,,i+10000])
		time.predict3[i,j] = exp(logtime.predict3[i,j])
	}
	
#group NYC
	for(j in 5:8)
	{
		logtime.predict3[i,j]=t(as.matrix(X.predict3[j-4,]))%*%as.matrix(BETA.post[2,,i+10000])
		time.predict3[i,j] = exp(logtime.predict3[i,j])
	}
	
#group Phi.
	for(j in 9:12)
	{
		logtime.predict3[i,j]=t(as.matrix(X.predict3[j-8,]))%*%as.matrix(BETA.post[3,,i+10000])
		time.predict3[i,j] = exp(logtime.predict3[i,j])
	}
	
#group SF.
	for(j in 13:16)
	{
		logtime.predict3[i,j]=t(as.matrix(X.predict3[j-12,]))%*%as.matrix(BETA.post[4,,i+10000])
		time.predict3[i,j] = exp(logtime.predict3[i,j])
	}
}


boxplot(time.predict3, outline=F, ylab="time", main="Boxplot Ð Male with 0-4 children taking subway in 4 Cities")



