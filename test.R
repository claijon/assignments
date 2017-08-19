library(mixtools)
# Saxony data
n <- 0:8
f <- c(215,1485,5331,10649,14959,11929,6678,2092,342)
x <- rep(n,f)
y<-8-x
dat<-cbind(x,y)
colnames(dat)<-c("M","F")

# Fit a G component mixture model
G<-1
fit<-multmixEM(dat,k=G)
# Examine the fit
summary(fit)

# store data in a object
hfm = read.csv('HarrisonFord.csv', header = TRUE)
colnames(hfm) <- c("SW","ESB","RJ","LC","LA","F")
a = hfm[,1]
b = hfm[,2]
#c = hfm[,3]
#d = hfm[,4]
#e = hfm[,5]
#f = hfm[,6]

dat <- cbind(a,b,c,d,e,f)
colnames(dat) <- c("SW","ESB","RJ","LC","LA","F")

dat <- cbind(a,b)
colnames(dat) <- c("SW","ESB")

dat <- makemultdata(a,b,c,d,e,f,cuts = median(c(a, b, c, d, e, f)))
colnames(dat) <- c("SW","ESB","RJ","LC","LA","F")
# Fit a G component mixture model
G<-2
fit <- multmixEM(dat, k=G)
summary(fit)


library(BayesLCA)
hfm = read.csv('HarrisonFord.csv', header = TRUE)
colnames(hfm) <- c("SW","ESB","RJ","LC","LA","F")

res = rep(NA, 4)
for(i in 1:6){
  fit <- blca.em(hfm,i)
  res[i] = fit$BIC
}
res
index = which.max(res)
cat("Groups:", index, "BIC:", res[index], sep=" ")


fit <- blca.em(hfm,4)
summary(fit)
plot(fit)

set.seed(1)
# Load the CO_2 data
#install.packages("flexmix")
library(flexmix)
data(CO2data)
# Load the mixtools package
library(mixtools)
# Fit a mixture of experts model with 50 random starting values for the EM algorithm.
# The highest BIC value is stored as bicval and the best fitting model as bestfit
bicval <- -Inf
itermax <- 50
for (iter in 1:itermax)
{
  G<-3
  fit<-regmixEM(CO2data$CO2,CO2data$GNP,k=G)
  n<-nrow(CO2data)
  p<-nrow(fit$beta)*G+G+(G-1)
  fitbic <- 2*fit$loglik - log(n)*p
  if (bicval<fitbic)
  {
    bicval<-fitbic
    bestfit<-fit
    print(c(iter,bicval))
  }
}
# Explore the fitted model
summary(bestfit)
plot(bestfit,which=2)


fit <- blca.em(hfm,4)
groups = c("Group 1","Group 2","Group 3","Group 4")
tab = as.data.frame(cbind("Groups" = groups, 
                          "SW" = fit$itemprob[,1],
                          "ESB" = fit$itemprob[,2],
                          "RJ" = fit$itemprob[,3],
                          "LC" = fit$itemprob[,4],
                          "LA" = fit$itemprob[,5],
                          "F" = fit$itemprob[,6]
)
)
kable(round(tab,3), capition = "Probability within the groups")



# Load the flexmix package
library(flexmix)
attach(hfm)
F
SW
# Load the CO_2 data
# Fit a mixture of experts model with 50 random starting values for the EM algorithm.
# The highest BIC value is stored as bicval and the best fitting model as bestfit
bicval <- Inf
itermax <- 50
for (iter in 1:itermax)
{
  fit<-flexmix(SW~ESB+RJ+LC+LA+F,data=hfm,k=6)
  if (bicval>BIC(fit))
  {
    bicval<-BIC(fit)
    bestfit<-fit
    print(c(iter,bicval))
  }
}
# Explore the fitted model
summary(bestfit)
parameters(bestfit)
plot(bestfit,which=4)


set.seed(1)

# Fit a mixture of experts model with 50 random starting values for the EM algorithm.
# The highest BIC value is stored as bicval and the best fitting model as bestfit
bicval <- -Inf
itermax <- 50
for (iter in 1:itermax)
{
  G<-2
  fit<-regmixEM(SW,ESB+RJ+LC+LA+F,k=G)
  n<-nrow(hfm)
  p<-nrow(fit$beta)*G+G+(G-1)
  fitbic <- 2*fit$loglik - log(n)*p
  if (bicval<fitbic)
  {
    bicval<-fitbic
    bestfit<-fit
    print(c(iter,bicval))
  }
}
# Explore the fitted model
summary(bestfit)
plot(bestfit,which=2)