# Assignment 3
# Claijon da Sila

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/Documents - Claijon’s MacBook Pro/DataAnalytics/StochasticModels/Assignment3")
install.packages('ggfortify')
library(ggfortify)
library(survival)

offend = read.csv('offend.csv', header = TRUE)
attach(offend)
head(offend)

time <- durat
event <- fail

X = cbind(drugs, black, married, educ)

summary(time)
summary(event)
summary(X)



head( offend )
dat = offend[1:2]

durat = dat[,1]
fail = dat[,2]

survdat = Surv( time=durat, event=fail )
head(survdat)
survdat


fit <- survfit(Surv(time, status) ~ sex, data = lung)
autoplot(fit)

X = cbind(educ)
fit <- survfit(Surv(durat, fail) ~ X, data = offend)
print(fit)
autoplot(fit)


fit <- survfit( survdat~1, se=FALSE)s
autoplot(fit)
summary(fit)
plot(fit)
str(fit)
ggplot() +
  geom_line(data = fit, aes(x = time_grid, y = pred, colour = pred))


# Using cox regression
fit=coxph(formula = Surv( durat, fail) ~ ., data = offend)
print(fit)
summary(fit)


'Call:
coxph(formula = Surv(durat, fail) ~ ., data = offend)

n= 1445, number of events= 552

coef exp(coef) se(coef)      z Pr(>|z|)
drugs    0.22951   1.25798  0.09796  2.343  0.01913 *
black    0.42125   1.52386  0.08727  4.827 1.39e-06 ***
married -0.32605   0.72177  0.10503 -3.104  0.00191 **
educ    -0.01975   0.98045  0.01707 -1.157  0.24736
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

exp(coef) exp(-coef) lower .95 upper .95
drugs      1.2580     0.7949    1.0382    1.5242
black      1.5239     0.6562    1.2843    1.8081
married    0.7218     1.3855    0.5875    0.8867
educ       0.9804     1.0199    0.9482    1.0138

Concordance= 0.578  (se = 0.013 )
Rsquare= 0.026   (max possible= 0.995 )
Likelihood ratio test= 38.27  on 4 df,   p=9.858e-08
Wald test            = 37.29  on 4 df,   p=1.569e-07
Score (logrank) test = 37.64  on 4 df,   p=1.328e-07

Education has a negative coefficient -0.019
This tells us that people with education ( in our case with more years of school ) has
lower hazard but its hight p-value tells us that this years of education is not significant

This lower hazard increase the survival function as for our case it shows that it
will take longer to reofend with more education.

All tests fail to reject the ho hypothesis that the coeffient is equal to zero.

The test shows that out of 1445 inmates 552 had events.

The exponential of the coeffient tell us how the hazard changes.
The "years of education" decrease the hazard by a multiple of 0.9893

The CIs from 0.9482 to 1.01 include 1 so we have 1 as a possible value for the exponential
of the coeffient so we still cannot reject the idea that the difference between
the time for reoffend for those with more years of education and those of less years
of education, is significant.

The 3 hypothesis test also show that the null hypothesis that all coefficients are
igual to zero is significant and that we have enough evidence to suggest it.

'





library(survival)
install.packages("TH.data")
library(TH.data)
data("GBSG2")
GBSG2

GBSG2$tgrade = factor( GBSG2$tgrade, ordered = FALSE)
fit=coxph(formula = Surv( time, cens) ~ ., data = BGSG2)


