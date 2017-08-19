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
c = hfm[,3]
d = hfm[,4]
e = hfm[,5]
f = hfm[,6]

dat <- cbind(a,b,c,d,e,f)
colnames(dat) <- c("SW","ESB","RJ","LC","LA","F")

dat <- makemultdata(a,b,c,d,e,f,cuts = median(c(a, b, c, d, e, f)))
colnames(dat) <- c("SW","ESB","RJ","LC","LA","F")
# Fit a G component mixture model
G<-1
fit <- multmixEM(dat, k=G)
