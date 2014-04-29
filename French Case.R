eI <- paste0("/Users/yifanli/Downloads/For Xi's Work-complete data/", 'ie_data_st_1954.csv')
exceptInterest<- read.csv(eI,stringsAsFactors=F)
I <-  paste0("/Users/yifanli/Downloads/For Xi's Work-complete data/", 'Interest-Rate.csv')
interestF <- read.csv(I,stringsAsFactors=F)
#from 1871 to 2013
d.all <- cbind(exceptInterest,X1YEARINTEREST=interestF$X1YEARINTEREST)
head(d.all)
#d.all <- d.all[-90:0,]
tail(d.all)
nrow(d.all)
714/3
#Gets the quarterly data
Quarterly <- rep(c(T,F,F),nrow(d.all)/3)
sapply(1:nrow(d.all),function(x){
  if (Quarterly[x] == T)
    return d.all[x,]
})
d.all <- d.all[Quarterly ,]
#no NA involved ~
d.all <- d.all[-(715:717),]
#inflation from 1872 to 2013
inflation <- sapply(2:(nrow(d.all)),function(x){
  (100*(d.all$CPI[x]-d.all$CPI[x-1])/d.all$CPI[x-1])
})
#Add inflation of year 1872 as 0
inflation <- c(0,inflation)
inflation <- inflation/100+1
log_inflation <- log(inflation)
#d.all$X1YEARINTEREST[!is.finite(d.all$X1YEARINTEREST)] <- 0
d.all$X1YEARINTEREST <- d.all$X1YEARINTEREST/100+1
r0nom <- log(d.all$X1YEARINTEREST)
r0 <- r0nom - log_inflation
#calculate Xe
renorm <- log(as.numeric(d.all$SPINDEX))
re <- renorm-log_inflation
Xe <- re - r0
#Runs the 10 year government yield
Yt<-d.all$X10yrgovbondyield
yt <- log(1+Yt)
Dt <- (1-(1+Yt)^(-10)) / (1-(1+Yt)^(-1))
is.vector(Dt)
ytNext <- c(yt[-1],1)
rbnorm <- Dt*yt -(Dt-1)*ytNext

rb <- rbnorm-log_inflation
Xb <- rb - r0
d.all$DIVIDEND[!is.finite(d.all$DIVIDEND)] <- 30

ldmp <- log(as.numeric(d.all$DIVIDEND))-log(as.numeric(d.all$SPINDEX))
spr <- rbnorm - r0nom


mean(r0,na.rm=TRUE)


sd(r0,na.rm=TRUE)
mean(Xe,na.rm=TRUE)
sd(Xe,na.rm=TRUE)
mean(Xb,na.rm=TRUE)
sd(Xb,na.rm=TRUE)
mean(r0nom,na.rm=TRUE)
sd(r0nom,na.rm=TRUE)
mean(ldmp,na.rm=TRUE)
sd(ldmp,na.rm=TRUE)
mean(spr,na.rm=TRUE)
sd(spr,na.rm=TRUE)

install.packages("tseries")
library('tseries')
adf.test(r0)
adf.test(Xe)
adf.test(Xb)
adf.test(r0nom)
adf.test(ldmp)
adf.test(spr)

install.packages("vars")
library("vars")
?var
?VAR
r0100 <- ts(r0 * 100)
dtr0 = residuals(lm (r0100~time(r0100)))

Xe100 <- ts(Xe * 100)
dtXe = residuals(lm (Xe100~time(Xe100)))


Xb100 <- ts(Xb * 100)
dtXb <- residuals(lm(Xb100~time(Xb100)))

r0nom100 <- ts(r0nom * 100)
dtr0 <- residuals(lm(r0nom100~time(r0nom100)))

ldmpTS <- ts(ldmp)
dtldmp <- residuals(lm(ldmpTS~time(ldmpTS)))

sprTS <- ts(spr)
dtspr <- residuals(lm(sprTS~time(sprTS)))

var_est <- VAR(data.frame(r0,Xe,Xb,r0nom,ldmp,spr),1)
summary(var_est)
var_est <- VAR(data.frame(dtr0,dtXe,dtXb,dtr0,dtldmp,dtspr),1)
?VAR
#I got model order:  1 singularities in the computation of the projection matrix results are only valid up to model order 0
#I suspect I have too few observations(143) with too many variables(6),I'll try quarterly data
ar.ols(data.frame(r0100,Xe,Xb,r0nom100,ldmp,spr),order= 1)
?ar.ols
nrow(d.all)
