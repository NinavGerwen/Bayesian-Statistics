# ====================================================================================================
# Question 2

# Execute the R code given below. To display the plots properly, 
# make certain your consol panel is bottom left, and, before running 
# the code, open the plots panel on the bottom right and make
# it large.

rm(list=ls())
sesamedata<-read.table("sesame.txt",header=TRUE)
dat <- sesamedata[,c(11,17,20)]
n=nrow(dat)

dat  <- list("n"=n, "y"= dat[,2],"x1"= dat[,1], "x2" = dat[,3])

ins1 <- list( b0=0, b1=1, b2=1)
ins2 <- list( b0=-5, b1=-2, b2=-2)
ivals <- list(ins1,ins2)

library(rjags)
modspec <- jags.model(file='modspec.txt',
                        data = dat,
                        inits=ivals,
                        n.chains = 2) 
update(modspec, 1000)
parstosave=c('b0','b1','b2')
samples=coda.samples(model=modspec, variable.names=parstosave, n.iter=10000)  
autocorr.plot(samples)

## Center data:

dat2 <- sesamedata[,c(11, 17, 20)]
dat2$prenumb <- dat2$prenumb - mean(dat2$prenumb)
dat2$peabody <- dat2$peabody - mean(dat2$peabody)

n = nrow(dat2)

dat2 <- list("n" = n, "y" = dat2[,2], "x1" = dat2[,1], "x2" = dat2[,3])

modspec2 <- jags.model(file='modspec.txt',
                      data = dat2,
                      inits=ivals,
                      n.chains = 2) 

update(modspec2, 1000)
parstosave <- c('b0','b1','b2')
samples2 <- coda.samples(model=modspec2, variable.names=parstosave, n.iter=10000)  
autocorr.plot(samples2)
# ====================================================================================================
# Question 3

# Modify modspec2 and modify the code below to be able
# to evaluate two way interactions.  

rm(list=ls())
sesamedata<-read.table("sesame.txt",header=TRUE)

dat <- sesamedata[,c(11,12,13,17)]
dat$prenumb <- dat$prenumb - mean(dat$prenumb)
dat$prerelat <- dat$prerelat - mean(dat$prerelat)
dat$preclasf <- dat$preclasf - mean(dat$preclasf)

n=nrow(dat)
dat  <- list("n"=n, "y"= dat[,4],"x1"= dat[,1], "x2" = dat[,2], "x3" = dat[,3])

library(rjags)

modspec2 <- jags.model(file='modspec2.txt',
                       data = dat,
                       n.chains = 2) 
update(modspec2, 1000)
parstosave=c('b0','b1','b2','b3','b4','tau')
samples2=coda.samples(model=modspec2, variable.names=parstosave, n.iter=10000)  
dic.model2 <- dic.samples(modspec2, 10000, "pD") 
dic.model2
