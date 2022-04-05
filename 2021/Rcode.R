library(rjags)

sesame <- read.table("sesame.txt", header = TRUE)

dat <- sesame[, c(3, 17)]
dat$sex <- dat$sex - 1

n <- nrow(dat)

listdat <- list("n" = n, "sex" = dat[, 1], "postnumb" = dat[, 2])

model.def <- jags.model(file = "ModelQ1.txt",
                        data = listdat, n.chains = 2)

update(model.def, 1000)

parameters <- c("b0", "b1", "sigma")

set.seed(123)

results <- coda.samples(model = model.def, variable.names = parameters, 
                        n.iter = 10000)

summary(results)

rm(list=ls())

load("SamplesExamQ4.RData")

B[1:10]
unique(B)
hist(B, breaks = 10000)