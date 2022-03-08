# In this exercise, you will practice with
# the Metropolis-Hastings sampler.
# Imagine that the posterior distribution (the target
# distribution) of the probability y is
# t(y) = 2 * y for y in the interval 0,1 
# and t(y) = 0 for y not in this interval.
# Use a Uniform distribution on the interval 0,1 as the 
# proposal density, that is, p(y) = U[0,1].
# Sample y using a Metropolis-Hastings sampler.

## Create the posterior in a function
posterior <- function(y){
  return(ifelse(y<0|y>1, 0, 2*y))
}

## Set number of iterations
iter <- 10000

y <- rep(0, 10000)

## Initial value
y[1] <- .000001

for(i in 2:iter) {
  prop_value <- y[i - 1] + runif(1, 0, 1)
  
  random_value <- runif(1, 0, 1)
  
  acceptance_ratio <- posterior(prop_value)/posterior(y[i - 1])
  
  if(random_value < acceptance_ratio) {
    y[i] <- prop_value } else { y[i] <- y[i - 1] }
  
}

plot(y)
