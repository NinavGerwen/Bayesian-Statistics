## ----setup, include=FALSE--------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----Loading the data and descriptive statistics, include=FALSE---------------------------------------------------------------------------------
library(haven)

dat <- read_sav(file = "Week6Data2.sav")

dat <- dat[, 2:4]

## ----Descriptive statistics, include = FALSE-------------------------------------------------------------------------
## For ranges and means:
summary(dat)
## For standard deviations:
sd(dat$verbal)
sd(dat$SES)
sd(dat$IQ)
## For visual inspection of the histograms:
hist(dat$verbal, breaks = 20)
hist(dat$SES)
hist(dat$IQ)

## To grand mean center the variable of verbal ability
dat$verbal <- dat$verbal - mean(dat$verbal)


## ----The Gibbs/MH Sampler function, include=FALSE--------------------------------------------------------------------
## For the input of the Gibbs/MH Sampler, one should provide: 
        ## a vector of continuous variables that serves as your dependent variable
        ## two vectors of continuous variables that serve as your independent variables
        ## a burn.in period (by default 1000 iterations)
        ## a total amount of iterations (by default 10000)
        ## the initial values for every regression coefficient
        ## a method function that specifies per parameter whether they would 
        ## like to have it sampled through a Gibbs Sampler or Metropolis Hastings algorithm
          ## by default, it assumes a Gibbs sampler for every regression coefficient, 
          ## and if you want to redefine it to a MH, you should change the 0 to a 1
          ## for the corresponding regression coefficient you want to change
              ## Examples: 
                ## method = c(0, 1, 0) will make it so that b1 is done through MH
GibbsSampler <- function(Y, X1, X2, burn.in = 1000, iterations = 10000, 
                         initial.values = c(1, 1, 1), method = c("0", "0", "0")) {
  ## First, the function should require ggplot2 as it is used to create traceplots later
  library(ggplot2)
  ## First create space for all the regression coefficients
  b0 <- rep(NA, iterations)
  b1 <- rep(NA, iterations)
  b2 <- rep(NA, iterations)
  vari <- rep(NA, iterations)
  
  ## Then assume the specified initial values
  b0[1] <- initial.values[1]
  b1[1] <- initial.values[2]
  b2[1] <- initial.values[3]
  vari[1] <- 1
  N <- length(Y)
 
  ## Now, we will start the sampling, it should start at the second element
  ## because the first element will be the initial values
  for(i in 2:iterations) {
    
    ## In every loop, We first update the intercept:
    ## Check whether the intercept should be sampled through Gibbs or MH
    if(method[1] == "0") {
      
      ## If through Gibbs Sampler:
      
      ## First, we update the parameters of the intercept (mean and standard deviation) with the following formula:
    
      mean.0.post <- (sum(Y - (b1[i - 1]*X1 - b2[i - 1]*X2), na.rm = TRUE)/vari[i - 1] + 
                        (mu0 / sigma0)) / ((N / vari[i - 1]) + (1 / sigma0))
      sd.0.post <- 1 / ((N / vari[i - 1]) + (1 / sigma0))

      ## Then through rnorm we randomly sample from a normal distribution the next value for b0 with the
      ## above calculated mean and standard deviation
      b0[i] <- rnorm(n = 1, mean = mean.0.post, sd = sqrt(sd.0.post))
    } else {
      
      ## If through MH:
      ## First, we gain a proposal value through adding the lm point estimate
      ## with the standard error of the estimate times 
      ## a random sampled value of a T-distribution with df = 1
      proposal_value <- 115.45920 + 1.95990 * rt(1, df = 1)
      ## Then calculate the acceptance ratio
      accept_ratio <- (dnorm(proposal_value, mean = 115.45, sd = 19.96)/dnorm(b0[i - 1], mean = 115.45, sd = 19.96)) * 
        (dt(b0[i - 1], df = 1)/dt(proposal_value, df = 1))
      ## Then see whether we accept the proposed value
      if(runif(1,0,1) < accept_ratio) {
        b0[i] <- proposal_value } else { b0[i] <- b0[i - 1] }
    }
 
    ## Then, every loop, we will update the coefficient of the first independent variable
    
    ## Again, check whether Gibbs or MH:
    if(method[2] == "0") {
    
      ## If through Gibbs:
      
      ## We can update the mean and standard deviation of the regression coefficient for the first variable
      ## using the updated value of the intercept
      mean.1.post <- ((sum(X1*(Y - b0[i] - b2[i - 1]*X2), na.rm = TRUE)/vari[i - 1]) + 
                        (mu1 / sigma1)) / ((sum((X1^2), na.rm = TRUE)/vari[i - 1]) + (1 / sigma1))
      sd.1.post <- 1/((sum((X1^2), na.rm = TRUE) / vari[i - 1]) + (1 / sigma1))
    
      ## Then again we randomly sample a new b1 through rnorm
      b1[i] <- rnorm(n = 1, mean = mean.1.post, sd = sqrt(sd.1.post))
      
    } else {
      ## If through MH:
      ## First, we gain a proposal value in a similar fashion as above
      proposal_value <- 11.73647 + 1.45593 * rt(1, df = 1)
      ## Calculate acceptance ratio
      accept_ratio <- (dnorm(proposal_value, mean = 11.73, sd = 1.46)/dnorm(b1[i - 1], mean = 11.73, sd = 1.46)) * 
        (dt(b1[i - 1], df = 1)/dt(proposal_value, df = 1))
      ## Then see whether we accept the proposed value
      if(runif(1,0,1) < accept_ratio) {
        b1[i] <- proposal_value } else { b1[i] <- b1[i - 1] }
    }
    
    ## Then, every loop, we update the regression coefficient of the second variable
    
    ## First check whether Gibbs or MH:

    if(method[3] == "0") {
      
      ## If through Gibbs:
    
      ## We update the mean and standard deviation of the regression coefficient for the second variable
      ## using the updated value of both the intercept and b1
      mean.2.post <- (sum(X2*(Y - b0[i] - b1[i]*X1), na.rm = TRUE)/vari[i - 1] + 
                        (mu2 / sigma2)) / ((sum(X2^2, na.rm = TRUE)/vari[i - 1]) + (1 / sigma2))
      sd.2.post <- 1/((sum(X2^2, na.rm = TRUE) / vari[i - 1]) + (1 / sigma2))
   
      ## Then, we randomly sample a new b2
      b2[i] <- rnorm(n = 1, mean = mean.2.post, sd = sqrt(sd.2.post))
    } else {
      ## If through MH:
      ## First, we gain a proposal value
      proposal_value <- 0.08399 + 0.05738 * rt(1, df = 1)
      ## Calculate acceptance ratio
      accept_ratio <- (dnorm(proposal_value, mean = 0.08, sd = 0.06)/dnorm(b2[i - 1], mean = 0.08, sd = 0.06)) * 
        (dt(b2[i - 1], df = 1)/dt(proposal_value, df = 1))
      ## Then see whether we accept the proposed value
      if(runif(1,0,1) < accept_ratio) {
        b2[i] <- proposal_value } else { b2[i] <- b2[i - 1] }
    }
    
    ## Finally, we update the parameters for the distribution of our variance using all above updated values
    a.post <- (N / 2) + a.prior
    b.post <- (sum((Y - (b0[i] + b1[i]*X1 + b2[i]*X2))^2, na.rm = TRUE) / 2) + b.prior

    ## And randomly sample a new variance again through rgamma
    vari[i] <- 1/rgamma(n = 1, shape = a.post, rate = b.post)
      ## We use the inverse of the randomly sample because we want the value of the variance, and not the precision.
  }
  
  ## Then, we remove the values of the burn-in
  b0 <- b0[-c(1:burn.in)]
  b1 <- b1[-c(1:burn.in)]
  b2 <- b2[-c(1:burn.in)]
  vari <- vari[-c(1:burn.in)]
  
  ## We also want the posterior distributions (histograms) of all parameters
  par(mfrow = c(2,2))
  hist(b0, breaks = 50)
  abline(v = mean(b0), col = "blue")
  hist(b1, breaks = 50)
  abline(v = mean(b1), col = "blue")
  hist(b2, breaks = 50)
  abline(v = mean(b2), col = "blue")
  hist(vari, breaks = 50)
  
  ## We want a dataframe consisting of all the sampled parameter values
  data_frame <- as.data.frame(cbind(b0, b1, b2, vari, iter = 1:length(b0)))
  
  ## And we want traceplots for all the parameters to assess convergence
  traceplot1 <- ggplot(data_frame, aes(x=iter, y=b0)) + 
    geom_line() + 
    labs(title="b0 trace")
  traceplot1
  
  traceplot2 <- ggplot(data_frame, aes(x=iter, y=b1)) + 
    geom_line() + 
    labs(title="b1 trace")
  traceplot2
  
  traceplot3 <- ggplot(data_frame, aes(x=iter, y=b2)) + 
    geom_line() + 
    labs(title="b2 trace")
  traceplot3
  
  traceplot4 <- ggplot(data_frame, aes(x=iter, y=vari)) + 
    geom_line() + 
    labs(title="var trace")
  traceplot4
  
  ## We create a list of all the output we want
  list_of_output <- list(data_frame, traceplot1, traceplot2, traceplot3, traceplot4)
  
  ## And finally, we ask the function to return this list of output
  return(list_of_output)
}

## Note, the above function only works if you first set all priors
## (i.e., a.prior, b.prior, mu0, mu1, mu2, sigma0, sigma1, sigma2)


## ----include=FALSE---------------------------------------------------------------------------------------------------
## Analysis 1: Uninformative prior + Gibbs -------------------------------------

## First set a seed for reproducibility
set.seed(3)

## Then set all priors to uninformative
a.prior <- 0.001
b.prior <- 0.001

mu0 <- 1
sigma0 <- 10000

mu1 <- 1
sigma1 <- 10000

mu2 <- 1
sigma2 <- 10000

## Then run the GibbsSampler function with our data

Analysis.1<- GibbsSampler(dat$IQ, dat$verbal, dat$SES, burn.in = 2500, 
                            iterations = 12500, initial.values = c(1, 1, 1))

## Analysis 2: Independent MH Sampler ------------------------------------------

Analysis.2 <- GibbsSampler(dat$IQ, dat$verbal, dat$SES, burn.in = 5000, 
                            iterations = 25000, initial.values = c(1, 1, 1),
                            method = c("1", "1", "1"))


## ----Parameter estimates & Credible Intervals, include=FALSE---------------------------------------------------------
## FIRST ANALYSIS --------------------------------------------------------------
## First, subset the dataframe from the list of output
LR1.dataframe <- Analysis.1[[1]]
## Now, to get the expected value and credible intervals of every regression coefficient:
mean(LR1.dataframe$b0)
quantile(LR1.dataframe$b0, probs = c(0.025, 0.975))
mean(LR1.dataframe$b1)
quantile(LR1.dataframe$b1, probs = c(0.025, 0.975))
mean(LR1.dataframe$b2)
quantile(LR1.dataframe$b2, probs = c(0.025, 0.975))

## SECOND ANALYSIS -------------------------------------------------------------
LR2.dataframe <- Analysis.2[[1]]

mean(LR2.dataframe$b0)
quantile(LR2.dataframe$b0, probs = c(0.025, 0.975))
mean(LR2.dataframe$b1)
quantile(LR2.dataframe$b1, probs = c(0.025, 0.975))
mean(LR2.dataframe$b2)
quantile(LR2.dataframe$b2, probs = c(0.025, 0.975))


## ----Autocorrelation plot, include=FALSE-----------------------------------------------------------------------------
## Autocorrelation plot function:
    ## For input: the autocorrelation plot should require a vector
    ## and the total amount of lags they wish to have (by default 50)
autocorrelationplot <- function(V, lag = 50) {
  ## First, it creates space for the values of the autocorrelations
  autocors <- rep(NA, lag)
  ## Then, a for loop is created that lasts up to the specified lag
  for(i in 1:lag) {
    ## First, it creates two versions of the given vector, where
    ## in the first version, there are i * 0's added at the end
    ## and in the second version, there are i * 0's added at the beginning
    V1 <- c(V, rep(0, i))
    V2 <- c(rep(0, i), V)
    ## Then, the two versions are put into a matrix
    matrix <- cbind(V1, V2)
    ## Then, we remove the i elements at the top and the bottom
      ## This is done so that the two vectors are of the same length
      ## and the values for which there is no lagged version are removed
          ## example: the third sampled value can not have a lag greater than 3
    matrix <- head(matrix, -i)
    matrix <- tail(matrix, -i)
    ## Finally, we ask for the correlation between the two vectors
    ## and put it in the created space
    autocors[i] <- abs(cor(matrix[, 1], matrix[, 2])[1])
  }
  
  ## Then, the autocorrelation plot is created with the lag on x-axis
  ## and the correlation values on the y-axis
  autocorplot <- plot(1:lag, autocors, ylim = c(0, 1))
  return(autocorplot)
}


## ----Convergence checks, include=FALSE-------------------------------------------------------------------------------
par(mfrow = c(2, 2))
## Convergence for the first analysis ------------------------------------------
    ## TRACEPLOTS
Analysis.1[[2]]
Analysis.1[[3]]
Analysis.1[[4]]
Analysis.1[[5]]
    ## AUTOCOR PLOTS
autocorrelationplot(V = LR1.dataframe$b0)
autocorrelationplot(V = LR1.dataframe$b1)
autocorrelationplot(V = LR1.dataframe$b2)
autocorrelationplot(V = LR1.dataframe$vari)

par(mfrow = c(2,2))
## Convergence for the second analysis -----------------------------------------
    ## TRACEPLOTS
Analysis.2[[2]]
Analysis.2[[3]]
Analysis.2[[4]]
Analysis.2[[5]]
    ## AUTOCOR PLOTS
autocorrelationplot(V = LR2.dataframe$b0)
autocorrelationplot(V = LR2.dataframe$b1)
autocorrelationplot(V = LR2.dataframe$b2)
autocorrelationplot(V = LR2.dataframe$vari)


## ----DIC calculations, include=FALSE---------------------------------------------------------------------------------
## To calculate the DIC, we create a function that as input
## requires the outcome variable, the independent variables and the
## posterior distribution (i.e., the gibbs sampled values)
DIC <- function(y, x1, x2, posterior) {
  ## First, we create a function that will give the log likelihood 
  ## with as input the sampled parameters
      ## The log likelihood is calculated by summing the results of the
      ## density function of all our outcome variables with the 
      ## predicted values as the mean and the variance coefficient
      ## as the standard deviation with log = TRUE, because we want the
      ## log likelihood, not the likelihood
  loglik <- function(parameters) {sum(dnorm(y, mean = parameters[1] + 
                                            parameters[2]*x1 + parameters[3]*x2, 
                                            sd = sqrt(parameters[4]), log = TRUE))}
  
  ## For the calculation of Dhat, we need to get the means of the
  ## Gibbs sampled values, we get these through colMeans and save them
  mean_parameters <- colMeans(posterior)
  
      ## Then, we can calculate Dhat by using our log likelihood function
      ## on the saved mean values and multiplying it by -2
  Dhat <- -2 * loglik(mean_parameters)
  
  ## For Dbar, which is the average likelihood over the posterior distribution
  ## we do the following:
      ## We get all individual log likelihoods for every row of sampled
      ## parameter values, and multiply these all by -2 and then
      ## take the mean of all the individual log likelihoods
  Dbar <- mean(- 2 * (apply(posterior, 1, loglik)))

  ## Then finally, we can calculate the DIC by taking -Dhat and
  ## adding 2 * Dbar to this!
  DIC_value <- -Dhat + 2 * Dbar
  
  ## And lastly, of course, the function should return this value
  return(DIC_value)
}

## Now, we can use the function on both our Gibbs and MH sampled analyses
Gibbs_DIC <- DIC(dat$IQ, dat$verbal, dat$SES, LR1.dataframe[1:4])
Gibbs_DIC

MH_DIC <- DIC(dat$IQ, dat$verbal, dat$SES, LR2.dataframe[1:4])
MH_DIC


## ----PP P-values, include=FALSE--------------------------------------------------------------------------------------
## First, we set a seed for reproducibility
set.seed(3)
## Then, we create an empty matrix with equal rows to our original dataset
## and as many columns as we would like datasets
sim.data <- matrix(data = NA, nrow = 400, ncol = 5000)

## Then, we create space for the regression coefficients that we will sample
## so that we can use these to calculate the residuals later
b0_t <- rep(NA, 5000)
b1_t <- rep(NA, 5000)
b2_t <- rep(NA, 5000)
s2_t <- rep(NA, 5000)

## Now, we can start the data simulation through a for loop
for(i in 1:5000) {
  ## In every loop, we sample one value from all our regression coefficients
  b0_t[i] <- sample(LR1.dataframe$b0, 1)
  b1_t[i] <- sample(LR1.dataframe$b1, 1)
  b2_t[i] <- sample(LR1.dataframe$b2, 1)
  s2_t[i] <- sample(LR1.dataframe$vari, 1)

  ## Then, we create a column of simulated Y values that use the sampled
  ## regression coefficients through rnorm, where the mean is their 
  ## expected value and the sd is the sampled variance
  sim.data[, i] <- rnorm(400, mean = b0_t[i] + b1_t[i]*dat$verbal + b2_t[i]*dat$SES, sd = sqrt(s2_t[i]))
}

## Now, as a discrepancy measure, we calculate the residuals for every
## simulated Y values using their corresponding sampled regression coefficients
residuals_sim <- matrix(data = NA, nrow = 400, ncol = 5000)

for(i in 1:5000) {
  residuals_sim[, i] <- sim.data[, i] - b0_t[i] - b1_t[i]*dat$verbal - b2_t[i]*dat$SES
}

## For the observed dataset, we also calculate the residuals a 1000 times
## using the sampled regression coefficients
residuals_obs <- matrix(data = NA, nrow = 400, ncol = 5000)

for(i in 1:5000){
  residuals_obs[, i] <- dat$IQ - b0_t[i] - b1_t[i]*dat$verbal - b2_t[i]*dat$SES
}

## ORIGINAL TEST STATISTIC -----------------------------------------------------

## Then, for every column of residuals (simulated and observed), we calculate
## our original test statistic
sim_skewness <- rep(NA, 5000)
for(i in 1:5000) {
  sim_skewness[i] <- (mean(residuals_sim[, i]) - median(residuals_sim[, i]))^2
}

obs_skewness <- rep(NA, 5000)

for(i in 1:5000) {
  obs_skewness[i] <- (mean(residuals_obs[, i]) - median(residuals_obs[, i]))^2
}

## And we calculate the Posterior Predictive P-value by finding the proportion
## of times that our statistic is larger in the simulated residuals than 
## in the observed residuals
ppp_value <- sum(sim_skewness > obs_skewness)/5000
ppp_value

## SKEWNESS STATISTIC ----------------------------------------------------------

library(moments)

obs_skew <- rep(NA, 5000)
sim_skew <- rep(NA, 5000)

## Again, we calculate skewness a thousand times for both the observed
## and simulated residuals
for(i in 1:5000) {
  obs_skew[i] <- skewness(residuals_obs[, i])
  sim_skew[i] <- skewness(residuals_sim[, i])
}

## And again, we calculate the posterior predictive p-value by finding the
## proportion of times that the skewness for simulated residuals is larger
## than the skewness in the observed residuals
comparison_ppp <- sum(sim_skew > obs_skew)/5000
comparison_ppp

## Visual inspection of the residuals in observed dataset
hist(dat$IQ - 118.3201 - 11.92829 * dat$verbal - 0.003478761 * dat$SES)


## ----Bayes Factor Calculations, include=FALSE------------------------------------------------------------------------
library(MASS)

## To be able to get the multivariate posterior density, we get
## the means and covariance matrix of the posterior distributions 
## from the IV regression coefficients
Means <- c(mean(LR1.dataframe$b1), mean(LR1.dataframe$b2))
Sigma <- cov(LR1.dataframe[2:3])

## Now, we will sample from the posterior and prior distributions

    ## First set a seed
set.seed(3)
    ## Then sample from the posterior, using the above specified means and sigma
posterior <- mvrnorm(100000, mu = Means, Sigma = Sigma)
    
    ## Calculate the fractional value
b <- 6/400 ## 3 regression coefficients: 3 means, 3 variances --> 6

    ## Then sample from the prior, using means = 0 and sigma = sigma/b
prior <- mvrnorm(100000, mu = c(0, 0), Sigma = Sigma/b)

## Finally, we can calculate the Bayes Factor for all our hypotheses
## by stating the correct statements

## For hypothesis 1: 
## For both the posterior and prior, we want the proportion of sampled values where both
## regression coefficients are higher than 0 and divide them in the correct way
BF.H1 <- (sum(posterior[, 1] > 0 & posterior[, 2] > 0)/100000) /
  (sum(prior[, 1] > 0 & prior[, 2] > 0)/100000)

## For hypothesis 2:
## For both posterior and prior, the first regression coefficient should be higher
## than 0 and the second one should be close to zero (i.e., -.1 < x < .1)
BF.H2 <- (sum(posterior[, 1] > 0 & posterior[, 2] < .1 & posterior[, 2] > -.1)/100000) / 
  (sum(prior[, 1] > 0 & prior[, 2] < .1 & prior[, 2] > -.1)/100000)

## For hypothesis 3:
## Exactly the other way around from hypothesis 2
BF.H3 <- (sum(posterior[, 2] > 0 & posterior[, 1] < .1 & posterior[, 1] > -.1)/100000) / 
  (sum(prior[, 2] > 0 & prior[, 1] < .1 & prior[, 1] > -.1)/100000)

## Sensitivity analysis for H1 and H2
  ## First, we create space for the BF values for H1
BFH1_values <- rep(NA, 50)
  ## Then, we create a for loop from 1 to 50 where
for(i in 1:50) {
    ## First, b is calculated using the following formula
  b <- as.numeric(i) / 400
    ## Then, we sample from the prior using mvrnorm() and the correct
    ## specifications
  prior <- mvrnorm(100000, mu = c(0, 0), Sigma = Sigma/as.numeric(b))
    ## Then, we calculate the correct BF values for hypothesis 1 and
    ## put these in the ith element of the earlier created space
  BFH1_values[i] <- (sum(posterior[, 1] > 0 & posterior[, 2] > 0)/100000) /
  (sum(prior[, 1] > 0 & prior[, 2] > 0)/100000)
}

## Finally, we plot the BF1 values for every value of J (1:50)
plot(x = 1:50, y = BFH1_values, ylim = c(0, 5))

## The above process is repeated for Hypothesis 2
BFH2_values <- rep(NA, 50)

for(i in 1:50) {
  b <- as.numeric(i) / 400
  prior <- mvrnorm(100000, mu = c(0, 0), Sigma = Sigma/as.numeric(b))
  BFH2_values[i] <- (sum(posterior[, 1] > 0 & posterior[, 2] < .1 & posterior[, 2] > -.1)/100000) / 
  (sum(prior[, 1] > 0 & prior[, 2] < .1 & prior[, 2] > -.1)/100000)
  
}

plot(x = 1:50, y = BFH2_values)

