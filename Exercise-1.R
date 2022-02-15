### Exercise 1 - Bayesian Statistics

library("rjags")

## Step 1: Data input ----------------------------------------------------------

source('Exercise 1 - Data.txt')

## Step 2: Specifying model ----------------------------------------------------

## This is done in the Exercise 1 - model.txt file

## Step 3: Obtaining initial values --------------------------------------------

## Not necessary in this exercise

## Step 4: Obtaining samples from the posterior distribution of the parameters -

## Defining the model from the file into R
model.def <- jags.model(file = "Exercise 1 - Model.txt", data = dat, n.chains = 2)

## Burn-in iterations
update(object = model.def, n.iter = 1000)

## Set monitors on the parameters of interest and draw a large number of samples from the posterior
## distribution

parameters <- c('theta.PE', 'theta.PC', 'RR')
res <- coda.samples(model = model.def, variable.names = parameters, n.iter = 10000)

## Step 5: Inspecting convergence ----------------------------------------------

## Not done in this exercise

## Step 6: Substantive interpretation ------------------------------------------

summary(res)

## These results can also be derived analytically of course through mathematics

### The same exercise, with historical data

## Picking the study: one investigated women, other one men (same as us)
## hence we pick the previous study that also investigated men

## The chosen weight:

## Our sample size = 284
## Previous study ss = 235

## We don't want our historical data to have more than 50% of our sample size,
## so we only take 40% of the historical data -- weight = .4

## obtaining power prior.

hist.dat <- list(y.PE = 40, n.PE = 105, y.PC = 45, n.PC = 130)

hist.model <- jags.model("Exercise 1 - Hist Model.txt", data = hist.dat, n.chains = 2)
update(object = hist.model, n.iter = 1000)

hist.parameters <- c('hist.theta.PC', 'hist.theta.PE')

hist.res <- coda.samples(model = hist.model, variable.names = hist.parameters, n.iter = 10000)

summary(hist.res)

## Okay never mind... Can be done easier. However in the below example I did not do a and b times .4!

## New model:

new.model.def <- jags.model(file = "Exercise 1 - New Model.txt", data = dat, n.chains = 2)

update(object = new.model.def, n.iter = 1000)

parameters <- c('theta.PE', 'theta.PC', 'RR')
new.res <- coda.samples(model = new.model.def, variable.names = parameters, n.iter = 10000)

summary(new.res)

## Comparison
summary(res)

## RR went from .69 to .78 

## Step 5: Inspecting convergence ----------------------------------------------

