if (!require("psych")) install.packages("psych") # install this package first (once)
library(psych) # for the function describeBy

if (!require("bain")) install.packages("bain") # install this package first (once)
library(bain) # for bain function - needed for the data set

if (!require("restriktor")) install.packages("restriktor") # install this package first (once)
library(restriktor) # for goric function - needed to do calculate GORIC(A) weights

###################################################################################


# Ex. 1 (one study) #

# Read/Load the Data # 
# It is in the bain package, called sesamesim
# I re-name it to data:
data <- sesamesim

# Make the grouping variable site a factor #
data$site <- factor(data$site)  # this command tells R that site is a factor and not a continuous variable

# Inspect the data #
head(data) # Look at first (6) rows of the data

# Compute descriptives for each group #
descrstat <- describeBy((data$postnumb-data$prenumb), data$site, mat = TRUE)
descrstat



## Compute the GORIC ##

# First, we need the R object with unconstrained estimates 
lm_fit_sesam <-  lm((postnumb-prenumb) ~ site-1, data = data)

# Check the names used in model:
names(coef(lm_fit_sesam))
# Specify restrictions using those names.
#Note: it often comes down to the name and levels of the factor(s).

# Hypotheses Set
#
# NOTES: 
# - restriktor always needs pairs of restrictions!
# - restriktor uses "==" to denote an equality restriction
# - restriktor uses ";" to separate the restrictions within one hypothesis
#
# As a reminder, the 5 groups are:
#1.	 disadvantaged inner city,
#2.	 advantaged suburban,
#3.	 advantaged rural,
#4.	 disadvantaged rural,
#5.	 disadvantage Spanish speaking.

# Let's say, our hypotheses is that:
# - In rural areas (groups 3 and 4), the difference in knowledge of numbers is lower than in other regions.
#   {mu3, mu4} < {mu1, mu2, mu5} - thus, among other things mu3 < mu2 (i.e., mu2 > mu3)
# - Advantaged regions (groups 2 and 3) improve more than disadvantaged regions. 
#   {mu2, mu3} > {mu1, mu4, mu5} - thus, among other things mu3 > mu4, mu2 > mu1, mu2 > mu5
# A possible hypothesis could be: mu3 > mu4, mu2 > mu1, mu2 > mu5, mu2 > mu3, that is:
H1_sesam <- 'site3 > site4; site2 > site1; site2 > site5; site2 > site3'


# p-value null hypothesis test(s)
summary(lm_fit_sesam)
# Each mean is significantly different from 0.
# And hypothesis that all means are zero is rejected as well 
# (F-statistic: 46.08 on 5 and 235 DF,  p-value: < 2.2e-16).
# But: We do not know anything yet about the orderings expected on forehand 
#                      as specified in our theories/expectations/hypotheses.


# Calculate GORIC values and weights
#In the calculation of the GORIC, an iterative process is needed to calculate the penalty / complexity part. 
# Therefore, one needs to set a seed value:
#1. Then, you will obtain the same penalty value every time you run this code.
#2. Then, you can change the seed value to check the sensitivity of the penalty value.
#   If it is sensitive, then increase number of iterations used in calculation of the penalty.
set.seed(123) # Set seed value
output <- goric(lm_fit_sesam, H1_sesam, comparison = 'complement')
summary(output)
#
#The order-restricted hypothesis ‘H1_sesam’ has  5.477 times more support than its complement.

#In case you want to inspect the order-restricted maximum likelihood estimates (or-mle), inspect:
output$ormle


###################################################################################


# Ex. 2 (two studies) #

JU <- read.table("JU.txt", header=TRUE)

# Make the variable group a factor #
#Since we loaded a txt file, R does not know the measurement levels of the variables 
#                           and assumes all to be continuous (so, interval or ratio). 
#Hence, we need to tell R that the variable `group` is a factor 
#           (i.e., a grouping / categorical / nominal variable): 
JU$g <- factor(JU$g)  # this command tells R that g is a factor and not a continuous variable 

# Inspect the data #
head(JU) # Look at first (6) rows of the data

# Compute descriptives for each group #
descrstat <- describeBy(JU$z, JU$g, mat = TRUE)
descrstat



## Compute the GORIC ##
#In this example, we will use the JU data set to render theory-based hypotheses 
#for the replication data set of C.

## JU data

# First, we need the R object with unconstrained estimates
lm_fit_JU <-  lm(z ~ g-1, data = JU)
# Note that:
# 1. `y ~ group - 1` instructs the function `lm` (linear model) to regress y on group.
# 2. The `- 1` instructs the function `lm` to drop the intercept and, therefore, 
#    estimate the means in each group, resulting here in five group means. 
#    On the other hand,  `y ~ group' would estimate an intercept, 
#    representing the mean of the reference group, and 
#    the mean differences between the other (here, four) groups and the reference group.
# 3. The results are collected in, what is called, an R-object, named `lm_fit_...`.

# Check the names used in model
names(coef(lm_fit_JU))
# Specify restrictions using those names

# Hypotheses Set
#
# NOTES: 
# - restriktor always needs pairs of restrictions!
# - restriktor uses "==" to denote an equality restriction
# - restriktor uses ";" to separate the restrictions within one hypothesis
#
#On the JU data set, we could do an exploratory analysis, 
# which means that we are going to use all combinations with equalities (and no restrictions).
#With 4 means there are a lot of combinations, so you may want to reduce the number of hypotheses; 
# especially when you have some expectations already.
#In case there are already some theories in the literature, then use those.
#I just assume, the following two exist in the literature 
# (you can of course specify different ones or even only one):
H1_JU <- 'grh > gph; gph > grl; grl > gpl' 
H2_JU <- 'grh > gph; gph == grl; grl > gpl'  
# Note, these are used as competing hypotheses. 
#
# It is possible to have two theories which are not competing.
# Say, one addressing comparison between men and women and another one addressing different education levels.
# Then, do not evaluate them together, but each separately against their own complement.



# Calculate GORIC values and weights
#In the calculation of the GORIC, an iterative process is needed to calculate the penalty / complexity part. 
#Therefore, one needs to set a seed value:
#1. Then, you will obtain the same penalty value every time you run this code.
#2. Then, you can change the seed value to check the sensitivity of the penalty value.
#   If it is sensitive, then increase number of iterations used in calculation of the penalty.
set.seed(123) # Set seed value
goric_JU <- goric(lm_fit_JU, H1_JU, H2_JU)
summary(goric_JU)
# Both H1 and H2 are not weak hypotheses, since their support is stronger than for the unconstrained.
# Since at least one of the competing hypotheses is not weak, one can compare their support.
#
# It can be seen that H1_JU receives the most support. 
# But H2 does obtain some support as well.
# Now, it is up to you as a researcher to decide whether you will only evaluate H1_JU or both H1_JU and H2_JU in the replication study.



### Replication of the JU study: C

C <- read.table("C.txt", header = TRUE)
C$g <- factor(C$g) # this command tells R that gr is a factor and not a continuous variable like at

## Inspect data
#head(C)

## Compute descriptives for each group
#descrip <- describeBy(C$z,C$g,mat=TRUE)
#print(descrip)

# lm object (of ANOVA model)
lm_fit_C <-  lm(z ~ g-1, data = C)

# Check names used in model
names(coef(lm_fit_C))
# Specify restrictions using those names

# Set hypotheses
# Based on the results of JU, we perhaps only want to inspect H1_JU.
# Note that if you evaluated only equalities in the first set,
# you can use the sample means to come to order-restricted hypotheses.
# Then, use those here (so, then you update your hypotheses).
H1_C <- H1_JU

# Calculate GORIC values and weights
set.seed(123) # Set seed value
goric_c <- goric(lm_fit_C, H1_C, comparison = 'complement')
summary(goric_c)
# The order-restricted hypothesis ‘H1_C’ has 25.826 times more support than its complement.

###################################################################################

