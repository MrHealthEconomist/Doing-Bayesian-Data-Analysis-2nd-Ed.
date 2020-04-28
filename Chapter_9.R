# ====================================================================================
# Load Libraries ----------------------------------------------------------
# ====================================================================================
library(rjags)
source("DBDA2E-utilities.R")

# ====================================================================================
# Calculating Gamma Distributions -----------------------------------------
# ====================================================================================
# The shape (s) and rate (r) parameters for a gamma distribution can be calculated. 
# Suppose we desire a dgamma with a mode omega = 42 and a sd = 20. Given:

# s = 1 + mode * r; where
# r = (mode + sqrt((omega ^ 2) + (4 * sd ^ 2))) / (2 * sd ^ 2) for a mode > 0

r <- (42 + sqrt((42 ^ 2) + (4 * 20 ^ 2))) / (2 * 20 ^ 2)
r

s <- 1 + 42 * r 
s

# Kruschke, however, has have created convenient utility functions in R that implement the
# parameter transformations:
gammaParam <- gammaShRaFromMeanSD(mean = 10, sd = 100)
gammaParam_2 <- gammaShRaFromModeSD(mode = 10, sd = 100)

gammaParam_2$shape
gammaParam_2$rate
# These functions may be useful when setting the constants for prior distributions.

# If you want to graph a gamma distribution in R, the gamma density is provided by dgamma:
dgamma(c(10, 100), shape = s, rate = r)

# ====================================================================================
# Doing Hierachical Dependencies with JAGS --------------------------------
# ====================================================================================
modelString <- "model {
 for (i in 1:Ntotal) {
  y[i] ~ dbern(theta[s[i]])
 }
 for (s in 1:Nsubj) {
  theta ~ dbeta(omega * (kappa - 2) + 1, (1 - omega) * (kappa - 2) + 1)
 }
 omega ~ dbeta(1, 1)
 kappa <- kappaMinusTwo + 2
 kappaMinusTwo ~ dgamma(.01, .01) # mean = 1, sd = 10 (a generic vague prior)
}"
writeLines(text = modelString, con = "modelCode.txt")
# The beginning of the model specification expresses the arrow for the Bernoulli 
# likelihood function and uses nested indexing in theta[s[i]]. The arrow expressing the 
# prior on omega is coded in JAGS as omega ∼ dbeta(1,1), which is a uniform distribution. 
# In general, the prior on omega can be whatever is an appropriate description of real 
# prior knowledge. In the present implementation, a vague and noncommittal prior is used 
# for generic application. Finally, the prior on k − 2 is implemented by two lines of code
# in the JAGS model specification.

# Notice that the dgamma distribution generates a variable called kappaMinusTwo, and then
# kappa itself is kappaMinusTwo + 2. This two-line implementation is needed because the 
# gamma distribution covers zero to infinity, but k must be no smaller than 2. The choice 
# of shape and rate constants in the dgamma distribution makes the prior allow a very 
# broad range of k values, yielding a sensible prior on ith theta.

# These details of the model specification in JAGS have been presented to demonstrate how
# easy it is to implement a hierarchical model in JAGS. If you can draw a coherent 
# hierarchical diagram, then you can implement it in JAGS. This functionality provides 
# great flexibility for defining meaningful descriptive models of data.

# ====================================================================================
# Theraputic touch example ------------------------------------------------
# ====================================================================================
# Read-in the data:
Data <- read.csv("TherapeuticTouchData.csv")
dim(Data)
# The first row of the data file must contain the names of the columns, and those column 
# names are included as the arguments sName and yName in the functions genMCMC and 
# plotMCMC.

# Load the relevant model functions provided by Kruschke (2014):
source("Jags-Ydich-XnomSsubj-MbernBetaOmegaKappa.R")

# Generate the MCMC chain:
mcmcCoda <- genMCMC(data = Data, sName = "s", yName = "y",
                    numSavedSteps = 20000, thinSteps = 10)
# In the call to the genMCMC function, you can see that there are 20,000 saved steps and
# thinning of 10 steps. These values were chosen after a short initial run with no 
# thinning (i.e., thinSteps = 1) showed strong autocorrelation that would have required a
# very long chain to achieve an effective sample size of 10,000 for the omega parameter. 
# To keep the saved MCMC sample down to a modest file size, I chose to set thinSteps = 10
# and to save 20,000 of the thinned steps. This still required waiting through 200,000 
# steps, however, and throwing away information. If computer memory is of no concern, 
# then thinning is neither needed nor recommended.

# Display the diagnostics of the chain, for specified parameters:
diagMCMC(codaObject = mcmcCoda, parName = "omega")
diagMCMC(codaObject = mcmcCoda, parName = "kappa")
diagMCMC(codaObject = mcmcCoda, parName = "theta[1]")
# The diagnostic plots produced by calls to the diagMCMC function all showed adequate 
# convergence. Only the kappa parameter showed high autocorrelation, which is typical of
# higher-level parameters that control the variance of lower-level parameters. 
# Nevertheless, the chains for kappa show good overlap and we are not concerned with a 
# very accurate estimate of its value. Moreover, we will see that the marginal posterior
# on kappa spans a wide range of values, and the estimates of the other parameters are
# not hugely affected by small changes in kappa.

# Get summary statistics of chain:
smryMCMC(mcmcCoda, compVal = .5, 
         diffIdVec = c(1, 14, 28), compValDiff = 0.0)
# A numerical summary of the posterior distribution is produced by the smryMCMC function. 
# An argument of the function that is unique to this model is diffIdVec. It specifies a
# vector of subject indices that should have their posterior differences summarized. For
# example, diffIdVec = c(1, 14, 28) produces summaries of the pos- terior distributions 
# of θ1 − θ14, θ1 − θ28, and θ14 − θ28. The argument defaults to showing no differences
# instead of all differences (which, in this application, would yield 28(28 − 1)/2 = 
# 378 differences).

# Display posterior info:
plotMCMC(mcmcCoda, data = Data, sName = "s", yName = "y",
         compVal = .5, diffIdVec = c(1, 14, 28), compValDiff = 0.0)
# The upper-right panel of the output shows the marginal posterior distribution on the 
# group-level mode, omega. It’s most credible value is less than 0.5, and its 95% HDI 
# includes the chance value of 0.5, so we would certainly not want to conclude that the 
# group of practitioners, as a class, detects the energy field of a nearby hand better 
# than chance. (If the group mode had been credibly less than 0.5, we might have inferred
# that the practitioners as a group could detect the experimenter’s hand but then 
# systematically misinterpreted the energy field to give a contrary response.)

# In conclusion, the posterior distribution indicates that the most credible values for
# the group as a whole and for all the individuals include chance performance. Either the
# experiment procedure was not appropriate for detecting the abilities of therapeutic 
# touch practitioners, or much more data would be needed to detect a very subtle effect
# detectable by the procedure, or there is no actual sensing ability in the 
# practitioners. Notice that these conclusions are tied to the particular descriptive
# model we used to analyze the data. We used a hierarchical model because it is a 
# reasonable way to capture individual differences and group-level tendencies. The model
# assumed that all individuals were representative of the same overarching group, and 
# therefore all individuals mutually informed each other’s estimates.

# It can be very useful to view the prior distribution in hierarchical models, as 
# sometimes the prior on mid-level parameters, implied by the top-level prior, is not 
# actually what was intuited or intended. It is easy to make JAGS produce an MCMC sample 
# from the prior by running the analysis without the data.

# ====================================================================================
# Shrinkage in Hierarchical Models ----------------------------------------
# ====================================================================================


















