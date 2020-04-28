library(rjags)
library(runjags)
library(parallel)

# Enable parallel processing for project:
options(mc.cores = detectCores())

# ==========================================================================================
# Load Data ---------------------------------------------------------------
# ==========================================================================================
# To bundle the data for JAGS, we put it into a list structure. JAGS will also have to know how
# many data values there are altogether, so we also compute that total and put it into the 
# list:
Data <- read.csv("z15N50.csv") 
y <- Data$y
Ntotal <- length(y) # Total number of flips
dataList <- list(
  y = y, 
  Ntotal = Ntotal)

# Defining the components within the list function means that this component is named y 
# (i.e., the left side of equal sign) and its value is the value of the variable y that 
# currently exists in R, which is a vector of 0’s and 1’s.

# Note: the names of the components must match variable names in the JAGS model 
# specification.

# ==========================================================================================
# Specify Model -----------------------------------------------------------
# ==========================================================================================
# The model specification begins with the key word model, and whatever follows, inside curly 
# braces, is a textual description of the dependencies between the variables.  JAGS does not 
# execute the model statement as an ordered procedure. Instead, JAGS merely reads in the 
# declarations of the dependencies and then compiles them together to see if they form a 
# coherent structure.
modelString <- "model {
 for (i in 1:Ntotal) {
  y[i] ~ dbern(theta) # Specify likelihood
 }
 theta ~ dbeta(1, 1) # Specify prior
}
"
writeLines(modelString, con = "modelCode.txt")
# ==========================================================================================
# Initialise Chains -------------------------------------------------------
# ==========================================================================================
# Although JAGS can automatically start the MCMC chains at default values, the efficiency of
# the MCMC process can sometimes be improved if we intelligently provide reasonable starting
# values to JAGS. To do this, we have to figure out values for the parameters in the model 
# that are a reasonable description of the data, and might be in the midst of the posterior 
# distribution.

# In general, a useful choice for initial values of the parameters is their maximum likelihood
# estimate (MLE). The MLE is the value of the parameter that maximizes the likelihood 
# function, which is to say, the value of the parameter that maximizes the probability of the
# data. It turns out that for the Bernoulli likelihood function, the MLE is θ = z/N. In other
# words, the value of θ that maximizes θ^z(1 − θ)^(N−z) is θ = z/N.

# For our current application, the data are 0’s and 1’s in the vector y, and therefore z is 
# sum(y) and N is length(y). We will name the ratio thetaInit, and then bundle the initial 
# value of the parameter in a list that will subsequently get sent to JAGS:
thetaInit <- sum(y) / length(y)
initList <- list(theta = thetaInit)
# In initsList, each component must be named as a parameter in the JAGS model specification. 
# Thus, in the list statement above, the component name, theta, refers to theta in the JAGS 
# model specification.

# When there are multiple chains, we can specify different or identical initial values for 
# each chain. The rjags package provides three ways of initializing chains. One way is to
# specify a single initial point for the parameters, as a single named list as in the example
# above, and having all chains start there. A second way is to specify a list of lists, with 
# as many sub-lists as chains, and specifying specific initial values in each sub-list. A 
# third way is to define a function that returns initial values when called. The rjags package
# calls the function as many times as there are chains.

# Starting the chains together at the MLE is not universally recommended. Some practitioners 
# recommend starting different chains at greatly dispersed points in parameter space, so that
# the chains’ eventual convergence may be taken as a sign that burn-in has been properly 
# achieved and that the full parameter space has been explored. In particular, this procedure
# is recommended for interpreting the Gelman-Rubin convergence statistic. In Kruscke's
# experience, however, for the relatively simple models used in this book, appropriate 
# burn-in and convergence is rarely a problem. And, for some complex models, if the chains
# are initialized at randomly dispersed arbitrary points, they might never converge during a
# reasonable finite run, with some chains being orphaned indefinitely (although orphaned 
# chains are usually less of a problem in Stan than in JAGS).

# A compromise approach is to start the chains at random points near the MLE. One way to do 
# this is, for each chain, to resample from the data and compute the MLE for the resampled 
# data. Resampled data will tend to have approximately the same proportion of heads, but 
# sometimes more and sometimes less. In a sense, a bootstrap sampling of the MLE;
initsList <- function() {
  resampledY <- sample(y, replace = TRUE)
  thetaInit <- sum(resampledY) / length(resampledY)
  thetaInit <- .001 + .998 * thetaInit # to nudge the sampling away from the extremes
  return(list(theta = thetaInit))
}
# The third line in the function, above, is a protective measure that keeps the initial value
# of theta in a valid range for JAGS. For example, a prior of beta(θ | 2, 2) has zero density
# at θ = 0 and θ = 1, and JAGS balks at zero prior probabilities, and therefore the chain 
# cannot be started at those extremes.

# To demonstrate how the initsList function works, suppose the data y consist of 75% 1’s:
y <- c(rep(0, 25), rep(1, 75))
# When the initsList function is called, it returns a list with a component named theta that
# has a value near 75%, as you can see in the following sequence of repeated calls:
initsList()

# Redefine y:
y <- Data$y

# ==========================================================================================
# Generate Chains ---------------------------------------------------------
# ==========================================================================================
# Now that we have assembled the data, composed the model specification, and established the
# initial values of the chains, we are ready to have JAGS actually generate the MCMC sample 
# from the posterior distribution.

# There are three steps to this process. The first step gets all the information into JAGS 
# and lets JAGS figure out appropriate samplers for the model. The next step runs the chains 
# for a burn-in period. Finally, the third step runs and records the MCMC sample that we will
# subsequently examine.

# The first step is accomplished using the jags.model function from the rjags package. This 
# function sends the model specification, data list, and initial-value list to JAGS and tells
# JAGS to figure out appropriate MCMC samplers:
jagsModel <- jags.model(file = "modelCode.txt", data = dataList, inits = initsList, 
                        n.chains = 3, n.adapt = 500)
# If you want JAGS to create its own initial values for the chains, simply omit the inits 
# argument entirely. The next two arguments specify the number of chains and the number of 
# steps to take for adapting (or tuning) the samplers.

# The jags.model function returns an object that here has been named jagsModel. The object is
# a list of functions that encapsulate, in JAGS terms, the model and samplers. We will not 
# delve into the jagsModel object. Instead, we merely pass it along to subsequent functions 
# that generate the MCMC sample.

# When executing the jags.model command, JAGS checks the model specification, data list, and 
# initial values, for coherence and consistency. If anything is awry, JAGS will issue an 
# error statement.

# After JAGS has created its model, we tell it to run the chains some number of steps to 
# accomplish burn in. We do this by using the update command from package rjags:
update(jagsModel, n.iter = 500)
# The first argument is the JAGS object previously created by jags.model. The argument 
# n.iter is the number of iterations, or steps, to take in the MCMC chains. For the present
# simple application, only a short burn-in is specified. The update function returns no 
# values, it merely changes the internal state of the jagsModel object. It does not record 
# the sampled parameter values during the updating.

# After burn-in, we tell JAGS to generate MCMC samples that we will actually use to represent
# the posterior distribution. The chains of the parameter values are arranged in a 
# specialized format so that various functions from the coda package can be used to examine
# the chains. Therefore, the function that generates the MCMC samples is called coda.samples 
# in the rjags package. Here is an example of its use:
codaSamples <- coda.samples(jagsModel, variable.names = c("theta"),
                            n.iter = 3334)
# As in the update function, the first argument is the JAGS object previously created by 
# jags.model. The argument n.iter is the number of iterations, or steps, taken by each 
# chain. Here, n.iter is set to 3334, which will yield a total of 10,002 steps because there
# were three chains specified in the jags.model function. Crucially, the variable.names 
# argument specifies which parameters will have their values recorded during the MCMC walk. 
# JAGS will only record the trajectories of parameters that you explicitly tell it to! The 
# variable.names argument must be a vector of character strings. In the present application,
# there is only one parameter, so the vector has only one element.

# The result of the coda.samples function is a coda-formatted object, here named codaSamples, 
# that contains all the sampled parameter values in all the chains. It is a list of matrices. 
# Each component of the list corresponds to a chain, hence in the current example, which has
# three chains, the list has three components. In each component is a matrix, which has rows
# indexed by the step in the chain and has columns corresponding to each parameter.
length(codaSamples)

# ==========================================================================================
# Examine Chains ----------------------------------------------------------
# ==========================================================================================
# When examining the MCMC samples, the first task is to check that the chains appear to be 
# well mixed and suitably representative of the posterior distribution.
source("DBDA2E-utilities.R")


# The trace plot in the upper-left panel shows no signs of orphaned chains. The density plot 
# in the lower-right shows that the three subchains are well super-imposed, which is echoed 
# by the Gelman- Rubin statistic in the lower-left panel being very close to 1.0. The
# autocorrelation plot in the upper-right panel shows essentially zero autocorrelation for
# this simple model, and the effective sample size is essentially as large as the full length
# of the chain.
diagMCMC(codaObject = codaSamples, parName = "theta")

# Visually inspecting the posterior:
plotPost(codaSamples[, "theta"], 
         main = "Theta",
         xlab = bquote(theta))
# The mode indicates the value of the parameter that is most credible, given the data, but
# unfortunately the estimate of the mode from an MCMC sample can be rather unstable because 
# the estimation is based on a smoothing algorithm that can be sensitive to random bumps and
# ripples in the MCMC sample.

plotPost(codaSamples[, "theta"], main = "Theta",
         xlab = bquote(theta),
         cenTend = "median", compVal = .5, ROPE = c(.45, .55), credMass = .9,
         border = "black")
ﬂ# The mode is most meaningful but also unstable in its MCMC approximation. The median is 
# typically the most stable. The compVal argument specifies a “comparison value.” For 
# example, we might want to know whether the estimated bias differs from the “fair” value of
# θ = 0.5. The plot shows the comparison value and annotates it with the percentage of the 
# MCMC sample below or above the comparison value. The credMass argument specifies 
# the mass of the HDI to display. It defaults to 0.95.

# ==========================================================================================
# Example: difference of biases -------------------------------------------
# ==========================================================================================
Data <- read.csv("z6N8z2N7.csv")
y <- Data$y
s <- as.numeric(Data$s) # converts character to discrete integer intervals
if (any(y != 0 & y != 1)) {stop("All y values must be 0 or 1.")} # check y values

# Next, the data are bundled into a list for subsequent shipment to JAGS:
Ntotal <- length(y)
Nsubj <- length(unique(s))
dataList <- list(
  y = y, 
  s = s,
  Ntotal = Ntotal,
  Nsubj = Nsubj
)
# For each subject, we want to estimate their bias, that is, their underlying probability of
# generating an outcome 1, which we will denote θs. We model the data as coming from a 
# Bernoulli distribution. Each subject’s bias, θs, has an independent beta-distributed prior.
# To keep the example consistent with the earlier demonstrations, we set A = 2 and B = 2.
# Having created an outline of the model, we can now express it in JAGS:
modelString <- "model {
 for (i in 1:Ntotal) {
 y[i] ~ dbern(theta[s[i]])
 } 
 for (s in 1:Nsubj) {
 theta[s] ~ dbeta(2, 2)
 }
}
"
writeLines(modelString, con = "modelCode.txt")

# Example of simplified scripts -------------------------------------------
# Load the data
myData = read.csv("z6N8z2N7.csv") # myData is a data frame.
# Load the functions genMCMC, smryMCMC, and plotMCMC: 
source("Jags-Ydich-XnomSsubj-MbernBeta.R")
# Generate the MCMC chain:
mcmcCoda = genMCMC( data=myData , numSavedSteps=10000 )
# Display diagnostics of chain, for specified parameter:
diagMCMC( mcmcCoda , parName="theta[1]" )
# Display numerical summary statistics of chain:
smryMCMC( mcmcCoda , compVal=NULL , compValDiff=0.0 )
# Display graphical posterior information:
plotMCMC( mcmcCoda , data=myData , compVal=NULL , compValDiff=0.0 )

# End file ----------------------------------------------------------------