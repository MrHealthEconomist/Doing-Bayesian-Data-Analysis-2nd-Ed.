# ==========================================================================================
# Exercise 7.1 ------------------------------------------------------------
# ==========================================================================================
# Open the program named BernMetrop.R from the files accompanying this book:
source("BernMetrop.R")
 # Shows that the best performing ESS is with a SD proposal of .2

# ==========================================================================================
# Exercise 7.2 ------------------------------------------------------------
# ==========================================================================================
# (A) Before each line, add a comment that explains what the line does. Include the commented 
# code in your write-up:

# Opens the saved graph above:
openGraph(height = 7, width = 3.5)
# Defines a window layout, much like par(mfrow = c(x, y)):
layout(matrix(1:2, nrow = 2))
# Computes and plots Autocorrelation Function for a given k lag = 30:
acf(acceptedTraj, lag.max = 30, col = "skyblue", lwd = 3)
# Defines length of chain:
Len <- length(acceptedTraj)
# Defines number of Lags:
Lag <- 10
# Defines acceptance probabilties for proposed steps:
trajHead <- acceptedTraj[1:(Len - Lag)]
trajTail <- acceptedTraj[(1 + Lag):Len]
# Plots output of chain versus lag values:
plot(trajHead, trajTail, pch = ".", col = "skyblue",
     main = bquote(list("Prpsl.SD" == .(proposalSD),
                        lag == .(Lag),
                        cor == .(round(cor(trajHead, trajTail), 3)))))
abline(0, 1)

# (B) Repeat the previous exercise, with the lines above appended to the script. Include the 
# resulting new graphs in your write-up. For each run, verify that the height of the ACF bar
# at the specified lag matches the correlation in the scatterplot.

# The resulting height of the ACF bar at the specified lag of k = 10, mathces the correlation
# on the scatterplot.

# (C) When the proposal distribution has SD=2, why does the scatter plot have a dense line of 
# points on the diagonal?

# As stated in Kruschke (2014), this is because of all the steps in the trajectory for which 
# the value at step ith is the same as the value at step i + 10. For most steps, the value of 
# the parameter does not change, and there are many steps for which the values has not changed
# even after 10 proposals.

# ==========================================================================================
# Exercise 7.3 ------------------------------------------------------------
# ==========================================================================================
# In this exercise, you will see that the Metropolis algorithm operates with multimodal 
# distributions.

# (A) Consider a prior distribution on coin bias that puts most credibility at 0.0, 0.5, 
# and 1.0, which we will formulate as p(θ) = (cos(4πθ) + 1)2/1.5.

# (B) Make a plot of the prior.
theta <- seq(0, 1, length.out = 501)
plot(theta, (cos(4 * pi * theta) + 1) ^ 2 / 1.5,
     col = "skyblue", lwd = 3)

# (C) In the script BernMetrop.R, find the function definition that specifies the prior 
# distribution. Inside that function definition, comment out the line that assigns a beta
# density to pTheta, and instead put in a trimodal prior like this:
pTheta = (cos(4*pi*theta)+1)ˆ2/1.5

# To have the Metropolis algorithm explore the prior, we give it empty data. Find the line in
# the script that specifies the data and set myData = c(). Run the script, using a proposal 
# SD = 0.2. Include the graphical output in your write-up. Does the histogram of the 
# trajectory look like the graph of the previous part of the exercise?

# The histogram of the trajectory does indeed look like the trimodal prior distribution 
# graphed in the previous part. Thus, the metropolis algorithm is able to sample 
# representatively from multimodal distributions, at least in this case. (The HDI as marked 
# is irrelevant and potentially misleading because its algorithm assumes a unimodal 
# distribution)

# (D) Repeat the previous part but now with myData = c(0,1,1). Does the posterior distribution
# make sense? Explain why.

# The posterior distribution makes sense because it is a compromise between the trimodal 
# prior and the gently peaked likelihood function of the small data set. The likelihood 
# function peaks at 0.667 (i.e., 2/3), but is very broad, gently descending to 0 height at 
# theta = 0 and theta = 1. Therefore the prior peaks at theta = 0.5 and near theta = 1.0 are
# “allowed” by the data, but the prior peak near theta = 0.0 is relatively unlikely given the
# data. (The HDI as marked is irrelevant and potentially misleading because its algorithm 
# assumes a unimodal distribution).

# (E) Repeat the previous part but now with proposal SD = 0.02. Does the posterior 
# distribution make sense? Explain why not; what has gone wrong? If we did not know from the 
# previous part that this output was unrepresentative of the true posterior, how could we try 
# to check?

# As stated in Kruschke (2014), the MCMC posterior shows a single peak near theta = 0, which
# is strange because the data have 2/3 1’s, and the prior allows theta values near 0.5 and 
# near 1.0. Why does this anomalous result occur? Because the arbitrary initial value of the
# chain is very near theta = 0, and the very small proposal SD has a difficult time squeezing
# under the very low ceiling between modes of the prior. With a long enough wait, there might
# be a series of proposals that just happen to make it under the low ceiling, but this would
# take a very, very long time.

# If we did not know from other considerations that this was an anomalous result, we could 
# start several chains at different starting positions and check that they all converged to 
# the same region of parameter space.

# (F) Repeat the previous part but now with the initial position at 0.99: 
# trajectory[1] = 0.99. In conjunction with the previous part, what does this result tell us?

# As stated in Kruschke (2014), when the trajectory arbitrarily starts at theta = 0.99, and 
# the proposal SD is 0.02, the MCMC chain gets stuck in the prior mode near theta = 1. If we
# started chains at several different values of theta, we would notice that they do not 
# converge to the same region of parameter space, which would be a sign that the results are 
# not fully representative of the posterior, and we would need to adjust the MCMC sampler.
# Basically, the sampler is not exploring the posterior sample space effectively and 
# efficiently!

# End file ----------------------------------------------------------------