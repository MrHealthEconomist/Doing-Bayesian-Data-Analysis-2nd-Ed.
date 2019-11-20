# ==========================================================================================
# Load functions and libraries --------------------------------------------
# ==========================================================================================
source("DBDA2E-utilities.R")
source("BernBeta.R")

# Specify the prior -------------------------------------------------------
t <- .75 # Specify prior mode
n <- 25 # Specify the effective prior sample size
a <- t * (n - 2) + 1 # Convert to beta shape parameter a
b <- (1 - t) * (n - 2) + 1 # Convert to beta shape parameter b
Prior <- c(a, b)

# Specify the data --------------------------------------------------------
N <- 20 # Total flips
z <- 17 # Total number of heads

Data <- c(rep(0, N - z), rep(1, z)) # Convert N and z into vector of 0’s and 1’s

Posterior <- BernBeta(priorBetaAB = Prior, Data = Data, plotType = "Bars",
                      showCentTend = "Mode", showHDI = TRUE, showpD = FALSE)

# ==========================================================================================
# Exercise 6.1 ------------------------------------------------------------
# ==========================================================================================
# For this exercise, use the R function BernBeta.R. Notice that the function returns the 
# posterior beta values each time it is called, so you can use the returned values as the 
# prior values for the next function call. Tails = 0, Heads = 1.

# (A) Start with a prior distribution that expresses some uncertainty that a coin is fair: 
# beta(θ|4,4). Flip the coin once; suppose we get a head. What is the posterior distribution?
post <- BernBeta(priorBetaAB = c(4, 4), Data = c(1))

# It is a posterior beta(theta | 5, 4)

# Use the posterior from the previous flip as the prior for the next flip. Suppose we flip 
# again and get a head. Now what is the new posterior?
postNext <- BernBeta(post, c(1))

# It is a posterior beta(theta | 6, 4)

# (C) Using that posterior as the prior for the next flip, flip a third time and get a tail. 
# Now what is the new posterior? 

postTail <- BernBeta(postNext, c(0))

# It is a posterior beta(theta | 6, 5)

# (D) Do the same three updates but in the order T, H, H instead of H, H, T. Is the final 
# posterior distribution the same for both orderings of the flip results?

postUpdated <- BernBeta(priorBetaAB = c(4, 4), Data = c(0, 1, 1))

#  It is the same due to the independence of the data. They are disjointed and independent.

# ==========================================================================================
# Execerise 6.2 -----------------------------------------------------------
# ==========================================================================================
# Suppose an election is approaching, and you are interested in knowing whether the general 
# population prefers candidate A or candidate B. There is a just-published poll in the 
# newspaper, which states that of 100 randomly sampled people, 58 preferred candidate A and 
# the remainder preferred candidate B.

N <- 100
A <- 58
B <- N - A

# For interest and practice sake of the previously described beta parameter formulae:
k <- A + B
mu <- A / (A + B)

a <- mu * (A + B)
a

b <- (1 - mu) * k
b

w <- (a - 1) / (a + b - 2)
w

aMode <- w * (k - 2) + 1
aMode

bMode <- (1 - w) * (k - 2) + 1
bMode

standdev <- sqrt((mu* (1 - mu)) / (a + b + 1))
standdev

# (A) Suppose that before the newspaper poll, your prior belief was a uniform distribution.
# What is the 95% HDI on your beliefs after learning of the newspaper poll results?
postElection <- BernBeta(priorBetaAB = c(1, 1), Data = c(rep(1, A), rep(0, B)),
                         showHDI = TRUE, showCentTend = TRUE)
# The 95% HDI is 0.483 to 0.673 for probability of candidate A.

# (B) You want to conduct a follow-up poll to narrow down your estimate of the population’s 
# preference. In your follow-up poll, you randomly sample 100 other people and find that 57 
# prefer candidate A and the remainder prefer candidate B. Assuming that peoples’ opinions 
# have not changed between polls, what is the 95% HDI on the posterior?
postElectionUpdate <- BernBeta(priorBetaAB = postElection, 
                               Data = c(rep(1, 57), rep(0, 100-57)),
                               showHDI = TRUE, showCentTend = TRUE)
# The 95% HDI is now narrower, from 0.506 to 0.642, for probability of candidate A.

# ==========================================================================================
# Exercise 6.3 ------------------------------------------------------------
# ==========================================================================================
# Suppose you train people in a simple learning experiment, as follows. When people see the 
# two words, “radio” and “ocean,” on the computer screen, they should press the F key on the 
# computer keyboard. They see several repetitions and learn the response well. Then you 
# introduce another correspondence for them to learn: Whenever the words “radio” and 
# “mountain” appear, they should press the J key on the computer keyboard. You keep training
# them until they know both correspondences well. Now you probe what they’ve learned by 
# asking them about two novel test items. For the first test, you show them the word “radio” 
# by itself and instruct them to make the best response (F or J) based on what they learned 
# before.

# For the second test, you show them the two words “ocean” and “mountain” and ask them to 
# make the best response. You do this procedure with 50 people. Your data show that for 
# “radio” by itself, 40 people chose F and 10 chose J. For the word combination “ocean” and
# “mountain,” 15 chose F and 35 chose J. Are people biased toward F or toward J for either of
# the two probe types? To answer this question, assume a uniform prior, and use a 95% HDI to
# decide which biases can be declared to be credible.

# Response F is y = 1, response J is y = 0.

# Radio -------------------------------------------------------------------
postRadio <- BernBeta(priorBetaAB = c(1, 1), Data = c(rep(1, 40), rep(0, 10)),
                      showCentTend = TRUE, showHDI = TRUE)
# The posterior indicates a rejection of the null value of theta = .5 because the 95% most 
# credible values are all not practically equivalent to theta = .5.

# Ocean -------------------------------------------------------------------
postOcean <- BernBeta(priorBetaAB = c(1, 1), Data = c(rep(1, 15), rep(0, 35)),
                      showCentTend = TRUE, showHDI = TRUE)
# The posterior 95% HDI goes from 0.187 to 0.433, which excludes any reasonable ROPE around
# theta = .5. In other words, we reject the null value of theta = .5 because the 95% most 
# credible values are all not practically equivalent to theta = .5.

# ==========================================================================================
# Exercise 6.4 ------------------------------------------------------------
# ==========================================================================================
# Suppose we have a coin that we know comes from a magic-trick store, and therefore we 
# believe that the coin is strongly biased either usually to come up heads or usually to 
# come up tails, but we don’t know which. Express this belief as a beta prior.

post <- BernBeta(priorBetaAB = c(1, 1) / 100, Data = c(rep(1, 4), rep(0, 1)),
                 showHDI = TRUE, showCentTend = TRUE)
# The posterior distribution is dbeta(theta|4.01,1.01) which, as shown above, has its mode 
# essentially at 1.0, not at 0.8.

# ==========================================================================================
# Exercise 6.5 ------------------------------------------------------------
# ==========================================================================================
# (A) Suppose you have a coin that you know is minted by the government and has not been 
# tampered with. Therefore you have a strong prior belief that the coin is fair. You flip 
# the coin 10 times and get 9 heads. What is your predicted probability of heads for the 
# 11th flip?

# For the strong prior belief, let’s suppose it’s equivalent to previously seeing 2,000 
# flips of the coin and observing 50% (i.e., 1000) heads. So the prior is
# dbeta(theta | 1000, 1000).

postFlips <- BernBeta(priorBetaAB = c(1, 1) * 1000, Data = c(rep(1, 9), rep(0, 1)),
                      showCentTend = "Mean", showHDI = TRUE)
# The predicted probability of heads on the next flip is the mean of the posterior 
# distribution, which in this case is 0.502 (as displayed in the graph). Thus, even though 
# the new data showed 90% heads, the strong prior keeps the posterior prediction very nearly 
# 50%.

# (B) Now you have a different coin, this one made of some strange material and marked 
# (in fine print) “Patent Pending, International Magic, Inc.” You flip the coin 10 times and
# get 9 heads. What is your predicted probability of heads for the 11th flip?

# Kruschke suggests using the dbeta(theta|0.01,0.01) prior from Exercise 6.4, to express a 
# strong prior belief that the coin is strongly heads biased or strongly tails biased.
postFlips <- BernBeta(priorBetaAB = c(1, 1) / 100, Data = c(rep(1, 9), rep(0, 1)),
                      showCentTend = "Mean", showHDI = TRUE)
# The predicted probability of heads on the next flip is the mean of the posterior 
# distribution, which is 0.899 (as displayed in the graph). This is due to the beta 
# prior with strong categorical outcomes of either or, rather than fluctauting probabilities.

# End file ----------------------------------------------------------------