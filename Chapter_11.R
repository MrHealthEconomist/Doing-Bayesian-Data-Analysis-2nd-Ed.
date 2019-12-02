# ====================================================================================
# Exercise 11.1 -----------------------------------------------------------
# ====================================================================================
# We have a six-sided die, and we want to know whether the probability that the six-dotted 
# face comes up is fair. Thus, we are considering two possible outcomes: six-dots or not 
# six-dots. If the die is fair, the probability of the six-dotted face is 1/6.

# (A) Suppose we roll the die N = 45 times, intending to stop at that number of rolls. 
# Suppose we get 3 six-dot rolls. What is the two-tailed p value?

N <- 45
z <- 3
theta <- 1/6
lowTailZ <- 0:z

sum(choose(N, lowTailZ) * theta^lowTailZ * (1 - theta) ^ (N - lowTailZ))
dbinom(z, N, theta)

sum((lowTailZ / N) * choose(N, lowTailZ) * theta^lowTailZ * (1-theta) ^ (N-lowTailZ))

# ====================================================================================
# Exercise 11.2 -----------------------------------------------------------
# ====================================================================================
# We continue with the scenario of the previous exercise: A dichotomous outcome, with 
# N = 45 and z = 3.

# (A) If the intention is to stop when N = 45, what is the 95% CI?

# Hints: Try this continuation of the R script from the previous exercise:
for (theta in seq(.170, .19, .001)) {
 show(c(
  theta, 2 * sum(choose(N, lowTailZ) * theta ^ lowTailZ * 
                  (1 - theta) ^ (N - lowTailZ))
 ))
}

highTailZ <- z:N

for (theta in seq(.005, .02, .001)) {
 show(c(
  theta, 2 * sum(choose(N, highTailZ) * theta ^ highTailZ * 
                  (1 - theta) ^ (N - highTailZ))
 ))
}


# (B) If the intention is to stop when z = 3, what is the 95% CI? Is the CI the same as 
# for stopping when N = 45?

# Hint: Modify the R script of the previous part for use with stopping at z, like the 
# second part of the previous exercise.

for (theta in seq(.170, .19, .001)) {
 show(c(
  theta, 2 * sum((lowTailZ / N) * choose(N, lowTailZ) * 
                  theta ^ lowTailZ * (1 - theta) ^ (N - lowTailZ))
 ))
}

highTailZ <- z:N

for (theta in seq(.005, .02, .001)) {
 show(c(
  theta, 2 * sum((highTailZ / N) * choose(N, highTailZ) * 
                  theta ^ highTailZ * (1 - theta) ^ (N - highTailZ))
 ))
}

# ====================================================================================
# Exercise 11.3 -----------------------------------------------------------
# ====================================================================================
# We continue with the scenario of the previous exercises: A dichotomous outcome, with 
# N = 45 and z = 3. Suppose that the die-roller of the previous exercises stopped rolling 
# because time expired at 6 min. For simplicity, suppose that during a 6-min interval, the
# roller could have rolled N = 40, or N = 41, or N = 42, through N = 50, with equal 
# probability. What is the p value for the observed outcome? Is it the same p value as 
# when assuming fixed N or fixed z?

# Hints: We need to compute the p value for each possible N, and then average them 
# according to the probability they would happen. For each N, the low tail consists of 
# outcomes that are a proportion less than or equal to the observed z / N = 3 / 45. 
# Examine the follow R script. Explain exactly what it does and interpret its output.
N <- 45
z <- 3
theta <- 1 / 6

# Specify all the possible N values:
Npossible <- 40:50

# Specify probability of each N (here all equal):
Nprob <- rep(1, length(Npossible))
Nprob <- Nprob / sum(Nprob)

# # For each possible N, compute p value, and compute the weighted total p:
totalP <- 0

# loop over each value of N possible samples:
for (i in 1:length(Npossible)) {
 thisN <- Npossible[i]
 # For this N, determine the max z that is in the low tail:
 thisZ <- max((0:thisN)[(0:thisN) / thisN <= z / N])
 # determine the lowtail value for z outcome:
 lowTailZ <- 0:thisZ
 # determine the probability for N samples:
 thisP <- 2 * sum(choose(thisN, lowTailZ) * theta ^ lowTailZ * 
                   (1 - theta) ^ (thisN - lowTailZ))
 # display outcomes for each sample outcome and the associated probability:
 show(c(thisN, thisP))
}

# Display the total probability:
show(totalP)

# End file ----------------------------------------------------------------