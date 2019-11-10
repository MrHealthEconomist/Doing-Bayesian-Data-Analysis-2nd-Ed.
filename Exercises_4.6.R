# ==============================================================================================
# Generating the RunningProportion Script ---------------------------------
# ==============================================================================================
set.seed(47405)

N <- 500 # Specify the total number of flips, denoted N.
pHeads <- .5 # Specify underlying probability of heads.

# Generate a random sample of N flips (heads=1, tails=0):
flipSequence <- sample(x = c(0, 1), prob = c(1 - pHeads, pHeads), 
                       size = N, replace = TRUE)

# Compute the running proportion of heads:
r <- cumsum(flipSequence) # Cumulative Sum: Number of heads at each step
n <- 1:N # Number of flips at each step
runProp <- r / n # Component by component division

# Graph the running proportion:
plot(n, runProp, type = "o", log = "x",
     col = "skyblue",
     xlim = c(1, N), ylim = c(0, 1),
     main = "Running Proportion of Heads",
     cex.main = 1.5,
     xlab = "Flip Number", ylab = "Proportion Heads", 
     cex.lab = 1.5)
# Plot a dotted horizontal line reference:
abline(h = pHeads, lty = "dotted",
       col = "red")

# Display the beginning of the flip sequence:
flipLetters <- paste(c("T", "H")[flipSequence[1:10] + 1], collapse = "")
displayString <- paste0("Flip Sequence =", flipLetters, "...")
text(N, .8, paste("End Proportion =", runProp[N]), adj = c(1, .5), cex = 1.3)

# ==============================================================================================
# 4.1 ---------------------------------------------------------------------
# ==============================================================================================
show(HairEyeColor) # Show data

EyeHairFreq <- apply(HairEyeColor, c("Eye","Hair"), sum) # Sum across sex 
# Above applies function sum to rows and columns for each variable and categorises them for eye 
# and hair colour in the variable EyeHairFreq as conditional on the other, i.e. Brown eyes and
# black hair totals 68 events...

EyeHairProp <- EyeHairFreq / sum(EyeHairFreq) # joint proportions, Table 4.1 
# Above divides the Frequency of each event by the sum of events to obtain probabilities.
show(round(EyeHairProp , 2))
# displays the results as rounded to 2 decimal places.

HairFreq <- apply(HairEyeColor, c("Hair"), sum) # Sum across sex and eye 
# Again, as in EyeFreq Colour, bu this time only sums the columns, i.e. total number of events
# just for a specific hair colour, independent of eye colour.

HairProp <- HairFreq / sum( HairFreq ) # marginal proportions, Table 4.1
show(round(HairProp, 2))
# ...same but...

EyeFreq <- apply(HairEyeColor ,c("Eye"), sum) # Sum across sex and eye 
# Again...but independent of hair colour.

EyeProp <- EyeFreq / sum(EyeFreq) # marginal proportions, Table 4.1
# ...same but...
show(round(EyeProp, 2))
# ...same but...

condPeyesBlue <- EyeHairProp["Blue", ] / EyeProp["Blue"] # conditional prob, Table 4.2
# Conditional probabilities of hair colour given blue eye colour:
condPeyesBlue

condPeyeBrown <- EyeHairProp["Brown", ] / EyeProp["Brown"]
# Conditional probabilities of hair colour given brown eye colour:
condPeyeBrown

condPhairBrown <- EyeHairProp[, "Brown"] / HairProp["Brown"]
# Conditional probabilities of hair colour given brown hair colour:
condPhairBrown

# ==============================================================================================
# 4.2 ---------------------------------------------------------------------
# ==============================================================================================
# Modify the coin flipping program in Section 4.5 RunningProportion.R to simulate a biased coin 
# that has p(H) = 0.8. Change the height of the reference line in the plot to match the new
# p(H).
set.seed(47405)

N <- 500 # Specify the total number of flips, denoted N.
pHeads <- .8 # Specify underlying probability of heads.

# Generate a random sample of N flips (heads=1, tails=0):
flipSequence <- sample(x = c(0, 1), prob = c(1 - pHeads, pHeads), 
                       size = N, replace = TRUE)

# Compute the running proportion of heads:
r <- cumsum(flipSequence) # Cumulative Sum: Number of heads at each step
n <- 1:N # Number of flips at each step
runProp <- r / n # Component by component division

# Graph the running proportion:
plot(n, runProp, type = "o", log = "x",
     col = "skyblue",
     xlim = c(1, N), ylim = c(0, 1),
     main = "Running Proportion of Heads",
     cex.main = 1.5,
     xlab = "Flip Number", ylab = "Proportion Heads", 
     cex.lab = 1.5)
# Plot a dotted horizontal line reference:
abline(h = pHeads, lty = "dotted",
       col = "red")

# Display the beginning of the flip sequence:
flipLetters <- paste(c("T", "H")[flipSequence[1:10] + 1], collapse = "")
displayString <- paste0("Flip Sequence =", flipLetters, "...")
text(N, .8, paste("End Proportion =", runProp[N]), adj = c(1, .5), cex = 1.3)

# Straightforward stuff. Change probability proportion, sample prbability changes to .2 and .8, 
# instead of .5 and .5.

# ==============================================================================================
# 4.3 ---------------------------------------------------------------------
# ==============================================================================================
# In a pinochle deck, there are 48 cards. There are six values: 9, 10, Jack, Queen, King, Ace. 
# There are two copies of each value in each of the standard four suits: hearts, diamonds, clubs, 
# spades.

# What is the probability of getting a 10?
N <- 12 * 4 # Total number of cards, i.e. the total values multiplied by the total suites
pTen <- (1/6 * 4/4) # Probability of 10, as each suite contains two tens; would be same as 2/12
pTen # Probability of drawing a 10 from a shuffled pinochle deck

# What is the probability of getting a 10 or Jack?
pTen_Jack <- (2/6 * 4/4)
pTen_Jack

# ==============================================================================================
# 4.4 ---------------------------------------------------------------------
# ==============================================================================================
set.seed(47405)
# A) Adapt the program IntegralOfDensity.R to plot this density function and approximate its 
# integral. Comment your code. Be careful to consider values of x only in the interval [0, 1]. 
# Hint: You can omit the first couple of lines regarding meanval and sdval, because those 
# parameter values pertain only to the normal distribution. Then set xlow = 0 and xhigh = 1, and
# set dx to some small value.

# Graph of normal probability density function, with comb of intervals.
xlow <- 0 # Specify low end of x-axis.
xhigh <- 1 # Specify high end of x-axis.
dx <- 0.01 # Specify interval width on x-axis

# B) Derive the exact integral using calculus:
integral_calculation <- 6*((1/3*1^3 - 1/4*1^4) - (1/3*0^3 - 1/4*0^4))
integral_calculation

# C) Does this function satisfy equation 4.3:
px <- 1 / dx
px
# Proof
100 * 0.01 # Sum multiplied by dx

# D) From inspecting the graph, what is the maximal value of p(x)?
# Calculate normal probability density:
x <- seq(from = xlow, to = xhigh, by = dx)
# p(x) = 1 / sd * sqrt(2 * pi) * exp(-1/2 * (x - mean / sd)^2)
y <- (1 / (.1 * sqrt(2 * pi))) * exp((-.5) * ((x - integral_calculation) / .1) ^ 2)

plot(x, y, type = "h", lwd = 1, cex.axis = 1.5,
     xlab = "x", ylab = "p(x)", cex.lab = 1.5,
     main = "Normal Probability Density", cex.main = 1.5)
lines(x, y)
# ...maxarg_p(x) = 4

# ==============================================================================================
# 4.5 ---------------------------------------------------------------------
# ==============================================================================================
# Adapt the code from IntegralOfDensity.R to determine (approximately) the probability mass 
# under the normal curve from x = μ−σ to x = μ+σ. Hint: Just change xlow and 
# xhigh appropriately, and change the text location so that the area still appears within the 
# plot.
xlow <- .5 - .1
xhigh <- .5 + .1

x <- seq(from = xlow, to = xhigh, by = dx)
y <- (1 / (.1 * sqrt(2 * pi))) * exp((-.5) * ((x - integral_calculation) / .1) ^ 2)

plot(x, y, type = "h", lwd = 1, 
     xlab = "x", ylab = "p(x)", cex.lab = 1.5,
     main = "Normal Probability Density Distribution", cex.main = 1.5)
lines(x, y)

area <- sum(dx * y)
area
# End file ----------------------------------------------------------------