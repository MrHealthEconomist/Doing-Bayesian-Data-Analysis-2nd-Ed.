# ================================================================================
# Libraries ---------------------------------------------------------------
# ================================================================================
library(R2jags)
library(parallel)

options(mc.cores = detectCores())

# ================================================================================
# Testing Differences between Groups --------------------------------------
# ================================================================================
# The data structure has one row per subject, with the number of trials (words) for 
# subject s denoted nTrlOfSubj[s], the number correctly recalled for subject s denoted 
# nCorrOfSubj[s], and the condition of subject s denoted CondOfSubj[s]. The model 
# specification begins with saying that each subject has an individual ability theta[s] 
# from a condition-specific beta distribution:
modelString <- model {
 for (i in 1:nSubj) {
  nCorrOfSubj[s] ~ dbinom(theta[s], nTrrlOfSubj[s])
 theta[s] ~ dbinom(aBeta[CondOfSubj[s]], bBeta[CondOfSubj[s]]) 
 }
 # The shape parameters of the beta distribution are then re-written in terms of the mode 
 # and concentration. Model 1 uses condition-specific omega[j], while Model 2 uses the 
 # same omega0 for all conditions. The JAGS function equals(mdlIdx,...) is used to select 
 # the appropriate model for index mdlIdx:
 for (j in 1:nCond) {
  # # Use omega[j] for model index 1, omega0 for model index 2:
  aBeta[j] <- (equals(mdlIdx, 1) * omega[j] + 
                equals(mdlIdx, 2) * omega0) * (kappa[j] - 2) + 1
  bBeta[j] <- (1 - (equals(mdlIdx, 1) * omega[j] 
                    + equals(mdlIdx, 2) * omega0)) * (kappa[j] - 2) + 1
  omega[j] ~ dbinom(a0[mdlIdx, b0[mdlIdx]])
 } 
 # The priors on the concentration parameters are then specified:
 for (j in 1:nCond) {
  kappa[j] <- kappaMinusTwo[j] + 2
  
 }
}
