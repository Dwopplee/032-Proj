rm(list=ls(all=TRUE))

source('Setup.R')

# TODO

# Covariance give information about linear relationship between X and Y
#   Can use this to help with determining which variables are dependent upon each other linearly

covariance = function() {
  # cor for correlation, cov for covariance
  covariance = cor(X)
  print(which(covariance == max(covariance), arr.ind = TRUE))
}

covariance()

# If line of best fit has no slope, variable has no (linear) effect on the target

# Note: coef can extract model coeffs. Can use this and which.min to remove least effective predictors
