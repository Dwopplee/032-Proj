rm(list=ls(all=TRUE))

source('Setup.R')

# TODO

# Covariance give information about linear relationship between X and Y
#   Can use this to help with determining which variables are dependent upon each other linearly

covariance = function() {
  # cor for correlation, cov for covariance
  covariance = cor(X)
  print(covariance)
}

covariance()

# If line of best fit has no slope, variable has no (linear) effect on the target