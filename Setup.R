load("crime.RData")

# Remove the non predictor columns
X = x[, -c(c(1:5), 128)]

# Choose from the predictor variables those which are socio-economic
socioEcon = X[, c(1:96)]

# Choose from the predictor variables those which are police-based
police = X[, c(97:122)]

# Create a vector for violent crime per population
y = x[, 128]

# Randomly divides set into training and test sets
#   pct     the percentage (on [0.0, 1.0]) of the set to use for training
#   data    the data.frame of predictor variables
#   target  the numeric (vector) of target variables
# Number of rows in data should match length of target and should be greater 
#   than one
# Returns list with the following elements:
#   trainData   the pct percentage of the data set for training models
#   testData    the 1-pct percentage of the data set for testing models
#   trainTarget the pct percentage of the target set for training models
#   testTarget  the 1-pct percentage of the target set for testing models
SplitSet = function(pct, data, target) {
  numRows = ceiling(pct * nrow(data))
  trainRows = sample(c(1:nrow(data)), numRows)
  trainData = data[trainRows, ]
  testData = data[-trainRows, ]
  trainTarget = target[trainRows]
  testTarget = target[-trainRows]
  return(list("trainData" = trainData, "testData" = testData, "trainTarget" = trainTarget, "testTarget" = testTarget))
}
