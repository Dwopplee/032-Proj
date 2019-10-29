load("crime.RData")

# Remove the non predictor columns
X = x[, -c(c(1:5), 128)]

# Choose from the predictor variables those which are socio-economic
socioEcon = X[, c(1:96)]

# Choose from the predictor variables those which are police-based
police = X[, c(97:122)]

# Create a vector for violent crime per population
y = x[, 128]

# Note: many functions serve primarily to allow me to avoid thinking

# Randomly divides set into training and test sets
#   pct     the percentage (on [0.0, 1.0]) of the set to use for training
#   data    the data.frame of predictor variables
#   target  the numeric (vector) of target variables
# Number of rows in data should match length of target and should be greater
# than one
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

# Simple function for taking mean absolute error
#   model   the model to test
#   data    the data used to test model
#   target  the target dataset used to test model
AbsError = function(model, data, target) {
  error = mean(abs(predict(model, newdata = data) - target))
  return(error)
}

# Simple function for taking mean squared error
# Favors tests in which large errors are particularly bad
#   model   the model to test
#   data    the data used to test model
#   target  the target dataset used to test model
SqdError = function(model, data, target) {
  error = mean((predict(model, newdata = data) - target)^2)
  return(error)
}

# TODO: Solid like 30% sure this function works for maxPow and minPow
# TODO: May be useful to generalize for multiple parameters
# Creates a model with positive, negative, or both types of exponenets
#   predictor   the column name of the column in trainData to use
#   trainData   the data set from which to draw training data
#   trainTarget the target variable vector from which to draw training data
#   maxPow      the maximum power that should be included in the model
#   minPow      the minimum power that should be included in the model
ModelExp = function(predictor, trainData, trainTarget, maxPow = -1, minPow = 1) {
  # To avoid dividing by zero
  if (minPow < 0) {
    trainData[trainData == 0] = NA
  }
  
  pows = paste("I(", predictor, "**", c(minPow:maxPow), ")", sep='')
  f = as.formula(paste("trainTarget ~ ", paste(pows, collapse = "+")))
  model = lm(f, data = trainData)
  return(model)
}

ExpFormula = function(predictor, maxPow) {
  pows = paste("I(", predictor, "**", c(1:maxPow), ")", sep='')
  return(paste(pows, collapse = "+"))
}

# TODO: document
# Stole this lol
Mode = function(dataSet) {
  uniqData <- unique(dataSet)
  return(uniqData[which.max(tabulate(match(dataSet, uniqData)))])
}

# Increases maximum exponent from 0 until error increases
#   predictor   a string containing the name of column to model
#   trainData   data used for modeling; must contain predictor
#   testData    data used for testing; must contain predictor
#   trainTarget data of target variable used for modeling
#   testTarget  data of target variable used for testing
# Returns the highest exponent reached before error increased
# TODO: This function outputs slightly different things based on what the 
#       training and test sets are
#           May want to run this multiple times, if possible.
#           We probably want to favor simpler models, which this innately does
#             Maybe use mode?
PosExps = function(predictor, trainData, testData, trainTarget, testTarget) {
  naIndex = which(is.na(testData[predictor]))
  if (length(naIndex) > 0) {
    testData = testData[-c(naIndex), ]
    testTarget = testTarget[-c(naIndex)]
  }
  
  # Absolute worst a model should be
  baseModel = lm(trainTarget ~ 1, data = trainData)
  baseError = AbsError(baseModel, testData, testTarget)
  
  oldError = Inf
  newError = baseError
  maxPow = 0
  # May be interesting to test this with MSE rather than MAE
  while (newError < oldError) {
    maxPow = maxPow + 1
    
    model = ModelExp(predictor, trainData, trainTarget, maxPow = maxPow)
    
    oldError = newError
    newError = AbsError(model, testData, testTarget)
  }
  
  return(maxPow - 1)
}

# This function is a pain to use; we can just rely on taylor expansions
# and use only positive exponents
# Decreases minimum exponent from 0 until error increases
#   predictor   a string containing the name of column to model
#   trainData   data used for modeling; must contain predictor
#   testData    data used for testing; must contain predictor
#   trainTarget data of target variable used for modeling
#   testTarget  data of target variable used for testing
# Returns the lowest exponent reached before error increased
# TODO: This function outputs slightly different things based on what the 
#       training and test sets are
#           May want to run this multiple times, if possible.
#           We probably want to favor simpler models, which this innately does
#             Maybe use mode?
NegExps = function(predictor, trainData, testData, trainTarget, testTarget) {
  # Absolute worst a model should be
  baseModel = lm(trainTarget ~ 1, data = trainData)
  baseError = AbsError(baseModel, testData, testTarget)
  
  oldError = Inf
  newError = baseError
  minPow = 0
  # May be interesting to test this with MSE rather than MAE
  while (newError < oldError) {
    minPow = minPow - 1
    
    model = ModelExp(predictor, trainData, trainTarget, minPow = minPow)
    
    oldError = newError
    newError = AbsError(model, testData, testTarget)
  }
  
  return(minPow + 1)
}
