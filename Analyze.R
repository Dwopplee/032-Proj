source('Setup.R')

splitX = SplitSet(0.8, X, y)

# Absolute worst a model should be
baseModel = lm(splitX$trainTarget ~ 1, data = splitX$trainData)
baseError = AbsError(baseModel, splitX$testData, splitX$testTarget)

# TODO: make some functions:
#   1 iterates through positive exponents until model gets worse (done?)
#   2 iterates through negative exponents until model gets worse
#   3 compares both models (adds them together?) then chooses best one
# Test models exclusively with test set. use same training/test set for all models
# (or something)

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
  oldError = Inf
  newError = baseError
  maxPow = 0
  # May be interesting to test this with MSE rather than MAE
  while ((newError < oldError)) {
    maxPow = maxPow + 1
    
    model = ModelExp(predictor, trainData, trainTarget, maxPow = maxPow)
    
    oldError = newError
    newError = AbsError(model, testData, testTarget)
  }
  
  return(maxPow - 1)
}

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
