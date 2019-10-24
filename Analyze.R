source('Setup.R')

splitX = SplitSet(0.8, X, y)
XTrain = splitX$trainData
XTest = splitX$testData
yTrain = splitX$trainTarget
yTest = splitX$testTarget


# Absolute worst a model should be
baseModel = lm(yTrain ~ 1, data = XTrain)
baseError = AbsError(baseModel, XTest, yTest)

# TODO: make some functions:
#   1 iterates through positive exponents until model gets worse (done?)
#   2 iterates through negative exponents until model gets worse
#   3 compares both models (adds them together?) then chooses best one
# Test models exclusively with test set. use same training/test set for all models
# (or something)

# Increases maximum exponent until error increases
#   predictor   a string containing the name of column to model
#   trainData   data used for modeling; must contain predictor
#   testData    data used for testing; must contain predictor
#   trainTarget data of target variable used for modeling
#   testTarget  data of target variable used for testing
# Returns the highest exponent reached before error increased
# TODO: This function outputs slightly different things based on what the training and test sets are
#           May want to run this multiple times, if possible. Maybe use median value?
posExps = function(predictor, trainData, testData, trainTarget, testTarget) {
  oldError = Inf
  newError = baseError
  maxPow = 0
  # `> 0.01` is there as a threshold to reduce variablity
  # Also means that we favor simpler models over models with slightly less error
  # May be interesting to test this with MSE rather than MAE; would have to change threshold
  while ((newError < oldError) && (abs(oldError - newError) > 0.01)) {
    maxPow = maxPow + 1
    
    pows = paste("I(", predictor, "**", c(1:maxPow), ")", sep='')
    f = as.formula(paste("trainTarget ~ ", paste(pows, collapse = "+")))
    model = lm(f, data = trainData)
    
    oldError = newError
    newError = AbsError(model, testData, testTarget)
  }
  
  return(maxPow - 1)
}

# TODO: document
# TODO: Fix case where we divide by zero (I think that's the problem):
# ```Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : 
#       NA/NaN/Inf in 'x'```
# This function outputs slightly different things based on what the training and test sets are
#   May want to run this multiple times, if possible. Maybe use median value?
negExps = function(predictor, trainData, testData, trainTarget, testTarget) {
  oldError = Inf
  newError = baseError
  minPow = 0
  # `> 0.01` is there as a threshold to reduce variablity
  # Also means that we favor simpler models over models with slightly less error
  # May be interesting to test this with MSE rather than MAE; would have to change threshold
  while ((newError < oldError) && (abs(oldError - newError) > 0.01)) {
    minPow = minPow - 1
    
    pows = paste("I(", predictor, "**", c(-1:minPow), ")", sep='')
    f = as.formula(paste("trainTarget ~ ", paste(pows, collapse = "+")))
    model = lm(f, data = trainData)
    
    oldError = newError
    newError = AbsError(model, testData, testTarget)
  }
  
  return(minPow + 1)
}

return(negExps("population", XTrain, XTest, yTrain, yTest))
