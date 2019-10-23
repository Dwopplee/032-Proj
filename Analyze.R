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
#   1 iterates through positive exponents until model gets worse
#   2 iterates through negative exponents until model gets worse
#   3 compares both models (adds them together?) then chooses best one
# Test models exclusively with test set. use same training/test set for all models
# (or something)

# TODO: Make this function take inputs (predictor and target)
# This function outputs slightly different things based on what the training and test sets are
#   May want to run this multiple times, if possible.
posExps = function(predictor) {
  oldError = Inf
  newError = baseError
  maxPow = 0
  # `> 0.01` is there as a threshold to reduce variablity
  # Also means that we favor simpler models over models with slightly less error
  # May be interesting to test this with MSE rather than MAE
  while ((newError < oldError) && (abs(oldError - newError) > 0.01)) {
    maxPow = maxPow + 1
    
    pows = paste("I(", predictor, "**",c(1:maxPow),")",sep='')
    f = as.formula(paste("yTrain ~ ", paste(pows, collapse="+")))
    model = lm(f, data = XTrain)
    
    oldError = newError
    newError = AbsError(model, XTest, yTest)
    # cat(maxPow, ":", oldError, newError, '\n')
  }
  
  return(maxPow-1)
}

return(posExps("population"))
