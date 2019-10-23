rm(list=ls(all=TRUE))

source('Setup.R')

splitX = SplitSet(0.8, X, y)
XTrain = splitX$trainData
XTest = splitX$testData
yTrain = splitX$trainTarget
yTest = splitX$testTarget

# TODO: make some functions:
#   1 iterates through positive exponents until model gets worse
#   2 iterates through negative exponents until model gets worse
#   3 compares both models (adds them together?) then chooses best one
# Test models exclusively with test set. use same training/test set for all models
# (or something)

posExps = function(predictor, data) {
  # This is disgusting
  oldError = 101000
  newError = 100000
  maxPow = 0
  while (newError < oldError) {
    maxPow = maxPow + 1
    
    pows = paste("I(population**",c(1:maxPow),")",sep='')
    f = as.formula(paste("yTrain ~ ", paste(pows, collapse="+")))
    model = lm(f, data = XTrain)
    
    oldError = newError
    newError = AbsError(model, XTest, yTest)
    print(newError)
  }
  
  return(maxPow)
}

print(posExps("predictor", "data"))
