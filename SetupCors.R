
# That model sucks (suffers from extreme saturation and likely overfitting).
# Try finding models with smallest errors:

# TODO: consider moving this to Setup.R
TestExps = function(pct, data, target, exps) {
  split = SplitSet(pct, data, target)
  models = mapply(ModelExp, predictor = colnames(data), maxPow = exps, MoreArgs = list(trainData = split$trainData,
                                                                                       trainTarget = split$trainTarget))
  errors = mapply(NAAbsError, models, colnames(data), MoreArgs = list(split$testData, split$testTarget))
  return(errors)
}

TestExps(0.8, Xmodel, y, modes)