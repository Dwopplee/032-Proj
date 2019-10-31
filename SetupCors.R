
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

errs = replicate(100, TestExps(0.8, Xmodel, y, modes))

# TODO: 85% of data set is below 0.5. maybe balance data set somehow?

# TODO: now that we have this stuff we can try checking linear dependence of powers of things?
#   May have to generate a model then use the right half as operator on each value, then check cor of those
#   Maybe just do cor() for all the things and then pick those that are greater than some threshold
