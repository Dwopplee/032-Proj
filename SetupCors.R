# That model sucks.
# Try finding models with smallest errors:

# TODO: consider moving this to Setup.R
TestExps = function(pct, data, target, exps) {
  split = SplitSet(pct, data, target)
  # TODO: mapply converts trainData and trainTarget to vectors. not sure how to fix this
  models = mapply(ModelExp, colnames(data), rep(split$trainData, times = length(exps)),
                  rep(split$trainTarget, times = length(exps)), exps)
  errors = sapply(models, AbsError, split$testData, split$testTarget)
  return(errors)
}
