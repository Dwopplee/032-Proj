# TODO: uncomment this out when complete.
# For now, we can just source this file and let the data sit.
# source('Powers.R')

# TODO: now that we have this stuff we can try checking linear dependence of
# powers of things?
#   May have to generate a model then use the right half as operator on each
#     value, then check cor of those
#   Maybe just do cor() for all the things and then pick those that are greater
#     than some threshold

# TODO: 85% of data set is below 0.5. maybe balance data set somehow?

# TODO: Once we have relationships between predictors maybe we can estimate
#       values of missing data points?

TestExps = function(pct, data, target, exps) {
  split = SplitSet(pct, data, target)
  models = mapply(
    ModelExp,
    predictor = colnames(data),
    maxPow = exps,
    SIMPLIFY = FALSE,
    MoreArgs = list(
      trainData = split$trainData,
      trainTarget = split$trainTarget
    )
  )
  errors = mapply(
    AbsError,
    model = models,
    predictor = colnames(data),
    MoreArgs = list(data = split$testData,
                    target = split$testTarget)
  )
  return(errors)
}
