source('Powers.R')

# Tests which polynomial models from Powers.R have lowest errors
#   pct     fraction by which to split data into test and training set
#   data    data.frame with predictor variables
#   target  vector containing target variable
#   exps    degree of polynomial models
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
