source('Powers.R')

# Tests which polynomial models from Powers.R have lowest errors
#   n       number of folds for cross-validation
#   data    data.frame with predictor variables
#   target  vector containing target variable
#   exps    degree of polynomial models
TestExps = function(n, data, target, exps) {
  folds = GenFolds(n, Xmodel)
  rhss = mapply(
    ExpFormula,
    predictor = colnames(data),
    maxPow = exps,
    SIMPLIFY = FALSE
  )
  errors = mapply(
    CrossValidate,
    rhs = rhss,
    predictor = colnames(data),
    MoreArgs = list(data = data,
                    target = target,
                    folds = folds)
  )
  return(errors)
}
