rm(list=ls(all=TRUE))

source('SetupPows.R')

# Generates the optimal maximum exponent based on a random set split
#   pct     the percentage (on [0.0, 1.0]) of the set to use for training
#   data    the data.frame of predictor variables
#   target  the numeric (vector) of target variables
SampleExps = function(pct, data, target) {
  split = SplitSet(pct, data, target)
  exps =  sapply(colnames(X), PosExps, split$trainData, split$testData,
          split$trainTarget, split$testTarget)
  return(exps)
}

# Try to mitigate the randomness with multiple samples
exps = replicate(100, SampleExps(0.8, X, y))

# Take the most common occurring exponents for each predictor
modes = sapply(exps[, ncol(exps)], Mode)

# We don't want predictors that are best as constants
Xmodel = X[, -which(modes==0)]
modes = modes[-which(modes==0)]

# Generate and plot a model with all predictors raised to generated powers

splitX = SplitSet(0.8, X, y)

powForm = paste(mapply(ExpFormula, colnames(X), modes), collapse = "+")
f = as.formula(paste("splitX$trainTarget ~ ", powForm))
model = lm(f, data = splitX$trainData)

plot(predict(model, newdata = splitX$testData), splitX$testTarget)
abline(0, 1)
