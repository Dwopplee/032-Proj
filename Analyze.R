rm(list=ls(all=TRUE))

source('Setup.R')

sampleExps = function(pct, data, target) {
  split = SplitSet(pct, data, target)
  exps = sapply(colnames(X), PosExps, split$trainData, split$testData, split$trainTarget, split$testTarget)
  return(exps)
}

exps = replicate(100, sampleExps(0.8, X, y))
modes = sapply(exps[, ncol(exps)], Mode)
modes = modes[-which(modes==0)]
powForm = paste(mapply(ExpFormula, colnames(X), modes), collapse = "+")

splitX = SplitSet(0.8, X, y)

# This model is really bad
f = as.formula(paste("splitX$trainTarget ~ ", powForm))
model = lm(f, data = splitX$trainData)

plot(predict(model, newdata = splitX$testData), splitX$testTarget)
abline(0, 1)