rm(list=ls(all=TRUE))

source('Setup.R')

sampleExps = function(pct, data, target) {
  split = SplitSet(pct, data, target)
  exps = sapply(colnames(X), PosExps, split$trainData, split$testData, split$trainTarget, split$testTarget)
  return(exps)
}

exps = replicate(100, sampleExps(0.8, X, y))

modes = sapply(exps[, ncol(exps)], Mode)
