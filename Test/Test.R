rm(list=ls(all=TRUE))

test = function(i) {
  source('Analyze.R')
  return(PosExps("medIncome", splitX$trainData, splitX$testData, splitX$trainTarget, splitX$testTarget))
}

results = sapply(c(1:1000), test)
hist(results)
