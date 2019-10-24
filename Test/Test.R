rm(list=ls(all=TRUE))

test = function(i) {
  source('Analyze.R')
  return(negExps("population", XTrain, XTest, yTrain, yTest))
}

results = sapply(c(1:1000), test)
hist(results)
