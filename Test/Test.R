rm(list=ls(all=TRUE))

source('Setup.R')

test = function(i) {
  splitX = SplitSet(0.8, X, y)
  return(PosExps("PctFam2Par", splitX$trainData, splitX$testData, splitX$trainTarget, splitX$testTarget))
}

results = sapply(c(1:1000), test)
print(table(results))


medMod = ModelExp("PctFam2Par", splitX$trainData, splitX$trainTarget, median(results))
modMod = ModelExp("PctFam2Par", splitX$trainData, splitX$trainTarget, Mode(results))
plot(X$PctFam2Par, y)
points(X$PctFam2Par, predict(medMod, newdata = X), col = 'red')
points(X$PctFam2Par, predict(modMod, newdata = X), col = 'blue')
