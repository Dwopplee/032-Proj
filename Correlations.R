# This code takes about 6 minutes to run (on my machine) if you source it from here.

source('SetupCors.R')

# At this point we've completed the planned-out part of making models.
# We've got a bunch of polynomial models with varying degree "optimized" to
# have minimal error. However, the model suffers from saturation and is far from
# actually optimal. Here, we'll try to explore the linear relationships between
# our predictors in order to reduce this saturation.

# With our "optimized" polynomial models, which predictors are most accurate?
errs = TestExps(10, Xmodel, y, exps)

bestIndex = sort(errs, index.return = TRUE)$ix

# We could technically call this our answer to our research question but the
# model sucks and we haven't done that much.

# Let's try making a model (manually) with accurate variables with low
# covariance:
top10cor = cor(Xmodel[, bestIndex[c(1:10)]])

# Model generated manually based on one sample:
#   Take predictor with lowest error and the one with least correlation with it
powForm0 = paste(mapply(ExpFormula, c("PctKids2Par", "NumIlleg"), c(3, 9)),
                 collapse = "+")
cat("Test 0 error and Variance:", CrossValidate(powForm0, X, y, folds, TRUE), '\n')
# This model reduces error but variance almost triples, which is interesting.
# I suppose less variables means more variance because they can't mitigate each others' variances

# Try to reduce saturation by removing highly correlated predictors:

# Returns indices of predictor and columns with correlations lower then 0.8
#   predictor the predictor to check correlations against
#   data      the data set of predictors to select from
#   oldData   the data set of all predictors, if different from data
LowCors = function(predictor, data, oldData = NULL) {
  cors = sapply(data, cor, data[predictor])
  
  predIndex = which(colnames(data) == predictor)
  if (!is.null(oldData)) {
    predIndex = which(colnames(oldData) == predictor)
  }
  
  # 0.8 is a rather arbitrary threshold, decided by what multiple of 0.05 made
  # test2 (defined later) most accurate during a few trial runs
  newPredictors = c(which(cors < 0.8), predIndex)
  return(newPredictors)
}

# Remove the predictors with high correlations with the most accurate predictor
cors = LowCors(colnames(Xmodel)[bestIndex[1]], Xmodel)

corData = Xmodel[, cors]
corExps = exps[cors]

# Generate and plot a model with all other predictors raised to generated powers
powForm1 = paste(mapply(ExpFormula, colnames(corData), corExps), collapse = "+")
cat("Test 1 error and Variance:", CrossValidate(powForm1, Xmodel, y, folds, TRUE), '\n')

# Try looping this stuff?

# Remove all predictors with high correlation to a "better" one
# This entire function is super sketchy
#   data  the data set of predictors
# This function also requires bestIndex and modes as defined previously
# in this file, but we don't pass them into the function because I'm lazy
# This function pretty much only exists to compartmentalize things a bit more
LoopCors = function(data) {
  newIndex = bestIndex
  newExps = exps
  newData = data
  i = 1
  
  while (i <= length(newIndex)) {
    nextIndex = newIndex[i]
    cors = LowCors(colnames(data)[nextIndex], newData, data)
    
    newData = data[, cors]
    newExps = newExps[cors]
    newIndex = newIndex[newIndex %in% cors]
    
    # We're adding a vector and an integer
    # This is disgusting
    i = which(newIndex == nextIndex) + 1
  }
  return(newIndex)
}

cor2Names = LoopCors(Xmodel)

powForm2 = paste(mapply(ExpFormula, colnames(Xmodel)[cor2Names], exps[cor2Names]), collapse = "+")
cat("Test 2 error and Variance:", CrossValidate(powForm2, Xmodel, y, folds, TRUE), '\n')
