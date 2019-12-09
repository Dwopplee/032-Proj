rm(list = ls(all = TRUE))

# Sourcing this file takes my machine slightly less than 1 minute to run.

# This is the first iteration of the model.

# We find the "optimal" polynomial relationship for each predictor with our
# dependent variable by repeatedly splitting our data set into training and test
# sets, then iterating up through higher degree polynomials until the absolute
# error in the training set increases. 

# We then create a model by adding together all the polynomials with degree of
# at least 1.

source('SetupPows.R')

# Shuffle the data set before doing anything
ranRows = sample(nrow(X))
X = X[ranRows, ]
y = y[ranRows]

folds = GenFolds(10, X)

# Our baseline model:
cat("Base error and Variance:", CrossValidate(1, X, y, folds, TRUE, NULL, TRUE), '\n')

exps = sapply(colnames(X), PosExps, X, y)

# We don't want predictors that are best as constants
Xmodel = X[, -which(exps == 0)]
exps = exps[-which(exps == 0)]

# Generate a right-hand-side with all predictors raised to generated powers
pow0 = paste(mapply(ExpFormula, colnames(Xmodel), exps), collapse = "+")

folds = GenFolds(10, Xmodel)

cat("model0 error and variance:", CrossValidate(pow0, Xmodel, y, folds, TRUE, NULL, TRUE), '\n')

# Alright so this model doesn't look that bad if you're not really paying attention
# It does bring the error down from ~0.1784 to 0.0996-0.1025
# Given that the values we're predicting range from 0 to 1, this isn't great. 
