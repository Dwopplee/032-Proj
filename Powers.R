rm(list = ls(all = TRUE))

# This is the first iteration of the model.

# We find the "optimal" polynomial relationship for each predictor with our
# dependent variable by repeatedly splitting our data set into training and test
# sets, then iterating up through higher degree polynomials until the absolute
# error in the training set increases. We do this 100 times in an attempt to
# remove the effect of the random sampling of the train/test split.

# We then create a model by adding together all the polynomials with degree of
# at least 1.

source('SetupPows.R')

# Generates the optimal maximum exponent based on a random set split
#   pct     the percentage (on [0.0, 1.0]) of the set to use for training
#   data    the data.frame of predictor variables
#   target  the numeric (vector) of target variables
SampleExps = function(pct, data, target) {
  split = SplitSet(pct, data, target)
  exps =  sapply(
    colnames(X),
    PosExps,
    split$trainData,
    split$testData,
    split$trainTarget,
    split$testTarget
  )
  return(exps)
}

# Try to mitigate the randomness with multiple samples
exps = replicate(100, SampleExps(0.8, X, y))

# Take the most common occurring exponents for each predictor
modes = sapply(exps[, ncol(exps)], Mode)

# We don't want predictors that are best as constants
Xmodel = X[, -which(modes == 0)]
modes = modes[-which(modes == 0)]

# Generate and plot a model with all predictors raised to generated powers

splitX = SplitSet(0.8, Xmodel, y)

model0 = CreateModel(colnames(Xmodel), splitX$trainData, splitX$trainTarget, modes)

TestModel(model0, splitX$testData, splitX$testTarget, plot = TRUE)

# This model doesn't quite suck (!), but we can (hopefully) do better.
# Edit: mean absolute percent error seems to be about 84%, which kinda sucks
#   Don't ask me to make a funciton that finds this
# It also has limited impact on our research question -- the impact of variables
# with high dependence on other variables may be diminished
# Notable issues:
#   High saturation: we predict negative crime, and crime greater than 1
#   Error increases as crime (predicted and actual) increases
