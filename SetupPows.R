load("crime.RData")

# I have a problem
par(bg = 'grey50')

# Remove the non predictor columns
X = x[, -c(c(1:5), 128)]

# Choose from the predictor variables those which are socio-economic
socioEcon = X[, c(1:96)]

# Choose from the predictor variables those which are police-based
police = X[, c(97:122)]

# Create a vector for violent crime per population
y = x[, 128]

# Note: many functions serve primarily to allow me to avoid thinking

# Randomly divides set into training and test sets
#   pct     the percentage (on [0.0, 1.0]) of the set to use for training
#   data    the data.frame of predictor variables
#   target  the numeric (vector) of target variables
# Number of rows in data should match length of target and should be greater
# than one
# Returns list with the following elements:
#   trainData   the pct percentage of the data set for training models
#   testData    the 1-pct percentage of the data set for testing models
#   trainTarget the pct percentage of the target set for training models
#   testTarget  the 1-pct percentage of the target set for testing models
SplitSet = function(pct, data, target) {
  numRows = ceiling(pct * nrow(data))
  trainRows = sample(c(1:nrow(data)), numRows)
  trainData = data[trainRows, ]
  testData = data[-trainRows, ]
  trainTarget = target[trainRows]
  testTarget = target[-trainRows]
  return(
    list(
      "trainData" = trainData,
      "testData" = testData,
      "trainTarget" = trainTarget,
      "testTarget" = testTarget
    )
  )
}

# Simple function for taking mean absolute error
#   model     the model to test
#   data      the data used to test model
#   target    the target dataset used to test model
#   predictor the up to one predictor the model is based on
AbsError = function(model, data, target, predictor = NULL) {
  if (!is.null(predictor)) {
    naIndex = which(is.na(data[predictor]))
    if (length(naIndex) > 0) {
      data = data[-c(naIndex), ]
      target = target[-c(naIndex)]
    }
  }
  error = suppressWarnings(mean(abs(predict(model, newdata = data) - target), na.rm =TRUE))
  return(error)
}

# Simple function for taking mean squared error
# Favors tests in which large errors are particularly bad
#   model     the model to test
#   data      the data used to test model
#   target    the target dataset used to test model
#   predictor the up to one predictor the model is based on
SqdError = function(model, data, target, predictor = NULL) {
  if (!is.null(predictor)) {
    naIndex = which(is.na(data[predictor]))
    if (length(naIndex) > 0) {
      data = data[-c(naIndex),]
      target = target[-c(naIndex)]
    }
  }
  error = mean((predict(model, newdata = data) - target) ^ 2, na.rm =TRUE)
  return(error)
}

# Creates a model with positive, negative, or both types of exponenets
#   predictor   the column name of the column in trainData to use
#   trainData   the data set from which to draw training data
#   trainTarget the target variable vector from which to draw training data
#   maxPow      the maximum power that should be included in the model
#   minPow      the minimum power that should be included in the model
ModelExp = function(predictor,
                    trainData,
                    trainTarget,
                    maxPow = -1,
                    minPow = 1) {
  # To avoid dividing by zero
  if (minPow < 0) {
    trainData[trainData == 0] = NA
  }
  
  pows = paste("I(", predictor, "**", c(minPow:maxPow), ")", sep = '')
  f = as.formula(paste("trainTarget ~ ", paste(pows, collapse = "+")))
  model = lm(f, data = trainData)
  return(model)
}

# Adds together all the powers from 1 to maxPow
#   predictor the predictor to add powers of (a string)
#   maxPow    the highest exponent to include
# Returns the expression as a string
ExpFormula = function(predictor, maxPow) {
  pows = paste("I(", predictor, "**", c(1:maxPow), ")", sep = '')
  return(paste(pows, collapse = "+"))
}

# Returns the most common element in a vector
#   dataSet   a numeric (vector)
Mode = function(dataSet) {
  uniqData <- unique(dataSet)
  return(uniqData[which.max(tabulate(match(dataSet, uniqData)))])
}

# Increases maximum exponent from 0 until error increases
#   predictor   a string containing the name of column to model
#   trainData   data used for modeling; must contain predictor
#   testData    data used for testing; must contain predictor
#   trainTarget data of target variable used for modeling
#   testTarget  data of target variable used for testing
# Returns the highest exponent reached before error increased
PosExps = function(predictor,
                   trainData,
                   testData,
                   trainTarget,
                   testTarget) {
  # Absolute worst a model should be
  baseModel = lm(trainTarget ~ 1, data = trainData)
  baseError = AbsError(baseModel, testData, testTarget)
  
  oldError = Inf
  newError = baseError
  maxPow = 0
  # May be interesting to test this with MSE rather than MAE
  while (newError < oldError) {
    maxPow = maxPow + 1
    
    model = ModelExp(predictor, trainData, trainTarget, maxPow = maxPow)
    
    oldError = newError
    newError = AbsError(model, testData, testTarget, predictor)
  }
  
  return(maxPow - 1)
}

# Creates a model with specifications according to parameters
#   predictors  predictors the model is based on
#   trainData   data of predictors to train model with
#   trainTarget data of target variable to train model with
#   exponenets  degree of polynomial models corresponding to predictors
CreateModel = function(predictors,
                       trainData,
                       trainTarget,
                       exponents) {
  powForm = paste(mapply(ExpFormula, predictors, exponents), collapse = "+")
  f = as.formula(paste("trainTarget ~ ", powForm))
  model = lm(f, data = trainData)
  return(model)
}

# Provides information on a model
#   model       the model to test
#   testData    data of predictors to test model with
#   testTarget  data of target variable to test model with
#   plot        whether or no to plot the model's predictions
TestModel = function(model, testData, testTarget, plot = FALSE) {
  if (plot) {
    plot(predict(model, newdata = testData), testTarget)
    abline(0, 1)
  }
  
  cat(deparse(substitute(model)),
      "error:",
      AbsError(model, testData, testTarget),
      '\n')
}
