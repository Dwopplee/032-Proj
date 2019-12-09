load("crime.RData")

# I have a problem
# par(bg = 'grey50')

# Remove the non predictor columns
X = x[, -c(c(1:5), 128)]

# Choose from the predictor variables those which are socio-economic
socioEcon = X[, c(1:96)]

# Choose from the predictor variables those which are police-based
police = X[, c(97:122)]

# Create a vector for violent crime per population
y = x[, 128]

# Note: some functions serve primarily to allow me to avoid thinking

# Generates n appropriately sized folds for a given data set
#   n     number of folds
#   data  data set needing splitting
GenFolds = function(n, data) {
  folds = cut(seq(1, nrow(data)), breaks = n , labels = FALSE)
  return(folds)
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
  error = suppressWarnings(mean(abs(predict(model, newdata = data) - target), na.rm = TRUE))
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
  error = suppressWarnings(mean((predict(model, newdata = data) - target) ^ 2, na.rm = TRUE))
  return(error)
}

# Runs a cross-validation loop for a given right hand side of a formula
# Also has to do most of our analysis because nothing gets written to global environment
#   rhs       string containing the predictor side of the formula
#   data      data to base the models off of
#   target    vector of data to be predicted
#   folds     assignments to a fold for each row in data
#   returnVar a boolean that determines whether to return error variance between test sets
#   predictor the name of the predictor, if there's only one. NULL otherwise
#   plot      a boolean that determines whether to plot the predictions against reality
CrossValidate = function(rhs, data, target, folds, returnVar = FALSE, predictor = NULL, plot = FALSE) {
  errors = c()
  
  xPoints = c()
  yPoints = c()
  
  n = folds[which.max(folds)]
  for(i in c(1:n)) {
    testIndex = which(folds == i, arr.ind = TRUE)
    testData = data[testIndex, ]
    testTarget = target[testIndex]
    trainData = data[-testIndex, ]
    trainTarget = target[-testIndex]
    
    f = as.formula(paste("trainTarget ~ ", rhs))
    model = lm(f, data = trainData)
    errors = c(errors, AbsError(model, testData, testTarget, predictor))
    
    xPoints = c(xPoints, predict(model, newdata = testData))
    yPoints = c(yPoints, testTarget)
  }
  
  if (plot) {
    plot(xPoints, yPoints, xlab = "Predictions", ylab = "Actual")
    
    plot(density(yPoints - xPoints, na.rm = TRUE), xlab = "Residuals", main = "")
  }
  
  if (returnVar) {
    return(c(mean(errors), var(errors, na.rm = TRUE)))
  } else {
    return(mean(errors, na.rm = TRUE))  
  }
}

# Adds together all the powers from 1 to maxPow
#   predictor the predictor to add powers of (a string)
#   maxPow    the highest exponent to include
# Returns the expression as a string
ExpFormula = function(predictor, maxPow) {
  pows = paste("I(", predictor, "**", c(1:maxPow), ")", sep = '')
  return(paste(pows, collapse = "+"))
}

# Increases maximum exponent from 0 until error increases
#   predictor   a string containing the name of column to model
#   data        data used for modeling; must contain predictor
#   target      data of target variable used for modeling
# Returns the highest exponent reached before error increased
PosExps = function(predictor,
                    data,
                    target) {
  folds = GenFolds(10, data)
  # Absolute worst a model should be
  baseError = CrossValidate(1, data, target, folds)
  
  oldError = Inf
  newError = baseError
  maxPow = 0
  # May be interesting to test this with MSE rather than MAE
  while (newError < oldError) {
    maxPow = maxPow + 1
    
    rhs = ExpFormula(predictor, maxPow)
    
    oldError = newError
    newError = CrossValidate(rhs, data, target, folds, predictor = predictor)
  }
  
  return(maxPow - 1)
}
