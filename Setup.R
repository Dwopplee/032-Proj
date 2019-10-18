load("crime.RData")

# Remove the non predictor columns
X = x[, -c(c(1:5), 128)]

# Choose from the predictor variables those which are socio-economic
socioEcon = X[, c(1:96)]

# Choose from the predictor variables those which are police-based
police = X[, c(97:122)]

# Create a vector for violent crime per population
y = x[, 128]
