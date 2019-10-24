rm(list=ls(all=TRUE))

source('Setup.R')

# Following from Oct 22 lecture
# just kidding im too slow

# Make a model with everything
# fullModel = lm(y ~ ., data = X)

# Statistical significance: dots/stars next to summary:
#   From R output:
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#   Higher values mean more likely to be random values
# print(summary(fullModel))

# Wasn't in lecture, he did a confusion matrix
# plot(y, predict(fullModel, newdata = X))
# abline(0, 1)

# Training/test sets
frac = 0.8
nrows = ceiling(0.8*nrow(X))
trainrows = sample(c(1:nrow(X)), nrows)

Xtrain = X[trainrows, ]
Xtest = X[-trainrows, ]
ytrain = y[trainrows]
ytest = y[-trainrows]

trainModel = lm(ytrain ~ ., data = Xtrain)

# print(summary(trainModel))
plot(ytest, predict(trainModel, newdata = Xtest), xlim = c(0, 1), ylim = c(0, 1))
abline(0, 1)

# Something about balancing data set. is this doable with continuous model?
# Forces favoring of one side or the other.
#   Actually this is our question about high violence rates, sort of.


# He moved on to continuous data sets (!)
# smaller set, easier to analyze by hand

Correlation = function(predictor) {
  cor(predictor, y)  
}

results = sapply(X[, c(1:122)], Correlation)
# Have high linear relationship
print(which(abs(results)>0.5))

# if you have classification data points, then convert to binary variable
# called contrast coding
# maybe relevant to:
#   LemasGangUnitDeploy
# but maybe not; already semi-binary

# for testing accuracy of model, qualitatively
# plot(density(prediction-actual))
# or
# plot(prediction, actual)
# and
# abline(0, 1)
