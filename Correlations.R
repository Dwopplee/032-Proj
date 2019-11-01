source('SetupCors.R')

# TODO: Add documentation following format of Powers.R

# At this point we've entered our exploratory phase: the initial plan is complete.

# With our "optimized" polynomial models, which predictors are most accurate?
errs = replicate(1000, TestExps(0.8, Xmodel, y, modes))
means = sapply(errs[, length(modes)], mean)

# This is actually super inconsistent, even with 1000 samples
bestIndex = sort(means, index.return = TRUE)$ix

# Let's try making a model (manually) with accurate variables with low covariance:
top10cor = cor(Xmodel[, bestIndex[c(1:10)]])

# Model generated manually based on one sample:
#   Take predictor with lowest error and the one with least correlation with it
test0 = paste(mapply(ExpFormula, c("PctKids2Par", "NumIlleg"), c(3, 9)),
              collapse = "+")
test0Form = as.formula(paste("splitX$trainTarget ~ ", test0))
test0Model = lm(test0Form, data = splitX$trainData)

plot(predict(test0Model, newdata = splitX$testData),
     splitX$testTarget)
abline(0, 1)

cat("Test 0 error:",
    AbsError(test0Model, splitX$testData, splitX$testTarget))

# This model seemed worse, but only slightly, and is much simpler.
# Noteable issues:
#   We never/very rarely predict high errors
#   Error increases significantly as crime (predicted and actual) increases

# A few options: 
#   Try to balance the data set between high and low crime rates
#     This should help us predict higher crime rates more often
#     Applies most directly to test0-type models
#   Try to decrease saturation by exploration of correlations
#     This primarily applies to our first model
#     Can be explored by hand or potentially automated
#     May find some nonlinear relationship that reduced spread in both models
#   Try to fill NAs based on other predictors
#     Probably would provide a better foundation for current/future work
#     Professor said it would be important (and I'm still tempted to ignore it)
#   Try to fill NAs based on same column
#     Should be pretty easy, doesn't really fit here though
