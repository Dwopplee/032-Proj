# WHen molding the data set, we have a few options:
#   Try to balance the data set between high and low crime rates
#     This should help us predict higher crime rates more often
#   Try to fill NAs based on other predictors
#     Probably would provide a better foundation for current/future work
#     Professor said it would be important (and I'm still tempted to ignore it)
#   Try to fill NAs based on same column
#     Should be pretty easy, doesn't really fit here though
#     Maybe someone else could do this?

# Side note:
# After current predictor trimming, OtherPerCap is only column w/ NA value left
# We could do something like this?
# Xmodel$OtherPerCap[131] = median(Xmodel$OtherPerCap, na.rm = TRUE)
# This might work for what we have here but would optimally be done before trying
# to generate polynomial models.