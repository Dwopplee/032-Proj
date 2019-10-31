source('SetupCors.R')

# TODO: Add documentation following format of Powers.R

errs = replicate(100, TestExps(0.8, Xmodel, y, modes))
