context('Tests of the basic PLS functionality')

# Temporary measure to pass the tests while production R and devel R have
# different algorithms for generating random numbers.  When 3.6.0 is out,
                                        # consider removing this and update the saved results:
suppressWarnings(RNGversion("3.5.0"))

# Set seed to make random choices reproducible
set.seed(100)
data(yarn)

## Default methods:
yarn.pcr <- pcr(density ~ NIR, 6, data = yarn, validation = "CV")
yarn.pcr$fit.time = 0   # This is the time it took to fit the model, this is not constant. Setting it to zero makes it constant and checkable in this test.
yarn.pls <- plsr(density ~ NIR, 6, data = yarn, validation = "CV")
yarn.pls$fit.time = 0
yarn.pls.widekernel <- plsr(density ~ NIR, 6, data = yarn, validation = "CV", method = 'widekernelpls')
yarn.pls.widekernel$fit.time = 0
cppls_weights = runif(nrow(yarn))
yarn.cppls <- cppls(density ~ NIR, 6, data = yarn, validation = "CV", weights = cppls_weights)
yarn.cppls$fit.time = 0
test_that('Basic model fitting methods works', {
  expect_equal_to_reference(yarn.pcr, 'yarn.pcr.rds', info = 'Principal Component Regression (pcr), method = `svdpc`')
  expect_equal_to_reference(yarn.pls, 'yarn.pls.rds', info = 'Partial Least Squars (pls), method = `kernelpls`')
  expect_equal_to_reference(yarn.pls.widekernel, 'yarn.pls.widekernel.rds', info = 'Partial Least Squars (pls), method = `widekernelpls`')
  expect_equal_to_reference(yarn.cppls, 'yarn.cppls.rds', info = 'Canocial Powered Partial Least Squares works (cppls), method = `cppls`')
})

## Alternative methods:
yarn.oscorespls <- mvr(density ~ NIR, 6, data = yarn, validation = "CV",
                       method = "oscorespls")
yarn.oscorespls$fit.time = 0
yarn.simpls <- mvr(density ~ NIR, 6, data = yarn, validation = "CV",
                   method = "simpls")
yarn.simpls$fit.time = 0
test_that('Alternative model fitting methods works', {
  expect_equal_to_reference(yarn.oscorespls, 'yarn.oscorespls.rds', info = 'Classical orthogonal scores algorithm (oscorespls)')
  expect_equal_to_reference(yarn.simpls, 'yarn.simpls.rds', info = 'SIMPLS algorithm')
})

## Match predictions
context('Test if predictions of the different methods line up')
pred.pcr = predict(yarn.pcr)
pred.pls = predict(yarn.pls)
pred.pls.widekernel = predict(yarn.pls.widekernel)
pred.cppls = predict(yarn.cppls)
pred.oscorepls = predict(yarn.oscorespls)
pred.simpls = predict(yarn.simpls)

test_that('The predictions for PLS are the same and for PCR/CPPLS are diffrent than PLS', {
  expect_equal_to_reference(pred.pls, 'pred.pls.rds', info = 'PLS predictions should not change')
  expect_equal_to_reference(pred.pls.widekernel, 'pred.pls.widekernel.rds', info = 'PLS widekernel predictions should not change')
  expect_equal_to_reference(pred.pcr, 'pred.pcr.rds', info = 'PCR predictions should not change')
  expect_equal_to_reference(pred.cppls, 'pred.cppls.rds', info = 'PCR predictions should not change')
  expect_equal(pred.pls, pred.oscorepls, info = 'PLS prediction should be the same for normal PLS and oscorepls')
  expect_equal(pred.pls, pred.pls.widekernel, info = 'PLS prediction should be the same for normal PLS and widekernel PLS')
  expect_equal(pred.pls, pred.simpls, info = 'PLS prediction should be the same for normal PLS and simpls')
  expect_false(isTRUE(all.equal(pred.pls, pred.pcr)), info = 'PCR should not provide same predictions as PLS')
  expect_false(isTRUE(all.equal(pred.pls, pred.cppls)), info = 'CPPLS should not provide same predictions as PLS')
})

## Look at results with mean centering disabled
context('Test if disabling mean centering works correctly')
yarn_nocenter.pcr <- pcr(density ~ NIR, 6, data = yarn, validation = "CV", center = FALSE)
yarn_nocenter.pls <- plsr(density ~ NIR, 6, data = yarn, validation = "CV", center = FALSE)
yarn_nocenter.pls.widekernel <- plsr(density ~ NIR, 6, data = yarn, validation = "CV", method = 'widekernel', center = FALSE)
yarn_nocenter.cppls <- cppls(density ~ NIR, 6, data = yarn, validation = "CV", weights = cppls_weights, center = FALSE)

## Alternative methods:
yarn_nocenter.oscorespls <- mvr(density ~ NIR, 6, data = yarn, validation = "CV",
                       method = "oscorespls", center = FALSE)
yarn_nocenter.simpls <- mvr(density ~ NIR, 6, data = yarn, validation = "CV",
                   method = "simpls", center = FALSE)

# Make predictions
pred_nocenter.pcr = predict(yarn_nocenter.pcr)
pred_nocenter.pls = predict(yarn_nocenter.pls)
pred_nocenter.pls.widekernel = predict(yarn_nocenter.pls.widekernel)
pred_nocenter.cppls = predict(yarn_nocenter.cppls)
pred_nocenter.oscorepls = predict(yarn_nocenter.oscorespls)
pred_nocenter.simpls = predict(yarn_nocenter.simpls)

test_that('Predictions with mean centering disabled are internally consistent', {
  expect_equal_to_reference(pred_nocenter.pls, 'pred_nocenter.pls.rds', info = 'PLS predictions should not change')
  expect_equal_to_reference(pred_nocenter.pls.widekernel, 'pred_nocenter.pls.widekernel.rds', info = 'PLS predictions should not change')
  expect_equal_to_reference(pred_nocenter.oscorepls, 'pred_nocenter.oscorepls.rds', info = 'PLS predictions should not change')
  expect_equal_to_reference(pred_nocenter.pcr, 'pred_nocenter.pcr.rds', info = 'PCR predictions should not change')
  expect_equal_to_reference(pred_nocenter.cppls, 'pred_nocenter.cppls.rds', info = 'PCR predictions should not change')
  expect_equal(pred_nocenter.pls, pred_nocenter.oscorepls, info = 'PLS prediction should be the same for normal PLS and oscorepls')
  expect_equal(pred_nocenter.pls, pred_nocenter.simpls, info = 'PLS prediction should be the same for normal PLS and simpls')
  expect_equal(pred_nocenter.pls, pred_nocenter.pls.widekernel, info = 'PLS prediction should be the same for normal PLS and PLS widekernel')
  expect_false(isTRUE(all.equal(pred_nocenter.pls, pred.pcr)), info = 'PCR should not provide same predictions as PLS')
  expect_false(isTRUE(all.equal(pred_nocenter.pls, pred.cppls)), info = 'CPPLS should not provide same predictions as PLS')
})

test_that('Predictions with mean centering disabled differ from normal predictions', {
  expect_false(isTRUE(all.equal(pred.pls, pred_nocenter.pls)), info = 'PLS with no mean center should differ')
  expect_false(isTRUE(all.equal(pred.pls.widekernel, pred_nocenter.pls.widekernel)), info = 'PLS widekernel with no mean center should differ')
  expect_false(isTRUE(all.equal(pred.simpls, pred_nocenter.simpls)), info = 'PLS simpls with no mean center should differ')
  expect_false(isTRUE(all.equal(pred.oscorepls, pred_nocenter.oscorepls)), info = 'PLS oscorepls with no mean center should differ')
  expect_false(isTRUE(all.equal(pred.cppls, pred_nocenter.cppls)), info = 'CPPLS with no mean center should differ')
  expect_false(isTRUE(all.equal(pred.pcr, pred_nocenter.pcr)), info = 'PCR with no mean center should differ')
})

context('Compare non-centered results to Unscrambler results')
# Results generated by The Unscrambler, another software package that can perform PLS.
unscrambler_results = structure(list(centering = c(99.54402, 80.03986, 80.06403, 60.68136,
                                                   60.42256, 60.82306, 40.03272, 40.23078, 41.24754, 40.77765, 20.25846,
                                                   20.27943, 20.41399, 20.6133, 20.28128, 0.1703148, -0.2173195,
                                                   0.05234528, 0.1929855, 0.2232819, 0.128685, 51.10463, 50.3434,
                                                   32.29767, 34.55276, 30.04396, 20.54807, 19.86818),
                                     no_centering = c(99.50454, 80.08273, 80.03199, 60.71494, 60.49206, 60.85455, 39.96323, 40.26112,
                                                   41.14723, 40.78748, 20.30801, 20.32145, 20.46079, 20.67536, 20.26472,
                                                   0.1926269, -0.2748941, 0.03286389, 0.1880585, 0.263258, 0.1293178,
                                                   51.07263, 50.36798, 32.26351, 34.51244, 30.06797, 20.57023, 19.77034)),
                                .Names = c("centering", "no_centering"), class = "data.frame", row.names = c(NA, -28L))

test_that('Predictions for both centering and non-centering match between the package and The Unscrambler', {
  expect_equal(as.numeric(pred.pls[,,6]), unscrambler_results$centering, info = 'centered results in R match that of The Unscrambler', tolerance = 1e-04)
  expect_equal(as.numeric(pred_nocenter.pls[,,6]), unscrambler_results$no_centering, info = 'non-centered results in R match that of The Unscrambler', tolerance = 1e-04)
})
