context("Generic tests")
test_that("Sanity checks", {
  #source("csindicators/R/Threshold.R")
  expect_error(Threshold(NULL),
    "Parameter 'data' cannot be NULL.")
  expect_error(Threshold('x'),
   "Parameter 'data' must be numeric.")
  data <- 1:20
  expect_error(Threshold(data, NULL),
    "Parameter 'threshold' cannot be NULL.")
  expect_error(Threshold(data, 'x'),
    "Parameter 'threshold' must be numeric.")
  threshold <- 0.9
  expect_equal(Threshold(data, threshold), 18.1)
  dim(data) <- c(2, 10)
  expect_error(Threshold(data, threshold),
    "Parameter 'data' must have named dimensions.")
  names(dim(data)) <- c('lat', 'sdate')
  expect_error(Threshold(data, threshold),
    "Could not find dimension 'member' in 1th object provided in 'data'.")
  expect_equal(Threshold(data, threshold, memb_dim = NULL),
               array(c(17.2, 18.2), c(lat = 2)))
  threshold <- c(0.1, 0.2)
  expect_equal(Threshold(data, threshold, memb_dim = NULL),
               array(c(2.8, 4.6, 3.8, 5.6), c(probs = 2, lat = 2)))
  data <- array(1:40, c(x = 2, ftime = 20))
  expect_error(Threshold(data, threshold),
    "Could not find dimension 'sdate' in 1th object provided in 'data'.")
  expect_equal(dim(Threshold(data, threshold, sdate_dim = 'ftime', memb_dim = NULL)),
               c(probs = 2, x = 2))
  # threshold with dimensions ?
  dim(threshold) <- c(member = 2, ftime = 1)
  expect_equal(dim(Threshold(data, threshold, sdate_dim = 'ftime', memb_dim = NULL)),
               c(probs = 2, x = 2))
  expect_equal(dim(Threshold(data, threshold, memb_dim = 'x', sdate_dim = 'ftime')),
               c(probs = 2))
})

test_that("Seasonal forecasts", {

  exp <- CSTools::lonlat_data$exp$data
  thresholdP <- Threshold(exp, threshold = 0.9)
  expect_equal(dim(exp)[4:6], dim(thresholdP)[2:4])
  expect_equal(round(thresholdP[1, , 2, 2]), c(283, 281, 280))
  exp1 <- exp[1, 1, 1, , , ] # no member
  library(s2dv) # 1 member and 1 sdate
  exp1 <- InsertDim(InsertDim(exp1, 1, 1, name = 'sdate'), 1, 1, name = 'member')
  exp1_thresholdP <- Threshold(exp1, threshold = 0.9)
  expect_equal(round(exp1_thresholdP[, 2, 2]), c(281, 279, 276))

})
