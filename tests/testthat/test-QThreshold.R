context("Generic tests")
test_that("Sanity checks", {
  #source("csindicators/R/QThreshold.R")
  expect_error(QThreshold(NULL),
    "Parameter 'data' cannot be NULL.")
  expect_error(QThreshold('x'),
   "Parameter 'data' must be numeric.")
  data <- 1:20
  expect_error(QThreshold(data, NULL),
    "Parameter 'threshold' cannot be NULL.")
  expect_error(QThreshold(data, 'x'),
    "Parameter 'threshold' must be numeric.")
  threshold <- 10
  expect_error(QThreshold(data, threshold),
    "'x' must have 1 or more non-missing values")
  dim(data) <- c(2, 10)
  expect_error(QThreshold(data, threshold),
    "Parameter 'data' must have named dimensions.")
  names(dim(data)) <- c('lat', 'sdate')
  threshold <- array(1:2, 2)
  expect_error(QThreshold(data, threshold),
    "Parameter 'threshold' must have named dimensions.")
  dim(threshold) <- c(time = 2)
  
  data <- array(1:40, c(x = 2, sdate = 20))
  threshold <- 10
  expect_equal(dim(QThreshold(data, threshold)), c(sdate = 20, x = 2))
  data <- array(1:40, c(x = 2, ftime = 20))
  expect_error(QThreshold(data, threshold),
    "Could not find dimension 'sdate' in 1th object provided in 'data'.")
  expect_equal(dim(QThreshold(data, threshold, sdate_dim = 'ftime')),
               c(ftime = 20, x = 2))
  dim(threshold) <- c(member = 1, ftime = 1)
  expect_equal(dim(QThreshold(data, threshold, sdate_dim = 'ftime')),
               c(ftime = 20, x = 2))
  expect_equal(dim(QThreshold(data, threshold, memb_dim = 'x', sdate_dim = 'ftime')),
               c(ftime = 20, x = 2))
  expect_error(QThreshold(data, threshold,
                               sdate_dim = 'x', ncores = 'Z'),
         "Parameter 'ncores' must be numeric")

  # dimensions:
  data <- array(1:20, c(time = 5, sdate = 2, lat = 2))
  # does this case made sense?
  threshold <- array(1:5, c(time = 5))  
  expect_equal(dim(QThreshold(data, threshold)),
               c(sdate = 2, time = 5, lat = 2))
  threshold <- array(1:2, c(lat = 2))
  expect_equal(dim(QThreshold(data, threshold)),
               c(sdate = 2, time = 5, lat = 2))
  data <- array(1:60, c(time = 5, member = 3, sdate = 2, lat = 2))
  expect_equal(dim(QThreshold(data, threshold)),
               c(sdate = 2, member = 3, time = 5, lat = 2))
  expect_equal(dim(QThreshold(data, threshold, memb_dim = NULL)),
               c(sdate = 2, time = 5, member = 3, lat = 2))

})

test_that("Seasonal forecasts", {

  obs <- CSTools::lonlat_data$obs$data - 248
  obs_percentile <- QThreshold(obs, threshold = 35)
  expect_equal(dim(obs)[4:6], dim(obs_percentile)[4:6])
  expect_equal(obs_percentile[, 1, 1, 3, 20, 53], c(rep(0.4, 4), rep(0.2, 2)))
  obs1 <- obs[,,2,,,] # no sdate
  expect_error(obs1_percentile <- QThreshold(obs1, threshold = 35),
               "Could not find dimension 'sdate' in 1th object provided in 'data'.")
  library(s2dv)
  obs1 <- InsertDim(obs1, 1, 1, name = 'sdate') # one sdate
  expect_error(obs1_percentile <- QThreshold(obs1, threshold = 35),
               "'x' must have 1 or more non-missing values")
  obs2 <- obs[,,,2,,] # one ftime
  obs2_percentile <- QThreshold(obs2, threshold = 35)
  expect_equal(dim(obs2), dim(obs2_percentile))
  expect_equal(obs2_percentile[,14,53], c(0.6, 0.4, 0.6, 0.6, 0.4, 0.4))

})
