context("Generic tests")
test_that("Sanity checks", {
  #source("csindicators/R/TotalTimeExceedingThreshold.R")
  expect_error(TotalTimeExceedingThreshold(NULL), 
    "Parameter 'data' cannot be NULL.")
  expect_error(TotalTimeExceedingThreshold('x'),
   "Parameter 'data' must be numeric.")
  data <- 1:20
  expect_error(TotalTimeExceedingThreshold(data, NULL),
    "Parameter 'threshold' cannot be NULL.")
  expect_error(TotalTimeExceedingThreshold(data, 'x'),
    "Parameter 'threshold' must be numeric.")
  threshold <- 10
  expect_equal(TotalTimeExceedingThreshold(data, threshold), 10)
  dim(data) <- c(2, 10)
  expect_error(TotalTimeExceedingThreshold(data, threshold),
    "Parameter 'data' must have named dimensions.")
  names(dim(data)) <- c('lat', 'time')
  threshold <- array(1:2, 2)
  expect_error(TotalTimeExceedingThreshold(data, threshold),
    "Parameter 'threshold' must have named dimensions.")
  dim(threshold) <- c(time = 2)
  expect_error(TotalTimeExceedingThreshold(data, threshold),  
    "Parameter 'data' and 'threshold' must have the same length on common dimensions.")
  data <- array(1:40, c(x = 2, ftime = 20))
  expect_error(TotalTimeExceedingThreshold(data, threshold),
    "Could not find dimension 'time' in 1th object provided in 'data'.")    
  threshold <- 10
  expect_equal(TotalTimeExceedingThreshold(data, threshold, time_dim = 'ftime'), 
               array(c(15, 15), c(x = 2)))
  dim(threshold) <- c(member = 1, ftime = 1)
  expect_equal(TotalTimeExceedingThreshold(data, threshold, time_dim = 'ftime'), 
               array(c(15, 15), c(x = 2)))
  expect_equal(TotalTimeExceedingThreshold(data, threshold, time_dim = 'x'),
               array(c(rep(0,5), rep(2,15)), c(ftime = 20)))
  expect_error(TotalTimeExceedingThreshold(data, threshold,
                               time_dim = 'x', ncores = 'Z'),
         "Parameter 'ncores' must be numeric")

  expect_equal(TotalTimeExceedingThreshold(data, threshold, time_dim = 2),
         array(c(15,15), c(x = 2)))
  # dimensions:
  data <- array(1:20, c(time = 5, sdate = 2, lat = 2))
  # does this case made sense?
  threshold <- array(1:5, c(time = 5))
  expect_equal(dim(TotalTimeExceedingThreshold(data, threshold)),
               c(sdate = 2, lat = 2))
  threshold <- array(1:2, c(lat = 2))
  expect_equal(dim(TotalTimeExceedingThreshold(data, threshold)),
               c(sdate = 2, lat = 2))
  data <- array(1:60, c(time = 5, fyear = 3, sdate = 2, lat = 2))
  expect_equal(dim(TotalTimeExceedingThreshold(data, threshold, 
                      time_dim = c('time', 'fyear'))),
               c(sdate = 2, lat = 2))

})

test_that("Seasonal forecasts", {
  # compare with scalar fixed threshold
  exp <- CSTools::lonlat_data$exp
  exp$data <- exp$data[1,1:3,,,,] - 247
  SU35_NoP <- CST_TotalTimeExceedingThreshold(exp, threshold = 35)$data  
  expect_equal(SU35_NoP[1,,15,3], c(0,1,1,1,0,0))
  # convert to percentile
  obs <- CSTools::lonlat_data$obs
  exp_percentile <- AbsToProbs(exp$data)
  obs_percentile <- drop(QThreshold(obs$data, threshold = 35))
  data <- exp
  data$data <- exp_percentile
  SU35_P <- CST_TotalTimeExceedingThreshold(data, threshold = obs_percentile)$data
  expect_equal(SU35_P[,2,5,5],  c(3,3,3,3,3,3))
})


