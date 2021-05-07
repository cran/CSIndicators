context("Generic tests")
test_that("Sanity checks", {
  #source("csindicators/R/TotalSpellTimeExceedingThreshold.R")
  expect_error(TotalSpellTimeExceedingThreshold(NULL), 
    "Parameter 'data' cannot be NULL.")
  expect_error(TotalSpellTimeExceedingThreshold('x'),
   "Parameter 'data' must be numeric.")
  data <- 1:20
  expect_error(TotalSpellTimeExceedingThreshold(data, NULL),
    "Parameter 'threshold' cannot be NULL.")
  expect_error(TotalSpellTimeExceedingThreshold(data, 'x'),
    "Parameter 'threshold' must be numeric.")
  threshold <- 10
  expect_error(TotalSpellTimeExceedingThreshold(data, threshold),
    paste("argument", '"spell"', "is missing, with no default"))
  dim(data) <- c(2, 10)
  expect_error(TotalSpellTimeExceedingThreshold(data, threshold, spell = 2),
    "Parameter 'data' must have named dimensions.")
  names(dim(data)) <- c('time', 'lat')
  threshold <- array(1:2, 2)
  dim(threshold) <- c(time = 2)
  expect_equal(TotalSpellTimeExceedingThreshold(data, threshold, spell = 2),  
    array(c(0,rep(2,9)), c(lat = 10)))
  data <- array(1:40, c(x = 2, ftime = 20))
  expect_error(TotalSpellTimeExceedingThreshold(data, threshold, spell = 2),
    "Could not find dimension 'time' in 1th object provided in 'data'.")    
  threshold <- 10
  expect_equal(TotalSpellTimeExceedingThreshold(data, threshold, spell = 2, time_dim = 'ftime'), 
               array(c(15, 15), c(x = 2)))
  threshold <- rep(10, 20)
  dim(threshold) <- c(member = 1, ftime = 20)
  expect_equal(TotalSpellTimeExceedingThreshold(data, threshold, spell = 2, time_dim = 'ftime'), 
               array(c(15, 15), c(x = 2, member = 1)))
  expect_error(TotalSpellTimeExceedingThreshold(data, threshold, spell = 2, time_dim = 'y'),
     paste("Could not find dimension 'y' in 1th object provided in 'data'"))

})

test_that("Seasonal Forecasts", {

  exp <- CSTools::lonlat_data$exp
  exp$data <- exp$data[1,1:3,1:3,,,]
  res <- CST_TotalSpellTimeExceedingThreshold(exp, threshold = 260, spell = 2)
  expect_equal(res$data[,,1,1],
               array(c(3,3,3,3,3,3,3,3,3), c(member = 3, sdate = 3)))
  # compare with percentile
  thresholdP <- Threshold(exp$data, threshold = 0.9)
  WSDI <- CST_TotalSpellTimeExceedingThreshold(exp, threshold = thresholdP, spell = 2)
  expect_equal(WSDI$data[3,3,3,],
               c(rep(0, 6), rep(2, 2), 0, 2, 0, 2, rep(0, 29), 2, rep(0, 11)))
  thresholdP1 <- thresholdP[1,,]
  WSDI1 <- CST_TotalSpellTimeExceedingThreshold(exp, threshold = thresholdP1, spell = 2)
  expect_equal(WSDI1$data[3,3,3,],
               c(rep(0, 53)))
})
