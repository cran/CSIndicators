context("Generic tests")
test_that("Sanity checks", {
  #source("csindicators/R/AccumulationExceedingThreshold.R")
  expect_error(AccumulationExceedingThreshold(NULL), 
    "Parameter 'data' cannot be NULL.")
  expect_error(AccumulationExceedingThreshold('x'),
   "Parameter 'data' must be numeric.")
  data <- 1:20
  expect_error(AccumulationExceedingThreshold(data, NULL),
    "Parameter 'threshold' cannot be NULL.")
  expect_error(AccumulationExceedingThreshold(data, 'x'),
    "Parameter 'threshold' must be numeric.")
  threshold <- 10
  expect_equal(AccumulationExceedingThreshold(data, threshold), 155)
  dim(data) <- c(2, 10)
  expect_error(AccumulationExceedingThreshold(data, threshold),
    "Parameter 'data' must have named dimensions.")
  names(dim(data)) <- c('lat', 'time')
  threshold <- array(1:2, 2)
  expect_error(AccumulationExceedingThreshold(data, threshold),
    "Parameter 'threshold' must have named dimensions.")
  dim(threshold) <- c(time = 2)
  data <- array(1:40, c(x = 2, ftime = 20))
  expect_error(AccumulationExceedingThreshold(data, threshold),
    "Could not find dimension 'time' in 1th object provided in 'data'.")    
  threshold <- 10
  expect_equal(AccumulationExceedingThreshold(data, threshold, time_dim = 'ftime'), 
               array(c(375, 390), c(x = 2)))
  dim(threshold) <- c(member = 1, ftime = 1)
  expect_equal(AccumulationExceedingThreshold(data, threshold, time_dim = 'ftime'), 
               array(c(375, 390), c(x = 2)))
  expect_equal(AccumulationExceedingThreshold(data, threshold, time_dim = 'x'),
               array(c(rep(0,5), seq(23, 79, 4)), c(ftime = 20)))
  expect_error(AccumulationExceedingThreshold(data, threshold,
                               time_dim = 'x', ncores = 'Z'),
         "Parameter 'ncores' must be numeric")

  expect_equal(AccumulationExceedingThreshold(data, threshold, time_dim = 2),
         array(c(375, 390), c(x = 2)))
  # dimensions:
  data <- array(1:20, c(time = 5, sdate = 2, lat = 2))
  # does this case made sense?
  threshold <- array(1:5, c(time = 5))
  expect_equal(dim(AccumulationExceedingThreshold(data, threshold)),
               c(sdate = 2, lat = 2))
  threshold <- array(1:2, c(lat = 2))
  expect_equal(dim(AccumulationExceedingThreshold(data, threshold)),
               c(sdate = 2, lat = 2))
  data <- array(1:60, c(time = 5, fyear = 3, sdate = 2, lat = 2))
  expect_equal(dim(AccumulationExceedingThreshold(data, threshold, 
                      time_dim = c('time', 'fyear'))),
               c(sdate = 2, lat = 2))

})

test_that("Seasonal forecasts", {

  exp <- CSTools::lonlat_data$exp
  exp$data <- exp$data[,1:4,1:2,,,]
  res <- CST_AccumulationExceedingThreshold(exp, threshold = 280)
  expect_equal(round(res$data[,2,2,2]),
               c(0, 280, 281, 281))
  # GDD
  exp <- array(NA, dim = c(member = 6, sdate = 3, ftime = 214, lat =4, lon = 4))
  exp1 <- drop(CSTools::lonlat_prec$data) * 86400000
  exp[,,1:31,,] <- exp1 + 10; exp[,,32:62,,] <- exp1 + 11
  exp[,,63:93,,] <- exp1 + 12; exp[,,94:124,,] <- exp1 + 13
  exp[,,125:155,,] <- exp1 + 14; exp[,,156:186,,] <- exp1 + 15
  exp[,,187:214,,] <- exp1[,,1:28,,] + 16
  Dates <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
               as.Date("30-11-2000", format = "%d-%m-%Y"), by = 'day'),
           seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
               as.Date("30-11-2001", format = "%d-%m-%Y"), by = 'day'),
           seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
               as.Date("30-11-2002", format = "%d-%m-%Y"), by = 'day'))
  GDD <- AccumulationExceedingThreshold(exp - 17, threshold = 0, dates = Dates, time_dim = 'ftime', 
                                        start = list(1, 4), end = list(31, 10), na.rm = TRUE)
  expect_equal(round(GDD[,1,1,1]),
               c(538, 372, 116, 525, 220, 330))
  expect_equal(dim(GDD),
              c(member = 6, sdate = 3, lat =4, lon = 4))
  expect_error(AccumulationExceedingThreshold(exp - 17, threshold = 0, dates = Dates, start = list(1, 4), end = list(31, 10)),
               "Could not find dimension 'time' in 1th object provided in 'data'.")
  expect_equal(all(is.na(AccumulationExceedingThreshold(exp - 17, threshold = 0, dates = Dates, time_dim = 'ftime',start = list(1, 4), end = list(31, 10)))),
               all(is.na(c(NA, NA))))

  # test the 'diff'
  input <- c(1:20)
  threshold <- 3
  expect_equal(AccumulationExceedingThreshold(input, threshold, diff = TRUE),
               153)
  expect_equal(AccumulationExceedingThreshold(input, threshold),
               204)
  input1 <- -input[1:15]
  threshold <- -5
  expect_equal(AccumulationExceedingThreshold(input1, threshold, op = '<'),
               -105)
  expect_equal(AccumulationExceedingThreshold(input1, threshold, op = '<', diff = TRUE),
               -55)
})
