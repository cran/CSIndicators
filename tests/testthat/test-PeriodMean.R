context("Generic tests")
test_that("Sanity Checks", {
  #source("csindicators/R/PeriodMean.R")
  expect_error(PeriodMean('x'), "Parameter 'data' must be numeric.")
  expect_equal(PeriodMean(array(1, c(x = 1)), time_dim = 'x'), 1)
  expect_error(PeriodMean(data = NULL), "Parameter 'data' cannot be NULL.")
  expect_error(PeriodMean(1, dates = '2000-01-01', end = 3, start = 4),
    "Parameter 'start' and 'end' must be lists indicating the day and the month of the period start and end.")

  expect_equal(PeriodMean(array(1:10, c(time = 10))), 5.5)
  data <- array(1:24, c(sdate = 2, time = 3, lon = 4))
  expect_equal(PeriodMean(data),
    array(c(3,4,9,10,15,16,21,22), c(sdate = 2, lon = 4)))
})

test_that("seasonal", {

  exp <- CSTools::lonlat_prec
  exp$data <- array(1:(1 * 3 * 214 * 2),
                    c(memb = 1, sdate = 3, ftime = 214, lon = 2))
  exp$Dates[[1]] <- c(seq(as.Date("01-04-2000", format = "%d-%m-%Y"),
                     as.Date("31-10-2000", format = "%d-%m-%Y"), by = 'day'),
                 seq(as.Date("01-04-2001", format = "%d-%m-%Y"),
                     as.Date("31-10-2001", format = "%d-%m-%Y"), by = 'day'),
                 seq(as.Date("01-04-2002", format = "%d-%m-%Y"),
                     as.Date("31-10-2002", format = "%d-%m-%Y"), by = 'day'))
  output <- exp
  output$data <- array(c(mean(exp$data[1,1,21:82,1]), mean(exp$data[1,2,21:82,1]),
                         mean(exp$data[1,3,21:82,1]), mean(exp$data[1,1,21:82,2]),
                         mean(exp$data[1,2,21:82,2]), mean(exp$data[1,3,21:82,2])),
                       c(memb = 1, sdate = 3, lon = 2))
  expect_equal(
               CST_PeriodMean(exp, start = list(21, 4), end = list(21, 6))$data,
               output$data)



})
