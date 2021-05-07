context("Generic tests")
test_that("Sanity Checks", {
  #source("csindicators/R/PeriodAccumulation.R")
  expect_error(PeriodAccumulation('x'), "Parameter 'data' must be numeric.")
  expect_equal(PeriodAccumulation(1), 1)
  expect_equal(PeriodAccumulation(1, time_dim = 'x'), 1)
  expect_error(PeriodAccumulation(data = NULL), 
    "Parameter 'data' cannot be NULL.")
  expect_error(PeriodAccumulation(1, dates = '2000-01-01', end = 3, start = 4),
    paste("Parameter 'start' and 'end' must be lists indicating",
          "the day and the month of the period start and end."))
  expect_equal(PeriodAccumulation(1:10), 55)
  data <- array(1:24, c(sdate = 2, time = 3, lon = 4))
  expect_equal(PeriodAccumulation(data),
    array(c(9, 12, 27, 30, 45, 48, 63, 66), c(sdate = 2, lon = 4)))

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
  output$data <- array(c(sum(exp$data[1,1,21:82,1]), sum(exp$data[1,2,21:82,1]),
                         sum(exp$data[1,3,21:82,1]), sum(exp$data[1,1,21:82,2]),
                         sum(exp$data[1,2,21:82,2]), sum(exp$data[1,3,21:82,2])),
                       c(memb = 1, sdate = 3, lon = 2))

  expect_equal(CST_PeriodAccumulation(exp, start = list(21, 4),
    end = list(21, 6))$data, output$data)

})
