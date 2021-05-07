context("Generic tests")
test_that("Sanity checks", {
  #source("csindicators/R/AbsToProbs.R")
  expect_error(AbsToProbs('x'), "Parameter 'data' must be numeric.")
  expect_equal(AbsToProbs(1), array(1, c(sdate = 1, member = 1)))
  expect_equal(AbsToProbs(1, memb_dim = 'x'), array(1, c(sdate = 1, x = 1)))
  expect_error(AbsToProbs(data = NULL), "Parameter 'data' cannot be NULL.")
  expect_error(AbsToProbs(1, dates = '2000-01-01', end = 3, start = 4),
    "Parameter 'start' and 'end' must be lists indicating the day and the month of the period start and end.")
  expect_equal(AbsToProbs(1:10), array(seq(0.1, 1, 0.1), c(sdate = 1, member = 10)))
  data <- array(1:24, c(member = 3, sdate = 2, lon = 4))
  expect_equal(AbsToProbs(data), array(rep(0:1,12), c(sdate = 2, member = 3, lon = 4)))
})

test_that("Seasonal forecasts", {

  exp <- CSTools::lonlat_data$exp$data[1,1:3,1:3,,1:5,1:5]
  exp_probs <- AbsToProbs(exp)
  expect_equal(dim(exp)[3:5], dim(exp_probs)[3:5])
  expect_equal(round(exp_probs[,1,1,1,1]), c(1, 0, 1))
  exp <- exp[,1,,,] # one sdate
  expect_error(exp1_probs <- AbsToProbs(exp), 
               "Could not find dimension 'sdate' in 1th object provided in 'data'.")
  library(s2dv)
  exp1 <- InsertDim(exp, 2, 1, name = 'sdate')
  exp1_probs <- AbsToProbs(exp1)
  expect_equal(round(exp1_probs[1,,2,2,2]), c(1, 0, 1))
})
