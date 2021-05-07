context("Generic tests")
test_that("Sanity checks", {
  #source("csindicators/R/MergeRefToExp.R")
  data_dates <- c(seq(as.Date("01-07-1993", "%d-%m-%Y", tz = 'UTC'),
                      as.Date("01-12-1993", "%d-%m-%Y", tz = 'UTC'), "day"),
                  seq(as.Date("01-07-1994", "%d-%m-%Y", tz = 'UTC'),
                      as.Date("01-12-1994","%d-%m-%Y", tz = 'UTC'), "day"))

  dim(data_dates) <- c(ftime = 154, sdate = 2)
  ref_dates <- seq(as.Date("01-01-1993", "%d-%m-%Y", tz = 'UTC'),
                   as.Date("01-12-1994", "%d-%m-%Y", tz = 'UTC'), "day")
  dim(ref_dates) <- c(ftime = 350, sdate = 2)
  ref <- array(1001:1700, c(ftime = 350, sdate = 2))
  data <- array(1:(2*154*2), c(ftime = 154, sdate = 2, member= 2))
  ref <- CSTools::s2dv_cube(data = ref, Dates = list(start = ref_dates,
                                                   end = ref_dates))
  data <- CSTools::s2dv_cube(data = data, Dates = list(start = data_dates,
                                            end = data_dates))
  
  expect_equal(CST_MergeRefToExp(data1 = ref, data2 = data, start1 = list(21, 6),
                                 end1 = list(30, 6), start2 = list(1, 7),  
                                 end2 = list(21, 9))$Dates, 
               SelectPeriodOnDates(ref_dates, start = list(21, 6), end = list(21,9)))

  output <- array(c(1172:1181, 1:83, 1537:1546, 155:237, 1172:1181, 309:391,
                    1537:1546, 463:545), c(ftime = 93, sdate = 2, member = 2)) 

  expect_equal(CST_MergeRefToExp(data1 = ref, data2 = data, start1 = list(21, 6),
                                 end1 = list(30, 6), start2 = list(1, 7),
                                 end2 = list(21, 9))$data, 
               output)
 })

test_that("Seasonal", {

  dates <- NULL 
  hcst.inityear <- 1993
  hcst.endyear <- 2017
  for (year in hcst.inityear:hcst.endyear){
    dates <- c(dates, format(seq(as.Date(paste0("01-04-",year), "%d-%m-%Y",
                                         tz = 'UTC'),
                                 as.Date(paste0("01-11-",year), "%d-%m-%Y",
                                         tz = 'UTC'), "day"),
                             "%Y-%m-%d"))
  }
  dates <- as.Date(dates, tz = 'UTC')
  dim.dates <- c(ftime=215, sweek = 1, sday = 1,
                 sdate=(hcst.endyear-hcst.inityear)+1)
  dim(dates) <- dim.dates

  ref <- array(1:(215*25), c(ftime = 215, sdate = 25))
  ref <- CSTools::s2dv_cube(data = ref, 
                            Dates = list(start = dates,
                                         end = dates))

  data <- array(1:(215*25*3), c(ftime = 215, sdate = 25, member=3))
  data <- CSTools::s2dv_cube(data = data, 
                             Dates = list(start = dates,
                                         end = dates))

  expect_equal(CST_MergeRefToExp(data1 = data, data2 = ref, start1 = list(21, 6),
                                 end1 = list(30, 6), start2 = list(1, 7),  
                                 end2 = list(21, 9))$Dates, 
               SelectPeriodOnDates(dates, start = list(21, 6), end = list(21,9)))
  
  expect_equal(CST_MergeRefToExp(data1 = ref, data2 = data, start1 = list(21, 6),
                                 end1 = list(30, 6), start2 = list(1, 7),  
                                 end2 = list(21, 9))$Dates, 
               SelectPeriodOnDates(dates, start = list(21, 6), end = list(21,9)))
})
