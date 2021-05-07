context("Generic tests")
  #source("R/zzz.R")
  #source("R/SelectPeriodOnDates.R")
  #source("R/SelectPeriodOnData.R")
  library(s2dv)
test_that("Sanity checks", {
    #source("csindicators/R/AbsToProbs.R")
  expect_error(SelectPeriodOnDates('x', start = list(1,1), end = list(1,1)), "invalid 'trim' argument")
  # Lluis issue #8:
  dates <- c(seq(as.Date("02-05-1993", "%d-%m-%Y", tz = 'UTC'),
               as.Date("01-12-1993","%d-%m-%Y", tz = 'UTC'), "day"),
           seq(as.Date("02-05-1994", "%d-%m-%Y", tz = 'UTC'),
               as.Date("01-12-1994","%d-%m-%Y", tz = 'UTC'), "day"),
           seq(as.Date("02-05-1995", "%d-%m-%Y", tz = 'UTC'),
               as.Date("01-12-1995","%d-%m-%Y", tz = 'UTC'), "day"))
  dim(dates) <- c(time = 214, file_date = 3)
  output <- c(seq(as.Date("21-06-1993", "%d-%m-%Y", tz = 'UTC'),
               as.Date("21-09-1993","%d-%m-%Y", tz = 'UTC'), "day"),
           seq(as.Date("21-06-1994", "%d-%m-%Y", tz = 'UTC'),
               as.Date("21-09-1994","%d-%m-%Y", tz = 'UTC'), "day"),
           seq(as.Date("21-06-1995", "%d-%m-%Y", tz = 'UTC'),
               as.Date("21-09-1995","%d-%m-%Y", tz = 'UTC'), "day"))
  dim(output) <- c(time = 93, file_date = 3)
  expect_equal(SelectPeriodOnDates(dates, list(21,6),list(21,9), time_dim = 'time'), output)
  dates <- s2dv::Reorder(dates, c('file_date', 'time'))
  output <- s2dv::Reorder(output, c('file_date', 'time'))
  expect_equal(SelectPeriodOnDates(dates, list(21,6),list(21,9), time_dim = 'time'), output)
})

test_that("Decadal", { 
 # -------- DECADAL ----------#
  # decadal: 1 sdate several consequtive years:
  dates <- seq(as.Date("01-01-2000", "%d-%m-%Y", tz = 'UTC'),
               as.Date("31-12-2005","%d-%m-%Y", tz = 'UTC'), "day")
  # No dims -> test .position
  output <- c(
      seq(as.Date("2000-02-01", "%Y-%m-%d"), as.Date("2000-02-10", "%Y-%m-%d"), 'day'),
      seq(as.Date("2001-02-01", "%Y-%m-%d"), as.Date("2001-02-10", "%Y-%m-%d"), 'day'),
      seq(as.Date("2002-02-01", "%Y-%m-%d"), as.Date("2002-02-10", "%Y-%m-%d"), 'day'),
      seq(as.Date("2003-02-01", "%Y-%m-%d"), as.Date("2003-02-10", "%Y-%m-%d"), 'day'),
      seq(as.Date("2004-02-01", "%Y-%m-%d"), as.Date("2004-02-10", "%Y-%m-%d"), 'day'),
      seq(as.Date("2005-02-01", "%Y-%m-%d"), as.Date("2005-02-10", "%Y-%m-%d"), 'day'))
  dim(output) <- c(ftime = 60)
  expect_equal(
    SelectPeriodOnDates(dates, start = list(1, 2), end = list(10, 2)),
    output)
  data <- array(1:(length(dates)*3),
                c(memb = 1, ftime = length(dates), lon = 3))

  expect_equal(
    SelectPeriodOnData(data, dates, start = list(1, 2), end = list(10, 2)),

    array(c(c(32:41, 398:407, 763:772, 1128:1137, 1493:1502, 1859:1868),
            c(32:41, 398:407, 763:772, 1128:1137, 1493:1502, 1859:1868) + 2192,
            c(32:41, 398:407, 763:772, 1128:1137, 1493:1502, 1859:1868) + 2 * 2192),
          c(ftime = 60, memb = 1, lon = 3)))    
  
  output2 <- c(
      seq(as.Date("2000-02-01", "%Y-%m-%d"), as.Date("2000-04-10", "%Y-%m-%d"), 'day'),
      seq(as.Date("2001-02-01", "%Y-%m-%d"), as.Date("2001-04-10", "%Y-%m-%d"), 'day'),
      seq(as.Date("2002-02-01", "%Y-%m-%d"), as.Date("2002-04-10", "%Y-%m-%d"), 'day'),
      seq(as.Date("2003-02-01", "%Y-%m-%d"), as.Date("2003-04-10", "%Y-%m-%d"), 'day'),
      seq(as.Date("2004-02-01", "%Y-%m-%d"), as.Date("2004-04-10", "%Y-%m-%d"), 'day'),
      seq(as.Date("2005-02-01", "%Y-%m-%d"), as.Date("2005-04-10", "%Y-%m-%d"), 'day'))
  dim(output2) <- c(ftime = 416)
  expect_equal(
    SelectPeriodOnDates(dates, start = list(1, 2), end = list(10, 4)),
    output2)
  
  expect_equal(
    SelectPeriodOnData(data, dates, start = list(1, 2), end = list(10, 4)),
    array(c(c(32:101, 398:466, 763:831, 1128:1196, 1493:1562, 1859:1927),
            c(32:101, 398:466, 763:831, 1128:1196, 1493:1562, 1859:1927) + 2192,
            c(32:101, 398:466, 763:831, 1128:1196, 1493:1562, 1859:1927) + 2 * 2192),
          c(ftime = 416, memb = 1, lon = 3)))

  # 1 dim -> test Apply
  dim(dates) <- c(ftime = length(dates))
  expect_equal(
    SelectPeriodOnDates(dates, start = list(1, 2), end = list(10, 2)),
    output) # no need to check on Data, repited
  expect_equal(
    SelectPeriodOnDates(dates, start = list(1, 2), end = list(10, 4)),
    output2) # no need to check on Data, repited
  
  # decadal: 5 sdates several consequtive years
  dates <- rep(seq(as.Date("01-01-2000", "%d-%m-%Y", tz = 'UTC'),
               as.Date("31-12-2005","%d-%m-%Y", tz = 'UTC'), "day"), 5)
  dim(dates) <- c(ftime = 2192, sdate = 5)
  output3 <- rep(output, 5)
  dim(output3) <- c(ftime = 60, sdate = 5)
  expect_equal(
    SelectPeriodOnDates(dates, start = list(1, 2), end = list(10, 2)),
    output3)

  data <- array(1:(length(dates)*3),
                c(memb = 1, sdate = 5, ftime = length(dates)/5, lon = 3))
  expect_equal( #To be extended for all sdate dimensions:
    SelectPeriodOnData(data, dates, start = list(1, 2), end = list(10, 2))[,1,1,1],
    c(1:10 * 5 + 151, 1:10 * 5 + 1981, 1:10 * 5 + 3806,
              1:10 * 5 + 5631, 1:10 * 5 + 7456, 1:10 * 5 + 9286))
  output4 <- rep(output2, 5)
  dim(output4) <- c(ftime = 416, sdate = 5)
  expect_equal(
    SelectPeriodOnDates(dates, start = list(1, 2), end = list(10, 4)),
    output4)
  
  expect_equal( #To be extended for all ftime dimensions:
    SelectPeriodOnData(data, dates, start = list(1, 2), end = list(10, 4))[1,1,,1],
    156:160)

  # Multiple dims: sdate, fyear, ftime
  library(CSTools)
  dates <- SplitDim(dates, indices = dates[,1],
                              split_dim = 'ftime', freq = 'year')
  dates <- as.POSIXct(dates * 24 * 3600, origin = '1970-01-01', tz = 'UTC')
  output5 <- SplitDim(output3, indices = output3[,1], split_dim = 'ftime' , freq = 'year')
  output5 <- as.POSIXct(output5 * 24 * 3600, origin = '1970-01-01', tz = 'UTC')
  expect_equal(
    SelectPeriodOnDates(dates, start = list(1, 2), end = list(10, 2)),
    output5)
  data <- array(1:(366*6*5*3),
               c(memb = 1, sdate = 5, year = 6, ftime = 366, lon = 3))
  expect_equal(
    SelectPeriodOnData(data, dates, start = list(1, 2), end = list(10, 2)),
    InsertDim(Reorder(data[,,,32:41,], c('ftime', 'sdate', 'year', 'lon')),
           len = 1, pos = 2, name = 'memb'))
  output6 <- SplitDim(output4, indices = output4[,1], split_dim = 'ftime' , freq = 'year')
  output6 <- as.POSIXct(output6 * 24 * 3600, origin = '1970-01-01', tz = 'UTC')
  expect_equal(
    SelectPeriodOnDates(dates, start = list(1, 2), end = list(10, 4)),
    output6)
  #expect_equal( # to be fixed:
  #  SelectPeriodOnData(data, dates, start = list(1, 2), end = list(10, 4)),
  #   (931:935), outer(seq(931, 3001, 30), 0:4, '+')
  #  InsertDim(Reorder(data[,,,32:41,], c('ftime', 'sdate', 'year', 'lon')),
  #         len = 1, pos = 2, name = 'memb'))
})


test_that("Seasonal", {
  # 1 start month, select the required 'ftime' of each 'sdate' in-between the entire timeseries 
    dates <- c(seq(as.Date("01-04-2000", format = "%d-%m-%Y"),
                   as.Date("31-10-2000", format = "%d-%m-%Y"), by = 'day'),
               seq(as.Date("01-04-2001", format = "%d-%m-%Y"),
                   as.Date("31-10-2001", format = "%d-%m-%Y"), by = 'day'),
               seq(as.Date("01-04-2002", format = "%d-%m-%Y"),
                   as.Date("31-10-2002", format = "%d-%m-%Y"), by = 'day'),
               seq(as.Date("01-04-2003", format = "%d-%m-%Y"),
                   as.Date("31-10-2003", format = "%d-%m-%Y"), by = 'day'))

    output <- c(seq(as.Date("21-04-2000", format = "%d-%m-%Y"),
                    as.Date("21-06-2000", format = "%d-%m-%Y"), by = 'day'),
                seq(as.Date("21-04-2001", format = "%d-%m-%Y"),
                     as.Date("21-06-2001", format = "%d-%m-%Y"), by = 'day'),
                seq(as.Date("21-04-2002", format = "%d-%m-%Y"),
                     as.Date("21-06-2002", format = "%d-%m-%Y"), by = 'day'),
                seq(as.Date("21-04-2003", format = "%d-%m-%Y"),
                   as.Date("21-06-2003", format = "%d-%m-%Y"), by = 'day'))
    dim(output) <- c(ftime = (30 - 20 + 31 + 21) * 4)
    expect_equal(
                 SelectPeriodOnDates(dates, start = list(21, 4), end = list(21, 6)),
                 output)
  
  # following the above case, and select the data
    data <- array(1:(5 * 4 * 214 * 2),             
                  c(memb = 5, sdate = 4, ftime = 214, lon = 2))
    dim(dates) <- c(ftime = 214, sdate = 4)
  
    expect_equal(
                 SelectPeriodOnData(data, dates, start = list(21, 4), end = list(21, 6))[,1,1,1],
                 data[1,1,21:82,1])


# when selecting the days across two years
    dates <- seq(as.Date("2000-01-01", "%Y-%m-%d"), as.Date("2003-12-31", "%Y-%m-%d"), 'day')
    
    output1 <- c(seq(as.Date("01-01-2000", format = "%d-%m-%Y"),
                     as.Date("31-01-2000", format = "%d-%m-%Y"), by = 'day'),
                 seq(as.Date("01-12-2000", format = "%d-%m-%Y"),
                     as.Date("31-01-2001", format = "%d-%m-%Y"), by = 'day'),
                 seq(as.Date("01-12-2001", format = "%d-%m-%Y"),
                     as.Date("31-01-2002", format = "%d-%m-%Y"), by = 'day'),
                 seq(as.Date("01-12-2002", format = "%d-%m-%Y"),
                     as.Date("31-01-2003", format = "%d-%m-%Y"), by = 'day'),
                 seq(as.Date("01-12-2003", format = "%d-%m-%Y"),
                     as.Date("31-12-2003", format = "%d-%m-%Y"), by = 'day'))
    dim(output1) <- c(ftime = 31 * 8)

    expect_equal(
                 SelectPeriodOnDates(dates, start = list(1, 12), end = list(31, 1)),
                 output1)
  # following the above case, and select the data
    data1 <- array(1:(length(dates) * 2),
                   c(memb = 1, ftime = length(dates), lon = 2))
    expect_equal(
                 SelectPeriodOnData(data1, dates, start = list(1, 12), end = list(31, 1)),
                 array(c(c(1:31, 336:397, 701:762, 1066:1127, 1431:1461),
                         c(1:31, 336:397, 701:762, 1066:1127, 1431:1461) + 1461),
                       c(ftime = 31 * 8, memb = 1, lon = 2)))
})

