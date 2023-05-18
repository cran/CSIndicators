#'Merge a Reference To Experiments 
#'
#'Some indicators are defined for specific temporal periods (e.g.: summer from 
#'June 21st to September 21st). If the initialization forecast date is later 
#'than the one required for the indicator (e.g.: July 1st), the user may want to 
#'merge past observations, or other references, to the forecast (or hindcast) 
#'to compute the indicator. The function \code{MergeObs2Exp} takes care of this 
#'steps. If the forecast simulation doesn't cover the required period because it 
#'is initialized too early (e.g.: Initialization on November 1st the forecast 
#'covers until the beginning of June next year), a climatology (or other 
#'references) could be added at the end of the forecast lead time to cover the 
#'desired period (e.g.: until the end of summer).
#'
#'@param data1 An 's2dv_cube' object as provided function \code{CST_Load} in 
#'  package CSTools.
#'@param data2 An 's2dv_cube' object as provided function \code{CST_Load} in 
#'  package CSTools.
#'@param start1 A list to define the initial date of the period to select from 
#'  data1 by providing a list of two elements: the initial date of the period 
#'  and the initial month of the period. 
#'@param end1 A list to define the final date of the period to select from 
#'  data1 by providing a list of two elements: the final day of the period and 
#'  the final month of the period.
#'@param start2 A list to define the initial date of the period to select from 
#'  data2 by providing a list of two elements: the initial date of the period 
#'  and the initial month of the period.
#'@param end2 A list to define the final date of the period to select from 
#'  data2 by providing a list of two elements: the final day of the period and 
#'  the final month of the period.
#'@param time_dim A character string indicating the name of the temporal 
#'  dimension. By default, it is set to 'ftime'. More than one dimension name 
#'  matching the dimensions provided in the object \code{data$data} can be 
#'  specified. This dimension is required to subset the data in a requested period.
#'@param sdate_dim A character string indicating the name of the dimension in 
#'  which the initialization dates are stored. 
#'@param ncores An integer indicating the number of cores to use in parallel 
#'  computation.
#'@return A 's2dv_cube' object containing the indicator in the element 
#'  \code{data}.
#'
#'@examples
#'data_dates <- c(seq(as.Date("01-07-1993", "%d-%m-%Y", tz = 'UTC'),
#'                    as.Date("01-12-1993","%d-%m-%Y", tz = 'UTC'), "day"),
#'                seq(as.Date("01-07-1994", "%d-%m-%Y", tz = 'UTC'),
#'                    as.Date("01-12-1994","%d-%m-%Y", tz = 'UTC'), "day"))
#'dim(data_dates) <- c(ftime = 154, sdate = 2)
#'data <- NULL
#'data$data <- array(1:(2*154*2), c(ftime = 154, sdate = 2, member= 2))
#'data$attrs$Dates<- data_dates
#'class(data) <- 's2dv_cube'
#'ref_dates <- seq(as.Date("01-01-1993", "%d-%m-%Y", tz = 'UTC'),
#'                 as.Date("01-12-1994","%d-%m-%Y", tz = 'UTC'), "day")
#'dim(ref_dates) <- c(ftime = 350, sdate = 2)
#'ref <- NULL
#'ref$data <- array(1001:1700, c(ftime = 350, sdate = 2))
#'ref$attrs$Dates <- ref_dates
#'class(ref) <- 's2dv_cube'
#'new_data <- CST_MergeRefToExp(data1 = ref, data2 = data, 
#'                              start1 = list(21, 6), end1 = list(30, 6),
#'                              start2 = list(1, 7), end2 = list(21, 9))
#' 
#'@import multiApply
#'@export
CST_MergeRefToExp <- function(data1, data2, start1, end1, start2, end2,
                              time_dim = 'ftime', sdate_dim = 'sdate',
                              ncores = NULL) {
  # Check 's2dv_cube'
  if (!inherits(data1, 's2dv_cube')) {
    stop("Parameter 'ref' must be of the class 's2dv_cube'.")
  }
  if (!inherits(data2, 's2dv_cube')) {
    stop("Parameter 'data' must be of the class 's2dv_cube'.")
  }
  # Dates subset of data1
  dates1 <- NULL
  if (!is.null(start1) && !is.null(end1)) {
    if (is.null(dim(data1$attrs$Dates))) {
      warning("Dimensions in 'data1' element 'attrs$Dates' are missed and ",
              "all data would be used.")
      start <- NULL
      end <- NULL
    } else {
      dates1 <- data1$attrs$Dates
    }
  }
  # Dates subset of data2
  dates2 <- NULL
  if (!is.null(start2) && !is.null(end2)) {
    if (is.null(dim(data2$attrs$Dates))) {
      warning("Dimensions in 'data2' element 'attrs$Dates' are missed and ",
              "all data would be used.")
      start <- NULL
      end <- NULL
    } else {
      dates2 <- data2$attrs$Dates
    }
  }

  data1$data <- MergeRefToExp(data1 = data1$data, dates1 = dates1,
                              start1 = start1, end1 = end1,
                              data2 = data2$data, dates2 = dates2,
                              start2, end2, time_dim = time_dim,
                              sdate_dim = sdate_dim, ncores = ncores)
  if (!is.null(dates1)) {
    data1$attrs$Dates <- SelectPeriodOnDates(dates1, start = start1, end = end1,
                                             time_dim = time_dim)
  }
  if (!is.null(dates2)) {
    data2$attrs$Dates <- SelectPeriodOnDates(dates2, start = start2,
                                             end = end2, time_dim = time_dim)
  }

  # TO DO CONCATENATE DATES
  remove_dates1_dim <- FALSE
  remove_dates2_dim <- FALSE
  if (!is.null(data1$attrs$Dates) & !is.null(data2$attrs$Dates)) {
    if (is.null(dim(data1$attrs$Dates))) {
      remove_dates1_dim <- TRUE
      dim(data1$attrs$Dates) <- length(data1$attrs$Dates)
      names(dim(data1$attrs$Dates)) <- time_dim
    }
    if (is.null(dim(data2$attrs$Dates))) {
      remove_dates2_dim <- TRUE
      dim(data2$attrs$Dates) <- length(data2$attrs$Dates)
      names(dim(data2$attrs$Dates)) <- time_dim
    }
  }
  res <- Apply(list(data1$attrs$Dates, data2$attrs$Dates), target_dims = time_dim,
               c, output_dims = time_dim, ncores = ncores)$output1
  if (inherits(data1$attrs$Dates, 'Date')) {
    data1$attrs$Dates <- as.Date(res, origin = '1970-01-01')
  } else {
    data1$attrs$Dates <- as.POSIXct(res*3600*24, origin = '1970-01-01', tz = 'UTC')
  }

  if (remove_dates1_dim) {
    dim(data1$attrs$Dates) <- NULL
  }
  if (remove_dates2_dim) {
    dim(data2$attrs$Dates) <- NULL
  }

  return(data1)
}

#'Merge a Reference To Experiments 
#'
#'Some indicators are defined for specific temporal periods (e.g.: summer from 
#'June 21st to September 21st). If the initialization forecast date is later 
#'than the one required for the indicator (e.g.: July 1st), the user may want to 
#'merge past observations, or other reference, to the forecast (or hindcast) to 
#'compute the indicator. The function \code{MergeObs2Exp} takes care of this 
#'steps.
#'
#'@param data1 A multidimensional array with named dimensions.
#'@param dates1 A vector of dates or a multidimensional array of dates with 
#'  named dimensions matching the dimensions on parameter 'data1'.
#'@param data2 A multidimensional array with named dimensions.
#'@param dates2 A vector of dates or a multidimensional array of dates with 
#'  named dimensions matching the dimensions on parameter 'data2'.
#'@param start1 A list to define the initial date of the period to select from 
#'  data1 by providing a list of two elements: the initial date of the period 
#'  and the initial month of the period.
#'@param end1 A list to define the final date of the period to select from 
#'  data1 by providing a list of two elements: the final day of the period and 
#'  the final month of the period.
#'@param start2 A list to define the initial date of the period to select from 
#'  data2 by providing a list of two elements: the initial date of the period 
#'  and the initial month of the period.
#'@param end2 A list to define the final date of the period to select from 
#'  data2 by providing a list of two elements: the final day of the period and 
#'  the final month of the period.
#'@param time_dim A character string indicating the name of the temporal 
#'  dimension. By default, it is set to 'ftime'. More than one dimension name 
#'  matching the dimensions provided in the object \code{data$data} can be 
#'  specified. This dimension is required to subset the data in a requested 
#'  period.
#'@param sdate_dim A character string indicating the name of the dimension in 
#'  which the initialization dates are stored. 
#'@param ncores An integer indicating the number of cores to use in parallel 
#'  computation.
#'
#'@return A multidimensional array with named dimensions.
#'
#'@examples
#'data_dates <- c(seq(as.Date("01-07-1993", "%d-%m-%Y", tz = 'UTC'),
#'                    as.Date("01-12-1993","%d-%m-%Y", tz = 'UTC'), "day"),
#'                seq(as.Date("01-07-1994", "%d-%m-%Y", tz = 'UTC'),
#'                    as.Date("01-12-1994","%d-%m-%Y", tz = 'UTC'), "day"))
#'dim(data_dates) <- c(time = 154, sdate = 2)
#'ref_dates <- seq(as.Date("01-01-1993", "%d-%m-%Y", tz = 'UTC'),
#'                 as.Date("01-12-1994","%d-%m-%Y", tz = 'UTC'), "day")
#'dim(ref_dates) <- c(time = 350, sdate = 2)
#'ref <- array(1001:1700, c(time = 350, sdate = 2))
#'data <- array(1:(2*154*2), c(time = 154, sdate = 2, member= 2))
#'new_data <- MergeRefToExp(data1 = ref, dates1 = ref_dates, start1 = list(21, 6),
#'                          end1 = list(30, 6), data2 = data, dates2 = data_dates,
#'                          start2 = list(1, 7), end = list(21, 9), 
#'                          time_dim = 'time')
#'
#'@import multiApply
#'@export
MergeRefToExp <- function(data1, dates1, start1, end1, data2, dates2, start2, 
                          end2, time_dim = 'ftime', sdate_dim = 'sdate',
                          ncores = NULL) {
  # Input checks
  # data
  if (!is.array(data1)) {
    dim(data1) <- c(length(data1))
    names(dim(data1)) <- time_dim
  }
  if (!is.array(data2)) {
    dim(data2) <- c(length(data2))
    names(dim(data2)) <- time_dim
  }
  # dates
  if (!is.null(dates1) & !is.null(dates2)) {
    if (is.null(dim(dates1))) {
      warning("Dimensions in 'dates1' element are missed and ",
              "all data would be used.")
      dim(dates1) <- length(dates1)
      names(dim(dates1)) <- time_dim
    }
    if (is.null(dim(dates2))) {
      warning("Dimensions in 'dates2' element are missed and ",
              "all data would be used.")
      dim(dates2) <- length(dates2)
      names(dim(dates2)) <- time_dim
    }
    data1 <- SelectPeriodOnData(data1, dates = dates1, start = start1,
                                end = end1, time_dim = time_dim, ncores = ncores)
  }

  # Check if data2 has dimension sdate_dim and it should be added to data1:
  if ((sdate_dim %in% names(dim(data2))) && dim(data2)[sdate_dim] > 1 && 
      !sdate_dim %in% names(dim(data1))) {
    dim(data1) <- c(length(data1)/dim(data2)[sdate_dim], dim(data2)[sdate_dim])
    names(dim(data1)) <- c(time_dim, sdate_dim)
  }
  # Check if data1 has dimension sdate_dim and it should be added to data2:
  if ((sdate_dim %in% names(dim(data1))) && dim(data1)[sdate_dim] > 1 &&
      !sdate_dim %in% names(dim(data2))) {
    dim(data2) <- c(length(data2)/dim(data1)[sdate_dim], dim(data1)[sdate_dim])
    names(dim(data2)) <- c(time_dim, sdate_dim)
  }
  # Check if data1 needs to be extended to the length of the dimensions of data2:
  if (length(dim(data2)) != length(dim(data1))) {
    dif_dims <- which(names(dim(data2)) %in% names(dim(data1)) == FALSE)
    if (length(dif_dims) > 0) {
      for (i in dif_dims) {
        data1 <- .insertdim(data1, posdim = i, lendim = dim(data2)[i], 
                            name = names(dim(data2))[i])
      }
    }
  }
  # Check if data2 needs to be extended to the length of the dimensions of data1:
  if (length(dim(data1)) != length(dim(data2))) {
    dif_dims <- which(names(dim(data1)) %in% names(dim(data2)) == FALSE)
    if (length(dif_dims) > 0) {
      for (i in dif_dims) {
        data2 <- .insertdim(data2, posdim = i, lendim = dim(data1)[i],
                            name = names(dim(data1))[i])
      }
    }
  }
  if (!is.null(dates2)) {
    data2 <- SelectPeriodOnData(data2, dates = dates2, start = start2,
                                end = end2, time_dim = time_dim, ncores = ncores)
  }
  data1 <- Apply(list(data1, data2), target_dims = time_dim, fun = 'c',
                 output_dims = time_dim, ncores = ncores)$output1
  return(data1)
}
   

