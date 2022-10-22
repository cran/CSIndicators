#'Accumulation of a variable when Exceeding (not exceeding) a Threshold
#'
#'The accumulation (sum) of a variable in the days (or time steps) that the 
#'variable is exceeding (or not exceeding) a threshold during a period. The 
#'threshold provided must be in the same units than the variable units, i.e. to 
#'use a percentile as a scalar, the function \code{Threshold} or 
#'\code{QThreshold} may be needed. Providing mean daily temperature data, the 
#'following agriculture indices for heat stress can be obtained by using this 
#'function:
#'\itemize{
#'  \item\code{GDD}{Summation of daily differences between daily average 
#'                  temperatures and 10°C between April 1st and October 31st}
#'}
#'
#'@param data An 's2dv_cube' object as provided by function \code{CST_Load} in 
#'  package CSTools.
#'@param threshold An 's2dv_cube' object as output of a 'CST_' function in the
#'  same units as parameter 'data' and with the common dimensions of the element 
#'  'data' of the same length. A single scalar is also possible.
#'@param op An operator '>' (by default), '<', '>=' or '<='. 
#'@param diff A logical value indicating whether to accumulate the difference
#'  between data and threshold (TRUE) or not (FALSE by default).
#'@param start An optional parameter to defined the initial date of the period
#'  to select from the data by providing a list of two elements: the initial
#'  date of the period and the initial month of the period. By default it is set
#'  to NULL and the indicator is computed using all the data provided in
#'  \code{data}.
#'@param end An optional parameter to defined the final date of the period to
#'  select from the data by providing a list of two elements: the final day of
#'  the period and the final month of the period. By default it is set to NULL
#'  and the indicator is computed using all the data provided in \code{data}.
#'@param time_dim A character string indicating the name of the dimension to
#'  compute the indicator. By default, it is set to 'ftime'. More than one
#'  dimension name matching the dimensions provided in the object
#'  \code{data$data} can be specified.
#'@param na.rm A logical value indicating whether to ignore NA values (TRUE) or
#'  not (FALSE). 
#'@param ncores An integer indicating the number of cores to use in parallel
#'  computation.
#'
#'@return A 's2dv_cube' object containing the indicator in the element \code{data}.
#'
#'@examples
#'exp <- NULL
#'exp$data <- array(rnorm(216)*200, dim = c(dataset = 1, member = 2, sdate = 3, 
#'                  ftime = 9, lat = 2, lon = 2))
#'class(exp) <- 's2dv_cube'
#'DOT <- CST_AccumulationExceedingThreshold(exp, threshold = 280)
#' 
#'@import multiApply
#'@export
CST_AccumulationExceedingThreshold <- function(data, threshold, op = '>', 
                                               diff = FALSE,
                                               start = NULL, end = NULL,
                                               time_dim = 'ftime',
                                               na.rm = FALSE, ncores = NULL) {
  if (!inherits(data, 's2dv_cube')) {
    stop("Parameter 'data' must be of the class 's2dv_cube', ",
         "as output by CSTools::CST_Load.")
  }
  # when subsetting is needed, dimensions are also needed:
  if (!is.null(start) && !is.null(end)) {
    if (is.null(dim(data$Dates$start))) {
      if (length(data$Dates$start) != dim(data$data)[time_dim]) {
        if (length(data$Dates$start) == 
            prod(dim(data$data)[time_dim] * dim(data$data)['sdate'])) {
          dim(data$Dates$start) <- c(dim(data$data)[time_dim],
                                     dim(data$data)['sdate'])
        } else {
          warning("Dimensions in 'data' element 'Dates$start' are missed and ",
                  "all data would be used.")
        }
      }
    }
  }
 if (inherits(threshold, 's2dv_cube')) {
    threshold <- threshold$data
  }
 total <- AccumulationExceedingThreshold(data$data, data$Dates[[1]],
                                         threshold = threshold, op = op, diff = diff,
                                         start = start, end = end, time_dim = time_dim,
                                         na.rm = na.rm, ncores = ncores)
  data$data <- total
  if (!is.null(start) && !is.null(end)) {
     data$Dates <- SelectPeriodOnDates(dates = data$Dates$start,
                                       start = start, end = end,
                                       time_dim = time_dim, ncores = ncores)
  }
  return(data)
}
#'Accumulation of a variable when Exceeding (not exceeding) a Threshold
#'
#'The accumulation (sum) of a variable in the days (or time steps) that the 
#'variable is exceeding (or not exceeding) a threshold during a period. The 
#'threshold provided must be in the same units than the variable units, i.e. to 
#'use a percentile as a scalar, the function \code{Threshold} or 
#'\code{QThreshold} may be needed. Providing mean daily temperature data, the 
#'following agriculture indices for heat stress can be obtained by using this 
#'function:
#'\itemize{
#'  \item\code{GDD}{Summation of daily differences between daily average 
#'                  temperatures and 10°C between April 1st and October 31st}
#'}
#'
#'@param data A multidimensional array with named dimensions.
#'@param threshold a multidimensional array with named dimensions in the same
#'  units as parameter 'data' and with the common dimensions of the element 
#'  'data' of the same length.
#'@param op An operator '>' (by default), '<', '>=' or '<='.
#'@param diff A logical value indicating whether to accumulate the difference
#'  between data and threshold (TRUE) or not (FALSE by default).
#'@param dates A vector of dates or a multidimensional array of dates with named 
#'  dimensions matching the dimensions on parameter 'data'. By default it is 
#'  NULL, to select a period this parameter must be provided.
#'@param start An optional parameter to defined the initial date of the period 
#'  to select from the data by providing a list of two elements: the initial
#'  date of the period and the initial month of the period. By default it is set
#'  to NULL and the indicator is computed using all the data provided in
#'  \code{data}.
#'@param end An optional parameter to defined the final date of the period to
#'  select from the data by providing a list of two elements: the final day of
#'  the period and the final month of the period. By default it is set to NULL
#'  and the indicator is computed using all the data provided in \code{data}.
#'@param time_dim A character string indicating the name of the dimension to
#'  compute the indicator. By default, it is set to 'ftime'. More than one
#'  dimension name matching the dimensions provided in the object
#'  \code{data$data} can be specified.
#'@param na.rm A logical value indicating whether to ignore NA values (TRUE) or 
#'  not (FALSE). 
#'@param ncores An integer indicating the number of cores to use in parallel
#'  computation.
#'
#'@return A multidimensional array with named dimensions containing the 
#'indicator in the element \code{data}.
#'
#'@import multiApply
#'@examples
#'# Assuming data is already (tasmax + tasmin)/2 - 10
#'data <- array(rnorm(5 * 3 * 214 * 2, mean = 25, sd = 3),
#'                    c(memb = 5, sdate = 3, time = 214, lon = 2)) 
#'Dates <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2000", format = "%d-%m-%Y"), by = 'day'),
#'           seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2001", format = "%d-%m-%Y"), by = 'day'),
#'           seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2002", format = "%d-%m-%Y"), by = 'day'))
#'GDD <- AccumulationExceedingThreshold(data, threshold = 0, start = list(1, 4),
#'                                      end = list(31, 10))
#'@export
AccumulationExceedingThreshold <- function(data, threshold, op = '>',
                                           diff = FALSE,
                                           dates = NULL, start = NULL, end = NULL,
                                           time_dim = 'time', na.rm = FALSE,
                                           ncores = NULL) {
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!is.numeric(data)) {
    stop("Parameter 'data' must be numeric.")
  }
  if (!is.array(data)) {
    dim(data) <- length(data)
    names(dim(data)) <- time_dim
  }
  if (is.null(threshold)) {
    stop("Parameter 'threshold' cannot be NULL.")
  }
   if (!is.numeric(threshold)) {
    stop("Parameter 'threshold' must be numeric.")
  }
  if (!is.array(threshold) && length(threshold) > 1) {
    dim(threshold) <- length(threshold)
    names(dim(threshold)) <- time_dim
  } else if (length(threshold) == 1) {
    dim(threshold) <- NULL
  }
  if (is.null(names(dim(data)))) {
    stop("Parameter 'data' must have named dimensions.")
  }
  if (is.null(names(dim(threshold))) && length(threshold) > 1) {
    stop("Parameter 'threshold' must have named dimensions.")
  }
  if (!is.null(dates)) {
    if (!is.null(start) && !is.null(end)) {
      if (!any(c(is.list(start), is.list(end)))) {
        stop("Parameter 'start' and 'end' must be lists indicating the ",
             "day and the month of the period start and end.")
      }
      if (all(time_dim %in% names(dim(threshold)))) {
        if (dim(threshold)[time_dim] == dim(data)[time_dim]) {
            threshold <- SelectPeriodOnData(threshold, dates, start, end,
                                            time_dim = time_dim, ncores = ncores)
        }
      }
      data <- SelectPeriodOnData(data, dates, start, end, 
                                 time_dim = time_dim, ncores = ncores)
    }
  }
  if (diff == TRUE) {
    data <- Apply(list(data, threshold),
                  target_dims = list(time_dim, NULL),
                  fun = function(x, y) {x - y}, ncores = ncores)$output1
    dim(data) <- dim(data)[-length(dim(data))]
    threshold <- 0
  } 
  if (is.null(dim(threshold))) {
    total <- Apply(list(data), target_dims = time_dim,
                   fun = .sumexceedthreshold,
                   y = threshold, op = op, na.rm = na.rm,
                   ncores = ncores)$output1
  } else if (all(time_dim %in% names(dim(threshold)))) {
    total <- Apply(list(data, threshold),
                   target_dims = list(time_dim, time_dim),
                   fun = .sumexceedthreshold, op = op, na.rm = na.rm,
                   ncores = ncores)$output1
  } else if (any(time_dim %in% names(dim(threshold)))) {
    total <- Apply(list(data, threshold),
                   target_dims = list(time_dim,
                                      time_dim[time_dim %in% names(dim(threshold))]),
                   fun = .sumexceedthreshold, op = op, na.rm = na.rm,
                   ncores = ncores)$output1
  } else {
    total <- Apply(list(data, threshold),
                   target_dims = list(time_dim, NULL),
                   fun = .sumexceedthreshold, op = op, na.rm = na.rm,
                   ncores = ncores)$output1
  }
  return(total)
}

.sumexceedthreshold <- function(x, y, op, na.rm) {
  if (op == '>') {
    res <- sum(x[x > y], na.rm = na.rm)
  } else if (op == '<') {
    res <- sum(x[x < y], na.rm = na.rm)
  } else if (op == '<=') {
    res <- sum(x[x <= y], na.rm = na.rm)
  } else {
    res <- sum(x[x >= y], na.rm = na.rm)
  }
  return(res)
}

