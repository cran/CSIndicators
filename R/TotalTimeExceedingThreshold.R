#'Total Time of a variable Exceeding (not exceeding) a Threshold
#'
#'The Total Time of a variable exceeding (or not) a Threshold returns the total number of days 
#'(if the data provided is daily, or the corresponding units to the data frequency provided)
#' that a variable is exceeding a threshold during a period. The threshold provided must be 
#'in the same units than the variable units, i.e. to use a percentile as a scalar, 
#'the function \code{AbsToProbs} or \code{QThreshold}  may be needed (see examples). 
#'Providing maximum temperature daily data, the following agriculture indices for heat stress can be obtained by using this function:
#'\itemize{
#'  \item\code{SU35}{Total count of days when daily maximum temperatures exceed 35°C in the seven months from the start month given (e.g. from April to October for start month of April).}
#'  \item\code{SU36}{Total count of days when daily maximum temperatures exceed 36 between June 21st and September 21st}
#'  \item\code{SU40}{Total count of days when daily maximum temperatures exceed 40 between June 21st and September 21st}
#'  \item\code{Spr32}{Total count of days when daily maximum temperatures exceed 32 between April 21st and June 21st}
#'}
#'
#'@param data a 's2dv_cube' object as provided by function \code{CST_Load} in package CSTools.
#'@param threshold a 's2dv_cube' object as output of a 'CST_' function in the same units as parameter \code{data} and with the common dimensions of the element \code{data} of the same length (e.g. an array with the same lengths of longitude and latitude). A single scalar is also possible (for the case of comparing all grid points with the same scalar).
#'@param op a opartor '>' (by default), '<', '>=' or '<='. 
#'@param start an optional parameter to defined the initial date of the period to select from the data by providing a list of two elements: the initial date of the period and the initial month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.
#'@param end an optional parameter to defined the final date of the period to select from the data by providing a list of two elements: the final day of the period and the final month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.
#'@param time_dim a character string indicating the name of the function to compute the indicator. By default, it is set to 'ftime'. More than one dimension name matching the dimensions provided in the object \code{data$data} can be specified.
#'@param na.rm a logical value indicating whether to ignore NA values (TRUE) or not (FALSE). 
#'@param ncores an integer indicating the number of cores to use in parallel computation.
#'
#'@return A 's2dv_cube' object containing the indicator in the element \code{data}.
#'
#'@import multiApply
#'@examples
#'exp <- CSTools::lonlat_data$exp
#'exp$data <- CSTools::lonlat_data$exp$data[1, 1, 3, 3, 1, 1]
#'DOT <- CST_TotalTimeExceedingThreshold(exp, threshold = 280)
#'@export
CST_TotalTimeExceedingThreshold <- function(data, threshold, op = '>',
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
        }
      } else {
        warning("Dimensions in 'data' element 'Dates$start' are missed and",
                "all data would be used.")
      }
    }
 }
 if (inherits(threshold, 's2dv_cube')) {
    threshold <- threshold$data
  }
 total <- TotalTimeExceedingThreshold(data$data, data$Dates[[1]],
                                      threshold = threshold, op = op,
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
#'Total Time of a variable Exceeding (not exceeding) a Threshold
#'
#'The Total Time of a variable exceeding (or not) a Threshold returns the total number of days 
#'(if the data provided is daily, or the corresponding units to the data frequency provided)
#' that a variable is exceeding a threshold during a period. The threshold provided must be 
#'in the same units than the variable units, i.e. to use a percentile as a threshold, 
#'the function \code{Threshold} or \code{QThreshold} may be needed (see examples). 
#'Providing maximum temperature daily data, the following agriculture indices for heat stress can be obtained by using this function:
#'\itemize{
#'  \item\code{SU35}{Total count of days when daily maximum temperatures exceed 35°C}
#'  \item\code{SU36}{Total count of days when daily maximum temperatures exceed 36 between June 21st and September 21st}
#'  \item\code{SU40}{Total count of days when daily maximum temperatures exceed 40 between June 21st and September 21st}
#'  \item\code{Spr32}{Total count of days when daily maximum temperatures exceed 32 between April 21st and June 21st}
#'}
#'
#'@param data a multidimensional array with named dimensions.
#'@param threshold a multidimensional array with named dimensions in the same units as parameter \code{data} and with the common dimensions of the element \code{data} of the same length (e.g. an array with the same lengths of longitude and latitude). A single scalar is also possible (for the case of comparing all grid points with the same scalar).
#'@param op a opartor '>' (by default), '<', '>=' or '<='. 
#'@param dates a vector of dates or a multidimensional array of dates with named dimensions matching the dimensions on parameter 'data'. By default it is NULL, to select a period this parameter must be provided.
#'@param start an optional parameter to defined the initial date of the period to select from the data by providing a list of two elements: the initial date of the period and the initial month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.
#'@param end an optional parameter to defined the final date of the period to select from the data by providing a list of two elements: the final day of the period and the final month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.
#'@param time_dim a character string indicating the name of the function to compute the indicator. By default, it is set to 'time'. More than one dimension name matching the dimensions provided in the object \code{data$data} can be specified.
#'@param na.rm a logical value indicating whether to ignore NA values (TRUE) or  not (FALSE). 
#'@param ncores an integer indicating the number of cores to use in parallel computation.
#'
#'@return A multidimensional array with named dimensions.
#'
#'@import multiApply
#'@examples
#'exp <- CSTools::lonlat_data$exp$data[1, 5, 3, 3, 1, 1]
#'DOT <- TotalTimeExceedingThreshold(exp, threshold = 300, time_dim = 'ftime')
#'
#'@export
TotalTimeExceedingThreshold <- function(data, threshold, op = '>',
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
  common_dims <- which(names(dim(data)) %in% names(dim(threshold)))
  if (length(threshold) > 1) {    
    if (any(dim(data)[common_dims] != 
            dim(threshold)[which(names(dim(threshold)) %in% names(dim(data)))])) {
    stop("Parameter 'data' and 'threshold' must have the same length on common dimensions.")
    }
  } 
  if (!is.null(dates)) {
    if (!is.null(start) && !is.null(end)) {
      if (!any(c(is.list(start), is.list(end)))) {
        stop("Parameter 'start' and 'end' must be lists indicating the ",
             "day and the month of the period start and end.")
      }
      if (time_dim %in% names(dim(threshold))) {
        if (dim(threshold)[time_dim] == dim(data)[time_dim]) {
            threshold <- SelectPeriodOnData(threshold, dates, start, end,
                                           time_dim = time_dim, ncores = ncores)
        }
      }
      data <- SelectPeriodOnData(data, dates, start, end, 
                                 time_dim = time_dim, ncores = ncores)
    }
  }
  if (is.null(dim(threshold))) {
    total <- Apply(list(data), target_dims = time_dim,
                   fun = .exceedthreshold,
                   y = threshold, op = op, na.rm = na.rm,
                   ncores = ncores)$output1
  } else if (all(time_dim %in% names(dim(threshold)))) {
    total <- Apply(list(data, threshold),
                   target_dims = list(time_dim, time_dim),
                   fun = .exceedthreshold, op = op, na.rm = na.rm,
                   ncores = ncores)$output1
  } else if (any(time_dim %in% names(dim(threshold)))) {
    total <- Apply(list(data, threshold),
                   target_dims = list(time_dim, 
                                      time_dim[time_dim %in% names(dim(threshold))]),
                   fun = .exceedthreshold, op = op, na.rm = na.rm,
                   ncores = ncores)$output1   
  } else {
    total <- Apply(list(data, threshold),
                   target_dims = list(time_dim, NULL),
                   fun = .exceedthreshold, op = op, na.rm = na.rm,
                   ncores = ncores)$output1
  }
  return(total)
}
.exceedthreshold <- function(x, y, op, na.rm) {
  if (op == '>') {
    res <- sum(x > y, na.rm = na.rm)
  } else if (op == '<') {
    res <- sum(x < y, na.rm = na.rm)
  } else if (op == '<=') {
    res <- sum(x <= y, na.rm = na.rm)
  } else {
    res <- sum(x >= y, na.rm = na.rm)
  }
  return(res)
}

