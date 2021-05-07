#'Absolute value of a relative threshold (percentile)
#'
#'Frequently, thresholds are defined by a percentile that may correspond to a different absolute value depending on the variable, gridpoint and also julian day (time).
#' This function calculates the corresponding value of a percentile given a dataset.
#'
#'@param data an 's2dv_cube' object as provided function \code{CST_Load} in package CSTools.
#'@param threshold a single scalar or vector indicating the relative threshold(s).
#'@param start an optional parameter to defined the initial date of the period to select from the data by providing a list of two elements: the initial date of the period and the initial month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.
#'@param end an optional parameter to defined the final date of the period to select from the data by providing a list of two elements: the final day of the period and the final month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.
#'@param time_dim a character string indicating the name of the temporal dimension. By default, it is set to 'ftime'. More than one dimension name matching the dimensions provided in the object \code{data$data} can be specified. This dimension is required to subset the data in a requested period.
#'@param memb_dim a character string indicating the name of the dimension in which the ensemble members are stored. When set it to NULL, threshold is computed for individual members.
#'@param sdate_dim a character string indicating the name of the dimension in which the initialization dates are stored. 
#'@param na.rm a logical value indicating whether to ignore NA values (TRUE) or  not (FALSE). 
#'@param ncores an integer indicating the number of cores to use in parallel computation.
#'
#'@return A 's2dv_cube' object containing the probabilites in the element \code{data}.
#'
#'@import multiApply
#'
#'@examples
#'threshold <- 0.9
#'exp <- CSTools::lonlat_prec
#'exp_probs <- CST_Threshold(exp, threshold)
#'exp$data <- array(rnorm(5 * 3 * 214 * 2),
#'                    c(member = 5, sdate = 3, ftime = 214, lon = 2)) 
#'exp$Dates[[1]] <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
#'                     as.Date("30-11-2000", format = "%d-%m-%Y"), by = 'day'),
#'                 seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
#'                     as.Date("30-11-2001", format = "%d-%m-%Y"), by = 'day'),
#'                 seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
#'                     as.Date("30-11-2002", format = "%d-%m-%Y"), by = 'day'))
#'exp_probs <- CST_Threshold(exp, threshold, start = list(21, 4), end = list(21, 6))
#'@export
CST_Threshold <- function(data, threshold, start = NULL, end = NULL,
                           time_dim = 'ftime', memb_dim = 'member', sdate_dim = 'sdate',
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
  thres <- Threshold(data$data, threshold, data$Dates[[1]], start, end,
                      time_dim = time_dim, memb_dim = memb_dim,
                      sdate_dim = sdate_dim, na.rm = na.rm, ncores = ncores)
  data$data <- thres
  if (!is.null(start) && !is.null(end)) {
     data$Dates <- SelectPeriodOnDates(dates = data$Dates[[1]],
                                start = start, end = end, 
                                time_dim = time_dim, ncores = ncores)
  }
  return(data)
}
#'Absolute value of a relative threshold (percentile)
#'
#'Frequently, thresholds are defined by a percentile that may correspond to a different absolute value depending on the variable, gridpoint and also julian day (time).
#' This function calculates the corresponding value of a percentile given a dataset.
#'
#'@param data a multidimensional array with named dimensions.
#'@param threshold a single scalar or vector indicating the relative threshold(s).
#'@param dates a vector of dates or a multidimensional array of dates with named dimensions matching the dimensions on parameter 'data'. By default it is NULL, to select a period this parameter must be provided.
#'@param start an optional parameter to defined the initial date of the period to select from the data by providing a list of two elements: the initial date of the period and the initial month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.
#'@param end an optional parameter to defined the final date of the period to select from the data by providing a list of two elements: the final day of the period and the final month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.
#'@param time_dim a character string indicating the name of the temporal dimension. By default, it is set to 'ftime'. More than one dimension name matching the dimensions provided in the object \code{data$data} can be specified. This dimension is required to subset the data in a requested period.
#'@param memb_dim a character string indicating the name of the dimension in which the ensemble members are stored. When set it to NULL, threshold is computed for individual members.
#'@param sdate_dim a character string indicating the name of the dimension in which the initialization dates are stored.
#'@param na.rm a logical value indicating whether to ignore NA values (TRUE) or  not (FALSE). 
#'@param ncores an integer indicating the number of cores to use in parallel computation.
#'
#'@return A multidimensional array with named dimensions.
#'
#'@import multiApply
#'@importFrom stats quantile
#'
#'@examples
#'threshold <- 0.9
#'data <- array(rnorm(25 * 3 * 214 * 2, mean = 26), 
#'              c(member = 25, sdate = 3, time = 214, lon = 2)) 
#'thres_q <- Threshold(data, threshold)
#'data <- array(rnorm(1 * 3 * 214 * 2), c(member = 1, sdate = 3, time = 214, lon = 2))
#'res <- Threshold(data, threshold)
#'@export
Threshold <- function(data, threshold, dates = NULL, start = NULL, end = NULL,
                      time_dim = 'time', memb_dim = 'member', sdate_dim = 'sdate',
                      na.rm = FALSE, ncores = NULL) {
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!is.numeric(data)) {
    stop("Parameter 'data' must be numeric.")
  }
  
  if (!is.array(data)) {
    dim(data) <- c(length(data), 1)
    names(dim(data)) <- c(memb_dim, sdate_dim)
  }
  if (is.null(threshold)) {
      stop("Parameter 'threshold' cannot be NULL.")
  }
   if (!is.numeric(threshold)) {
    stop("Parameter 'threshold' must be numeric.")
  }
  if (is.null(names(dim(data)))) {
    stop("Parameter 'data' must have named dimensions.")
  }
  if (!is.null(dates)) {
    if (!is.null(start) && !is.null(end)) {
      if (!any(c(is.list(start), is.list(end)))) {
        stop("Parameter 'start' and 'end' must be lists indicating the ",
             "day and the month of the period start and end.")
      }
      data <- SelectPeriodOnData(data, dates, start, end, 
                                 time_dim = time_dim, ncores = ncores)
    }
  }   
  if (!is.null(memb_dim)) {
    dimensions <- c(sdate_dim, memb_dim)
  } else {
    dimensions <- sdate_dim
  }
  if (length(threshold) == 1) {
    thres <- Apply(data, target_dims = dimensions,
             fun = function(x) {quantile(as.vector(x), threshold, na.rm)})$output1
  } else {
    thres <- Apply(data, target_dims = dimensions,
             fun = function(x) {quantile(as.vector(x), threshold, na.rm)},
             output_dims = 'probs')$output1
  }
  return(thres)
}