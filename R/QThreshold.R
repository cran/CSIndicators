#'Transform an absolute threshold into probabilities
#'
#'From a user perspective, an absolute threshold can be very useful for a specific needs (e.g.: grape variety).
#' However, this absolute threshold could be transform to a relative threshold in order to get its frequency in a given dataset.
#'Therefore, the function \code{QThreshold} returns the probability of an absolute threshold.
#'This is done by computing the Cumulative Distribution Function of a sample and leaving-one-ot.
#' The sample used will depend on the dimensions of the data provided and the dimension names provided in sdate_dim and memb_dim parameters:
#'\itemize{
#'  \item{Wheter a forecast (hindcast) has dimensions member and start date, and both must be used in the sample, their names should be passed in sdate_dim and memb_dim.}
#'  \item{Wheter a forecast (hindcast) has dimensions member and start date, and only start date must be used in the sample (the calculation is done in each separate member), memb_dim can be set to NULL.}
#'  \item{Wheter a reference (observations) has start date dimension, the sample used is the start date dimension.}
#'  \item{Wheter a reference (observations) doesn't have start date dimension, the sample used must be especified in sdate_dim parameter.}}
#'
#'@param data an 's2dv_cube' object as provided function \code{CST_Load} in package CSTools.
#'@param threshold an 's2dv_cube' object as output of a 'CST_' function in the same units as parameter 'data' and with the common dimensions of the element 'data' of the same length. A single scalar is also possible.
#'@param start an optional parameter to defined the initial date of the period to select from the data by providing a list of two elements: the initial date of the period and the initial month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.
#'@param end an optional parameter to defined the final date of the period to select from the data by providing a list of two elements: the final day of the period and the final month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.
#'@param time_dim a character string indicating the name of the temporal dimension. By default, it is set to 'ftime'. More than one dimension name matching the dimensions provided in the object \code{data$data} can be specified. This dimension is required to subset the data in a requested period.
#'@param memb_dim a character string indicating the name of the dimension in which the ensemble members are stored.
#'@param sdate_dim a character string indicating the name of the dimension in which the initialization dates are stored. 
#'@param ncores an integer indicating the number of cores to use in parallel computation.
#'
#'@return A 's2dv_cube' object containing the probabilites in the element \code{data}.
#'
#'@import multiApply
#'@importFrom ClimProjDiags Subset
#'
#'@examples
#'threshold <- 26
#'exp <- CSTools::lonlat_prec
#'exp_probs <- CST_QThreshold(exp, threshold)
#'exp$data <- array(rnorm(5 * 3 * 214 * 2),
#'                    c(member = 5, sdate = 3, ftime = 214, lon = 2)) 
#'exp$Dates[[1]] <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
#'                     as.Date("30-11-2000", format = "%d-%m-%Y"), by = 'day'),
#'                 seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
#'                     as.Date("30-11-2001", format = "%d-%m-%Y"), by = 'day'),
#'                 seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
#'                     as.Date("30-11-2002", format = "%d-%m-%Y"), by = 'day'))
#'exp_probs <- CST_QThreshold(exp, threshold, start = list(21, 4), end = list(21, 6))
#'@export
CST_QThreshold <- function(data, threshold, start = NULL, end = NULL,
                           time_dim = 'ftime', memb_dim = 'member', sdate_dim = 'sdate',
                           ncores = NULL) {
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
  probs <- QThreshold(data$data, threshold, data$Dates[[1]], start, end,
                      time_dim = time_dim, memb_dim = memb_dim,
                      sdate_dim = sdate_dim, ncores = ncores)
  data$data <- probs
  if (!is.null(start) && !is.null(end)) {
     data$Dates <- SelectPeriodOnDates(dates = data$Dates[[1]],
                                start = start, end = end, 
                                time_dim = time_dim, ncores = ncores)
  }
  return(data)
}
#'Transform an absolute threshold into probabilities
#'
#'From a user perspective, an absolute threshold can be very useful for a specific needs (e.g.: grape variety).
#' However, this absolute threshold could be transform to a relative threshold in order to get its frequency in a given dataset.
#'Therefore, the function \code{QThreshold} returns the probability of an absolute threshold.
#'This is done by computing the Cumulative Distribution Function of a sample and leaving-one-ot.
#' The sample used will depend on the dimensions of the data provided and the dimension names provided in sdate_dim and memb_dim parameters:
#'\itemize{
#'  \item{Wheter a forecast (hindcast) has dimensions member and start date, and both must be used in the sample, their names should be passed in sdate_dim and memb_dim.}
#'  \item{Wheter a forecast (hindcast) has dimensions member and start date, and only start date must be used in the sample (the calculation is done in each separate member), memb_dim can be set to NULL.}
#'  \item{Wheter a reference (observations) has start date dimension, the sample used is the start date dimension.}
#'  \item{Wheter a reference (observations) doesn't have start date dimension, the sample used must be especified in sdate_dim parameter.}}
#'
#'@param data a multidimensional array with named dimensions.
#'@param threshold a multidimensional array with named dimensions in the same units as parameter 'data' and with the common dimensions of the element 'data' of the same length.
#'@param dates a vector of dates or a multidimensional array of dates with named dimensions matching the dimensions on parameter 'data'. By default it is NULL, to select a period this parameter must be provided.
#'@param start an optional parameter to defined the initial date of the period to select from the data by providing a list of two elements: the initial date of the period and the initial month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.
#'@param end an optional parameter to defined the final date of the period to select from the data by providing a list of two elements: the final day of the period and the final month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.
#'@param time_dim a character string indicating the name of the temporal dimension. By default, it is set to 'ftime'. More than one dimension name matching the dimensions provided in the object \code{data$data} can be specified. This dimension is required to subset the data in a requested period.
#'@param memb_dim a character string indicating the name of the dimension in which the ensemble members are stored.
#'@param sdate_dim a character string indicating the name of the dimension in which the initialization dates are stored. 
#'@param ncores an integer indicating the number of cores to use in parallel computation.
#'
#'@return A multidimensional array with named dimensions.
#'
#'@import multiApply
#'@importFrom ClimProjDiags Subset
#'@examples
#'threshold = 25
#'data <- array(rnorm(5 * 3 * 20 * 2, mean = 26), 
#'              c(member = 5, sdate = 3, time = 20, lon = 2)) 
#'thres_q <- QThreshold(data, threshold)
#'@export
QThreshold <- function(data, threshold, dates = NULL, start = NULL, end = NULL,
                       time_dim = 'time', memb_dim = 'member', sdate_dim = 'sdate',
                       ncores = NULL) {
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
  if (is.null(memb_dim)) {
    memb_dim <- 99999
  }
  if (!is.null(dates)) {
    if (!is.null(start) && !is.null(end)) {
      if (!any(c(is.list(start), is.list(end)))) {
        stop("Parameter 'start' and 'end' must be lists indicating the ",
             "day and the month of the period start and end.")
      }
      if (time_dim %in% names(dim(threshold))) {
        if (dim(threshold)[time_dim] == dim(data)[time_dim]) {
          if (!is.null(dim(dates)) && sdate_dim %in% dim(dates)) {
            dates_thres <- Subset(dates, along = sdate_dim, indices = 1) 
            threshold <- SelectPeriodOnData(threshold, dates_thres, start, end,
                                           time_dim = time_dim, ncores = ncores)
          } else {
             threshold <- SelectPeriodOnData(threshold, dates, start, end,
                                           time_dim = time_dim, ncores = ncores)
          }
        }
      }
      data <- SelectPeriodOnData(data, dates, start, end, 
                                 time_dim = time_dim, ncores = ncores)
    }
  }
  if (length(threshold) == 1) {
    if (memb_dim %in% names(dim(data))) {
      probs <- Apply(list(data), target_dims = c(memb_dim, sdate_dim),
                     fun = .qthreshold_exp,
                     threshold, ncores = ncores)$output1
    } else {
      probs <- Apply(list(data), target_dims = sdate_dim, fun = .qthreshold_obs,
                     threshold, ncores = ncores)$output1
    }
  } else {
    target_thres <- NULL
    if (sdate_dim %in% names(dim(threshold))) {
      stop("Parameter threshold cannot have dimension 'sdate_dim'.") 
    }
    if (memb_dim %in% names(dim(data))) {
      if (memb_dim %in% names(dim(threshold))) {
      # comparison member by member
        probs <- Apply(list(data, threshold), 
                       target_dims = list(sdate_dim, NULL), 
                       fun = .qthreshold_obs, ncores = ncores)$output1
      } else {
        probs <- Apply(list(data, threshold),
                       target_dims = list(c(memb_dim, sdate_dim), NULL),
                       fun = .qthreshold_exp, ncores = ncores)$output1
      }
    } else {
      probs <- Apply(list(data, threshold), target_dims = list(sdate_dim, NULL),
                     fun = .qthreshold_obs, ncores = ncores)$output1
    }
  } 
  return(probs)
}
# By splitting the atomic function, a conditional repetition is avoided 
# inside the Apply loops
.qthreshold_obs <- function(data, threshold) {
  # dims data: sdate 
  dims <- dim(data)
  # no 'member' involving 
  qres <- unlist(lapply(1:dims, function(x) { 
                           ecdf(data[-x])(threshold)}))
  dim(qres) <- c(dims)
  return(qres)
}
.qthreshold_exp <- function(data, threshold) {
  qres <- unlist(
  lapply(1:(dim(data)[1]), function(x) { # dim 1: member
            lapply(1:(dim(data)[2]), function(y) { # dim 2: sdate
                     ecdf(as.vector(data[,-y]))(threshold)
                     })
        }))
  dim(qres) <- c(dim(data)[2], dim(data)[1])
  return(qres)
} 