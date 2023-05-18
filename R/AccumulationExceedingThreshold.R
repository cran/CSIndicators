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
#'@param threshold If only one threshold is used, it can be an 's2dv_cube' 
#'  object or a multidimensional array with named dimensions. It must be in the 
#'  same units and with the common dimensions of the same length as parameter 
#'  'data'. It can also be a vector with the same length of 'time_dim' from 
#'  'data' or a scalar. If we want to use two thresholds: it can be a vector 
#'  of two scalars, a list of two vectors with the same length of 
#'  'time_dim' from 'data' or a list of two multidimensional arrays with the 
#'  common dimensions of the same length as parameter 'data'. If two thresholds
#'  are used, parameter 'op' must be also a vector of two elements.
#'@param op An operator '>' (by default), '<', '>=' or '<='. If  two thresholds
#'  are used it has to be a vector of a pair of two logical operators: 
#'  c('<', '>'), c('<', '>='), c('<=', '>'), c('<=', '>='), c('>', '<'), 
#'  c('>', '<='), c('>=', '<'),c('>=', '<=')).
#'@param diff A logical value indicating whether to accumulate the difference 
#'  between data and threshold (TRUE) or not (FALSE by default). It can only be
#'  TRUE if a unique threshold is used.
#'@param start An optional parameter to define the initial date of the period
#'  to select from the data by providing a list of two elements: the initial 
#'  date of the period and the initial month of the period. By default it is 
#'  set to NULL and the indicator is computed using all the data provided in 
#'  \code{data}.
#'@param end An optional parameter to define the final date of the period to 
#'  select from the data by providing a list of two elements: the final day of 
#'  the period and the final month of the period. By default it is set to NULL
#'  and the indicator is computed using all the data provided in \code{data}.
#'@param time_dim A character string indicating the name of the dimension to 
#'  compute the indicator. By default, it is set to 'ftime'. It can only
#'  indicate one time dimension.
#'@param na.rm A logical value indicating whether to ignore NA values (TRUE) 
#'  or not (FALSE).
#'@param ncores An integer indicating the number of cores to use in parallel
#'computation.
#'
#'@return An 's2dv_cube' object containing the aggregated values in the element
#'\code{data} with dimensions of the input parameter 'data' except the dimension
#'where the indicator has been computed.
#'@examples 
#'exp <- NULL
#'exp$data <- array(rnorm(216)*200, dim = c(dataset = 1, member = 2, sdate = 3, 
#'                  ftime = 9, lat = 2, lon = 2))
#'class(exp) <- 's2dv_cube'
#'DOT <- CST_AccumulationExceedingThreshold(exp, threshold = 280)
#'
#'@import multiApply
#'@export
CST_AccumulationExceedingThreshold <- function(data, threshold, op = '>', diff = FALSE,
                                               start = NULL, end = NULL, time_dim = 'ftime',
                                               na.rm = FALSE, ncores = NULL) {
  # Check 's2dv_cube'
  if (!inherits(data, 's2dv_cube')) {
    stop("Parameter 'data' must be of the class 's2dv_cube'.")
  }
  # Dates subset
  if (!is.null(start) && !is.null(end)) {
    if (is.null(dim(data$attrs$Dates))) {
      warning("Dimensions in 'data' element 'attrs$Dates' are missed and ",
              "all data would be used.")
      start <- NULL
      end <- NULL
    }
  }

  if (length(op) == 1) {
    if (inherits(threshold, 's2dv_cube')) {
        threshold <- threshold$data
    }
  } else if (length(op) == 2) {
    if (inherits(threshold[[1]], 's2dv_cube')) {
      threshold[[1]] <- threshold[[1]]$data
    }
    if (inherits(threshold[[2]], 's2dv_cube')) {
      threshold[[2]] <- threshold[[2]]$data
    }
  }

  total <- AccumulationExceedingThreshold(data$data, dates = data$attrs$Dates,
                                          threshold = threshold, op = op, diff = diff,
                                          start = start, end = end, time_dim = time_dim,
                                          na.rm = na.rm, ncores = ncores)
  data$data <- total
  if (!is.null(start) && !is.null(end)) {
    data$attrs$Dates <- SelectPeriodOnDates(dates = data$attrs$Dates,
                                            start = start, end = end,
                                            time_dim = time_dim, 
                                            ncores = ncores)
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
#'@param threshold If only one threshold is used: it can be a multidimensional 
#'  array with named dimensions. It must be in the same units and with the 
#'  common dimensions of the same length as parameter 'data'. It can also be a
#'  vector with the same length of 'time_dim' from 'data' or a scalar. If we 
#'  want to use two thresholds: it can be a vector of two scalars, a list of 
#'  two vectors with the same length of 'time_dim' from 'data' or a list of 
#'  two multidimensional arrays with the common dimensions of the same length 
#'  as parameter 'data'. If two thresholds are used, parameter 'op' must be 
#'  also a vector of two elements.
#'@param op An operator '>' (by default), '<', '>=' or '<='. If  two thresholds
#'  are used it has to be a vector of a pair of two logical operators: 
#'  c('<', '>'), c('<', '>='), c('<=', '>'), c('<=', '>='), c('>', '<'), 
#'  c('>', '<='), c('>=', '<'),c('>=', '<=')).
#'@param diff A logical value indicating whether to accumulate the difference 
#'  between data and threshold (TRUE) or not (FALSE by default). It can only be
#'  TRUE if a unique threshold is used.
#'@param dates A vector of dates or a multidimensional array with of dates with 
#'  named dimensions matching the dimensions on parameter 'data'. By default it
#'  is NULL, to select a period this parameter must be provided.
#'@param start An optional parameter to define the initial date of the period
#'  to select from the data by providing a list of two elements: the initial 
#'  date of the period and the initial month of the period. By default it is 
#'  set to NULL and the indicator is computed using all the data provided in 
#'  \code{data}.
#'@param end An optional parameter to define the final date of the period to 
#'  select from the data by providing a list of two elements: the final day of 
#'  the period and the final month of the period. By default it is set to NULL
#'  and the indicator is computed using all the data provided in \code{data}.
#'@param time_dim A character string indicating the name of the dimension to 
#'  compute the indicator. By default, it is set to 'ftime'. It can only
#'  indicate one time dimension.
#'@param na.rm A logical value indicating whether to ignore NA values (TRUE) 
#'  or not (FALSE).
#'@param ncores An integer indicating the number of cores to use in parallel
#'computation.
#'
#'@return A multidimensional array with named dimensions containing the 
#'aggregated values with dimensions of the input parameter 'data' except the 
#'dimension where the indicator has been computed.
#'
#'@examples
#'# Assuming data is already (tasmax + tasmin)/2 - 10
#'data <- array(rnorm(5 * 3 * 214 * 2, mean = 25, sd = 3),
#'                    c(memb = 5, sdate = 3, ftime = 214, lon = 2)) 
#'Dates <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2000", format = "%d-%m-%Y"), by = 'day'),
#'           seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2001", format = "%d-%m-%Y"), by = 'day'),
#'           seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2002", format = "%d-%m-%Y"), by = 'day'))
#'GDD <- AccumulationExceedingThreshold(data, threshold = 0, start = list(1, 4),
#'                                      end = list(31, 10))
#'@import multiApply
#'@export
AccumulationExceedingThreshold <- function(data, threshold, op = '>', diff = FALSE,
                                           dates = NULL, start = NULL, end = NULL,
                                           time_dim = 'ftime', na.rm = FALSE,
                                           ncores = NULL) {
  # data
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
  if (is.null(names(dim(data)))) {
    stop("Parameter 'data' must have named dimensions.")
  }
  # time_dim
  if (!is.character(time_dim)) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!all(time_dim %in% names(dim(data)))) {
    stop("Parameter 'time_dim' is not found in 'data' dimension.")
  }
  if (length(time_dim) > 1) {
    warning("Parameter 'time_dim' has length greater than 1 and ",
            "only the first element will be used.")
    time_dim <- time_dim[1]
  }

  # op 
  if (!is.character(op)) {
    stop("Parameter 'op' must be a character.")
  }
  if (length(op) == 1) {
    if (!(op %in% c('>', '<', '>=', '<=', '='))) {
      stop("Parameter 'op' must be a logical operator.")
    }
  } else if (length(op) == 2) {
    op_list <- list(c('<', '>'), c('<', '>='), c('<=', '>'), c('<=', '>='), 
                    c('>', '<'), c('>', '<='), c('>=', '<'), c('>=', '<='))
    if (!any(unlist(lapply(op_list, function(x) all(x == op))))) {
      stop("Parameter 'op' is not an accepted pair of logical operators.")
    }
  } else {
    stop("Parameter 'op' must be a logical operator with length 1 or 2.")
  }
  # threshold
  if (is.null(unlist(threshold))) {
    stop("Parameter 'threshold' cannot be NULL.")
  }
  if (!is.numeric(unlist(threshold))) {
    stop("Parameter 'threshold' must be numeric.")
  }
  if (length(op) == 2) {
    if (length(op) != length(threshold)) {
      stop(paste0("If 'op' is a  pair of logical operators parameter 'threshold' ",
                  "also has to be a pair of values."))
    }
    if (!is.numeric(threshold[[1]]) | !is.numeric(threshold[[2]])) {
      stop("Parameter 'threshold' must be numeric.")
    }
    if (length(threshold[[1]]) != length(threshold[[2]])) {
      stop("The pair of thresholds must have the same length.")
    }

    if (!is.array(threshold[[1]]) && length(threshold[[1]]) > 1) {
      if (dim(data)[time_dim] != length(threshold[[1]])) {
        stop("If parameter 'threshold' is a vector it must have the same length as data any time dimension.")
      } else {
        dim(threshold[[1]]) <- length(threshold[[1]])
        dim(threshold[[2]]) <- length(threshold[[2]])
        names(dim(threshold[[1]])) <- time_dim
        names(dim(threshold[[2]])) <- time_dim
      }
    } else if (is.array(threshold[[1]]) && length(threshold[[1]]) > 1) {
      if (is.null(names(dim(threshold[[1]])))) {
        stop("If parameter 'threshold' is an array it must have named dimensions.")
      }
      if (!is.null(dim(threshold[[2]]))) {
        if (!all(names(dim(threshold[[1]])) %in% names(dim(threshold[[2]])))) {
          stop("The pair of thresholds must have the same dimension names.")
        }
      }
      namedims <- names(dim(threshold[[1]]))
      order <- match(namedims, names(dim(threshold[[2]])))
      threshold[[2]] <- aperm(threshold[[2]], order)
      if (!all(dim(threshold[[1]]) == dim(threshold[[2]]))) {
        stop("The pair of thresholds must have the same dimensions.")
      }
      if (any(names(dim(threshold[[1]])) %in% names(dim(data)))) {
        common_dims <- dim(threshold[[1]])[names(dim(threshold[[1]])) %in% names(dim(data))]
        if (!all(common_dims == dim(data)[names(common_dims)])) {
          stop(paste0("Parameter 'data' and 'threshold' must have same length of ",
                      "all common dimensions."))
        }
      }
    } else if (length(threshold[[1]]) == 1) {
      dim(threshold[[1]]) <- NULL
      dim(threshold[[2]]) <- NULL
    }
  } else {
    if (!is.array(threshold) && length(threshold) > 1) {
      if (dim(data)[time_dim] != length(threshold)) {
        stop("If parameter 'threshold' is a vector it must have the same length as data time dimension.")
      } else {
        dim(threshold) <- length(threshold)
        names(dim(threshold)) <- time_dim
      }
    } else if (is.array(threshold) && length(threshold) > 1) {
      if (is.null(names(dim(threshold)))) {
          stop("If parameter 'threshold' is an array it must have named dimensions.")
      }
      if (any(names(dim(threshold)) %in% names(dim(data)))) {
        common_dims <- dim(threshold)[names(dim(threshold)) %in% names(dim(data))]
        if (!all(common_dims == dim(data)[names(common_dims)])) {
          stop(paste0("Parameter 'data' and 'threshold' must have same length of ",
                      "all common dimensions."))
        }
      }
    } else if (length(threshold) == 1) {
      dim(threshold) <- NULL
    }
  }
  # ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }
  # dates
  if (!is.null(dates)) {
    if (!is.null(start) && !is.null(end)) {
      if (!any(c(is.list(start), is.list(end)))) {
        stop("Parameter 'start' and 'end' must be lists indicating the ",
             "day and the month of the period start and end.")
      }
      if (length(op) == 1) {
        if (time_dim %in% names(dim(threshold))) {
          if (dim(threshold)[time_dim] == dim(data)[time_dim]) {
            threshold <- SelectPeriodOnData(threshold, dates, start, end,
                                            time_dim = time_dim, ncores = ncores)
          }
        }
      } else if (length(op) == 2) {
        if (time_dim %in% names(dim(threshold[[1]]))) {
          if (dim(threshold[[1]])[time_dim] == dim(data)[time_dim]) {
            threshold[[1]] <- SelectPeriodOnData(threshold[[1]], dates, start, end,
                                                 time_dim = time_dim, ncores = ncores)
            threshold[[2]] <- SelectPeriodOnData(threshold[[2]], dates, start, end,
                                                 time_dim = time_dim, ncores = ncores)
          }
        }
      }
      data <- SelectPeriodOnData(data, dates, start, end, 
                                 time_dim = time_dim, ncores = ncores)
    }
  }
  # diff
  if (length(op) == 2 & diff == TRUE) {
    stop("Parameter 'diff' can't be TRUE if the parameter 'threshold' is a range of values.")
  } else  if (diff == TRUE) {
    if (length(threshold) != 1) {
      stop("Parameter 'diff' can't be TRUE if the parameter 'threshold' is not a scalar.")
    }
    data <- Apply(list(data, threshold),
                  target_dims = list(time_dim, NULL),
                  fun = function(x, y) {x - y}, ncores = ncores)$output1
    dim(data) <- dim(data)[-length(dim(data))]
    threshold <- 0
  }

  ###

  if (length(op) > 1) {
    thres1 <- threshold[[1]]
    thres2 <- threshold[[2]]
    if (is.null(dim(thres1))) {
      total <- Apply(list(data), target_dims = time_dim,
                     fun = .sumexceedthreshold,
                     y = thres1, y2 = thres2,
                     op = op, na.rm = na.rm,
                     ncores = ncores)$output1
    } else if (any(time_dim %in% names(dim(thres1)))) {
        total <- Apply(list(data, thres1, thres2),
                       target_dims = list(time_dim,
                                          time_dim[time_dim %in% names(dim(thres1))],
                                          time_dim[time_dim %in% names(dim(thres2))]),
                       fun = .sumexceedthreshold, op = op, na.rm = na.rm,
                       ncores = ncores)$output1
    } else {
        total <- Apply(list(data, thres1, thres2),
                       target_dims = list(time_dim, thres1 = NULL, thres2 = NULL),
                       fun = .sumexceedthreshold, op = op, na.rm = na.rm,
                       ncores = ncores)$output1
    }
  } else {
    if (is.null(dim(threshold))) {
        total <- Apply(list(data), target_dims = time_dim,
                       fun = .sumexceedthreshold,
                       y = threshold,
                       op = op, na.rm = na.rm,
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
  }
  return(total)
}


.sumexceedthreshold <- function(x, y, y2 = NULL, op, na.rm) {
  y <- as.vector(y)
  y2 <- as.vector(y2)
  if (is.null(y2)) {
    if (op == '>') {
      res <- sum(x[x > y], na.rm = na.rm)
    } else if (op == '<') {
      res <- sum(x[x < y], na.rm = na.rm)
    } else if (op == '<=') {
      res <- sum(x[x <= y], na.rm = na.rm)
    } else if (op == '>=') {
      res <- sum(x[x >= y], na.rm = na.rm)
    } else {
      res <- sum(x[x = y], na.rm = na.rm)
    }
  } else {
    if (all(op == c('<', '>'))) {
      res <- sum(x[x < y & x > y2], na.rm = na.rm)
    } else if (all(op == c('<', '>='))) {
      res <- sum(x[x < y & x >= y2], na.rm = na.rm)
    } else if (all(op == c('<=', '>'))) {
      res <- sum(x[x <= y & x > y2], na.rm = na.rm)
    } else if (all(op == c('<=', '>='))) {
      res <- sum(x[x <= y & x >= y2], na.rm = na.rm)
    } else if (all(op == c('>', '<'))) {
      res <- sum(x[x > y & x < y2], na.rm = na.rm)
    } else if (all(op == c('>', '<='))) {
      res <- sum(x[x > y & x <= y2], na.rm = na.rm)
    } else if (all(op ==  c('>=', '<'))) {
      res <- sum(x[x >= y & x < y2], na.rm = na.rm)
    } else if (all(op == c('>=', '<='))) {
      res <- sum(x[x >= y & x <= y2], na.rm = na.rm)
    }
  }
  
  return(res)
}