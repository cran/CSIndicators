#' Select a period on Data on 's2dv_cube' objects
#'
#' Auxiliary function to subset data for a specific period.
#'
#'@param data an 's2dv_cube' object as provided function \code{CST_Load} in package CSTools.
#'@param start a parameter to defined the initial date of the period to select from the data by providing a list of two elements: the initial date of the period and the initial month of the period.
#'@param end a parameter to defined the final date of the period to select from the data by providing a list of two elements: the final day of the period and the final month of the period.
#'@param time_dim a character string indicating the name of the dimension to compute select the dates. By default, it is set to 'ftime'. More than one dimension name matching the dimensions provided in the object \code{data$data} can be specified.
#'@param ncores an integer indicating the number of cores to use in parallel computation.
#'
#'@return A 's2dv_cube' object containing the subset of the object \code{data$data} during the period requested from \code{start} to \code{end}.
#'
#'@import multiApply
#'
#'@examples
#'exp <- CSTools::lonlat_prec
#'exp$data <- array(rnorm(5 * 3 * 214 * 2),
#'                    c(memb = 5, sdate = 3, ftime = 214, lon = 2)) 
#'exp$Dates[[1]] <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2000", format = "%d-%m-%Y"), by = 'day'),
#'               seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2001", format = "%d-%m-%Y"), by = 'day'),
#'               seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2002", format = "%d-%m-%Y"), by = 'day'))
#'Period <- CST_SelectPeriodOnData(exp, start = list(21, 6), end = list(21, 9))
#'@export
CST_SelectPeriodOnData <- function(data, start, end, time_dim = 'ftime', ncores = NULL) {
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
  res <- SelectPeriodOnData(data$data, data$Dates[[1]],
                            start = start, end = end,
                            time_dim = time_dim, ncores = ncores)
  data$data <- res
  if (!is.null(start) && !is.null(end)) {
     data$Dates <- SelectPeriodOnDates(dates = data$Dates[[1]],
                                start = start, end = end,
                                time_dim = time_dim, ncores = ncores)
  }
  return(data)
}


#' Select a period on Data on multidimensional array objects
#'
#' Auxiliary function to subset data for a specific period.
#'
#'@param data a multidimensional array with named dimensions.
#'@param dates a vector of dates or a multidimensional array of dates with named dimensions.
#'@param start an optional parameter to defined the initial date of the period to select from the data by providing a list of two elements: the initial date of the period and the initial month of the period.
#'@param end an optional parameter to defined the final date of the period to select from the data by providing a list of two elements: the final day of the period and the final month of the period.
#'@param time_dim a character string indicating the name of the dimension to compute select the dates. By default, it is set to 'ftime'. More than one dimension name matching the dimensions provided in the object \code{data$data} can be specified.
#'@param ncores an integer indicating the number of cores to use in parallel computation.
#'
#'@return A multidimensional array with named dimensions.
#'
#'@import multiApply
#'
#'@examples
#'data <- array(rnorm(5 * 3 * 214 * 2),
#'                    c(memb = 5, sdate = 3, ftime = 214, lon = 2)) 
#'Dates <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2000", format = "%d-%m-%Y"), by = 'day'),
#'               seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2001", format = "%d-%m-%Y"), by = 'day'),
#'               seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2002", format = "%d-%m-%Y"), by = 'day'))
#'dim(Dates) <- c(ftime = 214, sdate = 3)
#'Period <- SelectPeriodOnData(data, Dates, start = list(21, 6), end = list(21, 9))
#'
#'@export
SelectPeriodOnData <- function(data, dates, start, end, 
                         time_dim = 'ftime', ncores = NULL) {
  if (is.null(dim(dates))) {
    dim(dates) <- length(dates)
    names(dim(dates)) <- time_dim
  }
  if (is.null(dim(data))) {
    dim(data) <- length(data)
    names(dim(data)) <- time_dim
  }
  res <- Apply(list(dates), target_dims = time_dim,
               fun = .position,
               ini_day = start[[1]], ini_month = start[[2]],
               end_day = end[[1]], end_month = end[[2]],
               ncores = ncores)$output1
  # when 29Feb is included the length of the output changes:
  regular <- Apply(list(res), target_dims = time_dim,
                   fun = sum, ncores = ncores)$output1
  dims <- dim(data)
  dims[names(dims) == time_dim] <- max(regular)
  if (any(regular != max(regular))) {
    res <- Apply(list(data, res), target_dims = time_dim,
                 fun = function(x, y) {
                     if (sum(y) != max(regular)) {
                       result <- c(x[y], NA)
                     } else { 
                       result <- x[y]
                     }
                     dim(result) <- length(result)
                     names(dim(result)) <- names(dim(x))
                     return(result)  
                     }, output_dims = time_dim, ncores = ncores)$output1
  } else {
    res <- Apply(list(data, res), target_dims = time_dim,
                 fun = function(x, y) {
                    res <- x[y]
                 }, output_dims = time_dim, ncores = ncores)$output1 
  }
  return(res)
}

