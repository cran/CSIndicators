#'Period Accumulation on 's2dv_cube' objects
#'
#'Period Accumulation computes the sum (accumulation) of a given variable in a period.
#'Providing precipitation data, two agriculture indices can be obtained by using this function:
#'\itemize{
#'  \item\code{SprR}{Spring Total Precipitation: The total precipitation from April 21th to June 21st}
#'  \item\code{HarR}{Harvest Total Precipitation: The total precipitation from August 21st to October 21st}}
#'
#'@param data an 's2dv_cube' object as provided function \code{CST_Load} in package CSTools.
#'@param start an optional parameter to defined the initial date of the period to select from the data by providing a list of two elements: the initial date of the period and the initial month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.
#'@param end an optional parameter to defined the final date of the period to select from the data by providing a list of two elements: the final day of the period and the final month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.
#'@param time_dim a character string indicating the name of the function to compute the indicator. By default, it is set to 'ftime'. More than one dimension name matching the dimensions provided in the object \code{data$data} can be specified.
#'@param na.rm a logical value indicating whether to ignore NA values (TRUE) or  not (FALSE). 
#'@param ncores an integer indicating the number of cores to use in parallel computation.
#'
#'@return A 's2dv_cube' object containing the indicator in the element \code{data}.
#'
#'@import multiApply
#'
#'@examples
#'exp <- CSTools::lonlat_prec
#'TP <- CST_PeriodAccumulation(exp)
#'exp$data <- array(rnorm(5 * 3 * 214 * 2),
#'                    c(memb = 5, sdate = 3, ftime = 214, lon = 2)) 
#'exp$Dates[[1]] <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
#'                     as.Date("30-11-2000", format = "%d-%m-%Y"), by = 'day'),
#'                 seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
#'                     as.Date("30-11-2001", format = "%d-%m-%Y"), by = 'day'),
#'                 seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
#'                     as.Date("30-11-2002", format = "%d-%m-%Y"), by = 'day'))
#'SprR <- CST_PeriodAccumulation(exp, start = list(21, 4), end = list(21, 6))
#'dim(SprR$data)
#'head(SprR$Dates)
#'HarR <- CST_PeriodAccumulation(exp, start = list(21, 8), end = list(21, 10))
#'dim(HarR$data)
#'head(HarR$Dates)
#'@export
CST_PeriodAccumulation <- function(data, start = NULL, end = NULL,
                                   time_dim = 'ftime', na.rm = FALSE,
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
  total <- PeriodAccumulation(data$data, data$Dates[[1]], start, end,
                             time_dim = time_dim, na.rm = na.rm, ncores = ncores)
  data$data <- total
  if (!is.null(start) && !is.null(end)) {
     data$Dates <- SelectPeriodOnDates(dates = data$Dates[[1]],
                                start = start, end = end, 
                                time_dim = time_dim, ncores = ncores)
  }
  return(data)
}
#'Period Accumulation on multidimensional array objects
#'
#'Period Accumulation computes the sum (accumulation) of a given variable in a period.
#'Providing precipitation data, two agriculture indices can be obtained by using this function:
#'\itemize{
#'  \item\code{SprR}{Spring Total Precipitation: The total precipitation from April 21th to June 21st}
#'  \item\code{HarR}{Harvest Total Precipitation: The total precipitation from August 21st to October 21st}}
#'
#'@param data a multidimensional array with named dimensions.
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
#'
#'@examples
#'exp <- CSTools::lonlat_prec$data
#'TP <- PeriodAccumulation(exp, time_dim = 'ftime')
#'data <- array(rnorm(5 * 3 * 214 * 2),
#'              c(memb = 5, sdate = 3, time = 214, lon = 2)) 
#'# ftime tested
#'Dates <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2000", format = "%d-%m-%Y"), by = 'day'),
#'               seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2001", format = "%d-%m-%Y"), by = 'day'),
#'               seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2002", format = "%d-%m-%Y"), by = 'day'))
#'SprR <- PeriodAccumulation(data, dates = Dates, start = list(21, 4), end = list(21, 6))
#'HarR <- PeriodAccumulation(data, dates = Dates, start = list(21, 8), end = list(21, 10))
#'@export
PeriodAccumulation <- function(data, dates = NULL, start = NULL, end = NULL, 
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
  total <- Apply(list(data), target_dims = time_dim, fun = sum,
                 na.rm = na.rm, ncores = ncores)$output1
  return(total)
}

