#'Wind power density on s2dv_cube objects
#'
#'@author Llorenç Lledó, \email{llledo@bsc.es}
#'@description Wind Power Density computes the wind power that is available for extraction per square meter of swept area. 
#'@description It is computed as 0.5*ro*wspd^3. As this function is non-linear, it will give inaccurate results if used with period means.
#'
#'@param wind a s2dv_cube object with instantaneous wind speeds expressed in m/s obtained from CST_Load or s2dv_cube functions from CSTools pacakge
#'@param ro a scalar, or alternatively a multidimensional array with the same dimensions as wind, with the air density expressed in kg/m^3. By default it takes the value 1.225, the standard density of air at 15ºC and 1013.25 hPa.
#'@param start an optional parameter to defined the initial date of the period to select from the data by providing a list of two elements: the initial date of the period and the initial month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.
#'@param end an optional parameter to defined the final date of the period to select from the data by providing a list of two elements: the final day of the period and the final month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.
#'@param time_dim a character string indicating the name of the function to compute the indicator. By default, it is set to 'ftime'. More than one dimension name matching the dimensions provided in the object \code{data$data} can be specified.
#'@param ncores an integer indicating the number of cores to use in parallel computation for temporal subsetting.
#'
#'@return A s2dv_cube object containing Wind Power Density expressed in W/m^2.
#'
#'@examples
#'wind <- array(rweibull(n = 100, shape = 2, scale = 6), c(member = 10, lat = 2, lon = 5))
#'wind <- CSTools::s2dv_cube(data = wind, lat = c(40, 41), lon = 1:5,
#'                  Variable = list(varName = 'sfcWind', level = 'Surface'), 
#'                  Datasets = 'synthetic', when = Sys.time(),
#'                  Dates = list(start = '1990-01-01 00:00:00', end = '1990-01-01 00:00:00'),
#'                  source_file = NA)
#'WPD <- CST_WindPowerDensity(wind)
#'
#'@export
CST_WindPowerDensity <- function(wind, ro = 1.225, start = NULL, end = NULL, 
                                 time_dim = 'ftime', ncores = NULL) {
  if (!inherits(wind, 's2dv_cube')) {
    stop("Parameter 'wind' must be of the class 's2dv_cube', ",
         "as output by CSTools::CST_Load.")
  }
  # when subsetting is needed, dimensions are also needed:
  if (!is.null(start) && !is.null(end)) {
    if (is.null(dim(wind$Dates$start))) {
      if (length(wind$Dates$start) != dim(wind$data)[time_dim]) {
        if (length(wind$Dates$start) ==
            prod(dim(wind$data)[time_dim] * dim(wind$data)['sdate'])) {
          dim(wind$Dates$start) <- c(dim(wind$data)[time_dim],
                                     dim(wind$data)['sdate'])
        }
      } else {
        warning("Dimensions in 'wind' element 'Dates$start' are missed/unmatched. All data would be used.")
      }
    }
  }
  wind$data <- WindPowerDensity(wind$data, ro = ro, dates = wind$Dates[[1]], 
                                start = start, end = end, ncores = ncores)
  if ('Variable' %in% names(wind)) {
    if ('varName' %in% names(wind$Variable)) {
      wind$Variable$varName <- 'WindPowerDensity'
    }
  }
  if (!is.null(start) && !is.null(end)) {
     wind$Dates <- SelectPeriodOnDates(dates = wind$Dates[[1]],
                                start = start, end = end,
                                time_dim = time_dim, ncores = ncores)
  }	
  return(wind)
}

#'Wind power density on multidimensional array objects
#'
#'@author Llorenç Lledó, \email{llledo@bsc.es}
#'@description Wind Power Density computes the wind power that is available for extraction per square meter of swept area. 
#'@description It is computed as 0.5*ro*wspd^3. As this function is non-linear, it will give inaccurate results if used with period means.
#'
#'@param wind a multidimensional array, vector or scalar with instantaneous wind speeds expressed in m/s.
#'@param ro a scalar, or alternatively a multidimensional array with the same dimensions as wind, with the air density expressed in kg/m^3. By default it takes the value 1.225, the standard density of air at 15ºC and 1013.25 hPa.
#'@param dates a vector of dates or a multidimensional array of dates with named dimensions matching the dimensions on parameter 'data'. By default it is NULL, to select a period this parameter must be provided.
#'@param start an optional parameter to defined the initial date of the period to select from the data by providing a list of two elements: the initial date of the period and the initial month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.
#'@param end an optional parameter to defined the final date of the period to select from the data by providing a list of two elements: the final day of the period and the final month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.
#'@param time_dim a character string indicating the name of the function to compute the indicator. By default, it is set to 'ftime'. More than one dimension name matching the dimensions provided in the object \code{data$data} can be specified.
#'@param ncores an integer indicating the number of cores to use in parallel computation for temporal subsetting.
#'
#'@return An array with the same dimensions as wind, containing Wind Power Density expressed in W/m^2.
#'
#'@examples
#'wind <- rweibull(n = 100, shape = 2, scale = 6)
#'WPD <- WindPowerDensity(wind)
#'
#'@export
WindPowerDensity <- function(wind, ro = 1.225, dates = NULL, start = NULL, end = NULL,
                             time_dim = 'time', ncores = NULL) {
  if (!is.null(dates)) {
    if (!is.null(start) && !is.null(end)) {
      if (!any(c(is.list(start), is.list(end)))) {
       stop("Parameter 'start' and 'end' must be lists indicating the ",
             "day and the month of the period start and end.")
      }
      wind <- SelectPeriodOnData(wind, dates, start, end,
                                 time_dim = time_dim, ncores = ncores)
    }
  }

	return(0.5 * ro * wind^3)
}
