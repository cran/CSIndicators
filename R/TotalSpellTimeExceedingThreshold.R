#'Total Spell Time Exceeding Threshold
#'
#'The number of days (when daily data is provided) that are part of a spell (defined by its minimum length e.g. 6 consecutive days) that exceed (or not exceed) a threshold are calculated with \code{TotalSpellTimeExceedingThreshold}.
#'This function allows to compute indicators widely used in Climate Services, such as:
#'\itemize{
#' \code{WSDI}{Warm Spell Duration Index that count the total number of days with at least 6 consecutive days when the daily temperature maximum exceeds its 90th percentile.}}
#'This function requires the data and the threshold to be in the same units. The 90th percentile can be translate into absolute values given a reference dataset using function \code{Threshold} or the data can be transform into probabilites by using function \code{AbsToProbs}. See section @examples.
#'@seealso [Threshold()] and [AbsToProbs()].
#'
#'@param data an 's2dv_cube' object as provided by function \code{CST_Load} in package CSTools.
#'@param threshold an 's2dv_cube' object as output of a 'CST_' function in the same units as parameter 'data' and with the common dimensions of the element 'data' of the same length. A single scalar is also possible. If \code{timd_dim} is in the dimension (with the same length as \code{data}), the comparison will be done day by day. 
#'@param spell a scalar indicating the minimum length of the spell.
#'@param op a opartor '>' (by default), '<', '>=' or '<='. 
#'@param start an optional parameter to defined the initial date of the period to select from the data by providing a list of two elements: the initial date of the period and the initial month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.
#'@param end an optional parameter to defined the final date of the period to select from the data by providing a list of two elements: the final day of the period and the final month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.
#'@param time_dim a character string indicating the name of the function to compute the indicator. By default, it is set to 'ftime'. More than one dimension name matching the dimensions provided in the object \code{data$data} can be specified.
#'@param ncores an integer indicating the number of cores to use in parallel computation.
#'
#'@return A 's2dv_cube' object containing the indicator in the element \code{data}.
#'
#'@import multiApply
#'@examples
#'exp <- CSTools::lonlat_data$exp
#'exp$data <- array(rnorm(5 * 3 * 20 * 2, mean = 25, sd = 3),
#'                    c(member = 5, sdate = 3, ftime = 20, lon = 2))  
#'TTSET <- CST_TotalSpellTimeExceedingThreshold(exp, threshold = 23, spell = 3)
#'@export
CST_TotalSpellTimeExceedingThreshold <- function(data, threshold, spell, op = '>',
                                            start = NULL, end = NULL,
                                            time_dim = 'ftime',
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
 total <- TotalSpellTimeExceedingThreshold(data$data, data$Dates[[1]],
                                      threshold = threshold, spell = spell, op = op,
                                      start = start, end = end, time_dim = time_dim,
                                      ncores = ncores)
  data$data <- total
  if (!is.null(start) && !is.null(end)) {
     data$Dates <- SelectPeriodOnDates(dates = data$Dates$start,
                                       start = start, end = end,
                                       time_dim = time_dim, ncores = ncores)
  }
  return(data)
}
#'Total Spell Time Exceeding Threshold
#'
#'The number of days (when daily data is provided) that are part of a spell (defined by its minimum length e.g. 6 consecutive days) that exceed (or not exceed) a threshold are calculated with \code{TotalSpellTimeExceedingThreshold}.
#'This function allows to compute indicators widely used in Climate Services, such as:
#'\itemize{
#' \code{WSDI}{Warm Spell Duration Index that count the total number of days with at least 6 consecutive days when the daily temperature maximum exceeds its 90th percentile.}}
#'This function requires the data and the threshold to be in the same units. The 90th percentile can be translate into absolute values given a reference dataset using function \code{Threshold} or the data can be transform into probabilites by using function \code{AbsToProbs}. See section @examples.
#'@seealso [Threshold()] and [AbsToProbs()].
#'
#'@param data a multidimensional array with named dimensions.
#'@param threshold a multidimensional array with named dimensions in the same units as parameter 'data' and with the common dimensions of the element 'data' of the same length. If \code{timd_dim} is in the dimension (with the same length as \code{data}), the comparison will be done day by day.
#'@param spell a scalar indicating the minimum length of the spell.
#'@param op a opartor '>' (by default), '<', '>=' or '<='. 
#'@param dates a vector of dates or a multidimensional array of dates with named dimensions matching the dimensions on parameter 'data'. By default it is NULL, to select a period this parameter must be provided.
#'@param start an optional parameter to defined the initial date of the period to select from the data by providing a list of two elements: the initial date of the period and the initial month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.
#'@param end an optional parameter to defined the final date of the period to select from the data by providing a list of two elements: the final day of the period and the final month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.
#'@param time_dim a character string indicating the name of the function to compute the indicator. By default, it is set to 'ftime'. More than one dimension name matching the dimensions provided in the object \code{data$data} can be specified.
#'@param ncores an integer indicating the number of cores to use in parallel computation.
#'
#'@return A multidimensional array with named dimensions.
#'
#'@details This function considers NA values as the end of the spell. For a different behaviour consider to modify the 'data' input by substituting NA values by values exceeding the threshold.
#'@import multiApply
#'@examples
#'data <- array(rnorm(120), c(member = 1, sdate = 2, time = 20, lat = 4))
#'threshold <- array(rnorm(4), c(lat = 4))
#'total <- TotalSpellTimeExceedingThreshold(data, threshold, spell = 6)
#'@export
TotalSpellTimeExceedingThreshold <- function(data, threshold, spell, op = '>', dates = NULL,
                                             start = NULL, end = NULL, time_dim = 'time',
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
  if (is.null(names(dim(data)))) {
    stop("Parameter 'data' must have named dimensions.")
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
                   fun = .totalspellthres,
                   threshold = threshold, spell = spell, op = op,
                   ncores = ncores)$output1
  } else if (any(time_dim %in% names(dim(threshold)))) {
    total <- Apply(list(data, threshold),
                 target_dims = list(time_dim, 
                                    time_dim[time_dim %in% names(dim(threshold))]),
                 fun = .totalspellthres, spell = spell, op = op,
                 ncores = ncores)$output1

  } else {
    total <- Apply(list(data, threshold), target_dims = list(time_dim, NULL), 
                 fun = .totalspellthres, spell = spell, op = op,
                 ncores = ncores)$output1
  }
  return(total) 
}  
#data <- c(1,1,3,3,3,3,1,1,3,1,1,3,3,3,3,3)
#spell <- 3
#threshold <- 2
.totalspellthres <- function(data, threshold, spell, op = '>') {
  # data a time serie, threshold single value:
  if (op == '>') {
    exceed <- data > threshold
  } else if (op == '<') {
    exceed <- data < threshold
  } else if (op == '<=') {
    exceed <- data <= threshold
  } else {
    exceed <- data >= threshold
  }
  spells_exceed <- sequence(rle(as.character(exceed))$lengths)
  spells_exceed[exceed == FALSE] <- NA
  pos_spells <- which(spells_exceed == spell)
  total <- sum(unlist(lapply(pos_spells, function(y) {
                        last_days <- x <- y
                        while (!is.na(x)) {
                          x <- spells_exceed[last_days + 1]
                          last_days <- last_days + 1
                        }    
                        days <- length((y - spell + 1): (last_days - 1))
                        return(days)
                        })))
  return(total)
}
