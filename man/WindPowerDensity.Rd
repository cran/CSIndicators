% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WindPowerDensity.R
\name{WindPowerDensity}
\alias{WindPowerDensity}
\title{Wind power density on multidimensional array objects}
\usage{
WindPowerDensity(
  wind,
  ro = 1.225,
  dates = NULL,
  start = NULL,
  end = NULL,
  time_dim = "time",
  ncores = NULL
)
}
\arguments{
\item{wind}{A multidimensional array, vector or scalar with instantaneous wind
speeds expressed in m/s.}

\item{ro}{A scalar, or alternatively a multidimensional array with the same
dimensions as wind, with the air density expressed in kg/m^3. By default it
takes the value 1.225, the standard density of air at 15ºC and 1013.25 hPa.}

\item{dates}{A multidimensional array of dates with named dimensions matching 
the temporal dimensions on parameter 'data'. By default it is NULL, to  
select aperiod this parameter must be provided.}

\item{start}{An optional parameter to defined the initial date of the period
to select from the data by providing a list of two elements: the initial
date of the period and the initial month of the period. By default it is set
to NULL and the indicator is computed using all the data provided in
\code{data}.}

\item{end}{An optional parameter to defined the final date of the period to
select from the data by providing a list of two elements: the final day of
the period and the final month of the period. By default it is set to NULL
and the indicator is computed using all the data provided in \code{data}.}

\item{time_dim}{A character string indicating the name of the dimension to
compute the indicator. By default, it is set to 'time'. More than one
dimension name matching the dimensions provided in the object
\code{data$data} can be specified.}

\item{ncores}{An integer indicating the number of cores to use in parallel
computation for temporal subsetting.}
}
\value{
An array with the same dimensions as wind, containing Wind Power 
Density expressed in W/m^2.
}
\description{
Wind Power Density computes the wind power that is available for 
extraction per square meter of swept area.

It is computed as 0.5*ro*wspd^3. As this function is non-linear, 
it will give inaccurate results if used with period means.
}
\examples{
wind <- array(rweibull(n = 32100, shape = 2, scale = 6), 
             c(member = 5, sdate = 3, time = 214, lon = 2, lat = 5))
Dates <- c(seq(as.Date("01-05-2000", format = "\%d-\%m-\%Y"), 
              as.Date("30-11-2000", format = "\%d-\%m-\%Y"), by = 'day'),
          seq(as.Date("01-05-2001", format = "\%d-\%m-\%Y"), 
              as.Date("30-11-2001", format = "\%d-\%m-\%Y"), by = 'day'),
          seq(as.Date("01-05-2002", format = "\%d-\%m-\%Y"), 
              as.Date("30-11-2002", format = "\%d-\%m-\%Y"), by = 'day'))
dim(Dates) <- c(sdate = 3, time = 214)
WPD <- WindPowerDensity(wind, dates = Dates, start = list(21, 4), 
                       end = list(21, 6))

}
\author{
Llorenç Lledó, \email{llledo@bsc.es}
}
