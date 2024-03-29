% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PeriodMax.R
\name{CST_PeriodMax}
\alias{CST_PeriodMax}
\title{Period Max on 's2dv_cube' objects}
\usage{
CST_PeriodMax(
  data,
  start = NULL,
  end = NULL,
  time_dim = "time",
  na.rm = FALSE,
  ncores = NULL
)
}
\arguments{
\item{data}{An 's2dv_cube' object as provided function \code{CST_Start} or 
\code{CST_Load} in package CSTools.}

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

\item{na.rm}{A logical value indicating whether to ignore NA values (TRUE) or 
not (FALSE).}

\item{ncores}{An integer indicating the number of cores to use in parallel 
computation.}
}
\value{
An 's2dv_cube' object containing the indicator in the element 
\code{data} with dimensions of the input parameter 'data' except the 
dimension where the max has been computed (specified with 'time_dim'). A new 
element called 'time_bounds' will be added into the 'attrs' element in the 
's2dv_cube' object. It consists of a list containing two elements, the start 
and end dates of the aggregated period with the same dimensions of 'Dates' 
element.
}
\description{
Period Max computes the maximum (max) of a given variable in a period.
Two bioclimatic indicators can be obtained by using this function:
\itemize{
 \item{'BIO5', (Providing temperature data) Max Temperature of Warmest 
       Month. The maximum monthly temperature occurrence over a 
       given year (time-series) or averaged span of years (normal).}
 \item{'BIO13', (Providing precipitation data) Precipitation of Wettest 
       Month. This index identifies the total precipitation 
       that prevails during the wettest month.}
}
}
\examples{
exp <- NULL
exp$data <- array(rnorm(45), dim = c(member = 7, sdate = 4, time = 3))
Dates <- c(seq(as.Date("2000-11-01", "\%Y-\%m-\%d", tz = "UTC"), 
              as.Date("2001-01-01", "\%Y-\%m-\%d", tz = "UTC"), by = "month"),
          seq(as.Date("2001-11-01", "\%Y-\%m-\%d", tz = "UTC"), 
              as.Date("2002-01-01", "\%Y-\%m-\%d", tz = "UTC"), by = "month"),
          seq(as.Date("2002-11-01", "\%Y-\%m-\%d", tz = "UTC"), 
              as.Date("2003-01-01", "\%Y-\%m-\%d", tz = "UTC"), by = "month"),
          seq(as.Date("2003-11-01", "\%Y-\%m-\%d", tz = "UTC"), 
              as.Date("2004-01-01", "\%Y-\%m-\%d", tz = "UTC"), by = "month"))
dim(Dates) <- c(sdate = 4, time = 3)
exp$attrs$Dates <- Dates
class(exp) <- 's2dv_cube'

res <- CST_PeriodMax(exp, start = list(01, 12), end = list(01, 01))

}
