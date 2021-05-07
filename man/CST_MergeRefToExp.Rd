% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MergeRefToExp.R
\name{CST_MergeRefToExp}
\alias{CST_MergeRefToExp}
\title{Merge a Reference To Experiments}
\usage{
CST_MergeRefToExp(
  data1,
  data2,
  start1,
  end1,
  start2,
  end2,
  time_dim = "ftime",
  sdate_dim = "sdate",
  ncores = NULL
)
}
\arguments{
\item{data1}{an 's2dv_cube' object as provided function \code{CST_Load} in package CSTools.}

\item{data2}{an 's2dv_cube' object as provided function \code{CST_Load} in package CSTools.}

\item{start1}{a list to defined the initial date of the period to select from data1 by providing a list of two elements: the initial date of the period and the initial month of the period.}

\item{end1}{a list to defined the final date of the period to select from data1 by providing a list of two elements: the final day of the period and the final month of the period.}

\item{start2}{a list to defined the initial date of the period to select from data2 by providing a list of two elements: the initial date of the period and the initial month of the period.}

\item{end2}{a list to defined the final date of the period to select from data2 by providing a list of two elements: the final day of the period and the final month of the period.}

\item{time_dim}{a character string indicating the name of the temporal dimension. By default, it is set to 'ftime'. More than one dimension name matching the dimensions provided in the object \code{data$data} can be specified. This dimension is required to subset the data in a requested period.}

\item{sdate_dim}{a character string indicating the name of the dimension in which the initialization dates are stored.}

\item{ncores}{an integer indicating the number of cores to use in parallel computation.}
}
\value{
A 's2dv_cube' object containing the indicator in the element \code{data}.
}
\description{
Some indicators are defined for specific temporal periods (e.g.: summer from June 21st to September 21st). If the initialization forecast date is later than the one required for the indicator (e.g.: July 1st), the user may want to merge past observations, or other references, to the forecast (or hindcast) to compute the indicator. The function \code{MergeObs2Exp} takes care of this steps. If the forecast simulation doesn't cover the required period because it is initialized too early (e.g.: Initialization on November 1st the forecast covers until the beginning of June next year), a climatology (or other references) could be added at the end of the forecast lead time to cover the desired period (e.g.: until the end of summer).
}
\examples{
data_dates <- c(seq(as.Date("01-07-1993", "\%d-\%m-\%Y", tz = 'UTC'),
                   as.Date("01-12-1993","\%d-\%m-\%Y", tz = 'UTC'), "day"),
               seq(as.Date("01-07-1994", "\%d-\%m-\%Y", tz = 'UTC'),
                   as.Date("01-12-1994","\%d-\%m-\%Y", tz = 'UTC'), "day"))
dim(data_dates) <- c(ftime = 154, sdate = 2)
ref_dates <- seq(as.Date("01-01-1993", "\%d-\%m-\%Y", tz = 'UTC'),
                as.Date("01-12-1994","\%d-\%m-\%Y", tz = 'UTC'), "day")
dim(ref_dates) <- c(ftime = 350, sdate = 2)
ref <- array(1001:1700, c(ftime = 350, sdate = 2))
data <- array(1:(2*154*2), c(ftime = 154, sdate = 2, member= 2))
ref <- CSTools::s2dv_cube(data = ref, Dates = list(start = ref_dates,
                                                  end = ref_dates))
data <- CSTools::s2dv_cube(data = data, Dates = list(start = data_dates,
                                           end = data_dates))
new_data <- CST_MergeRefToExp(data1 = ref, data2 = data, 
                             start1 = list(21, 6), end1 = list(30, 6),
                             start2 = list(1, 7), end2 = list(21, 9))
}