% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AbsToProbs.R
\name{AbsToProbs}
\alias{AbsToProbs}
\title{Transform ensemble forecast into probabilities}
\usage{
AbsToProbs(
  data,
  dates = NULL,
  start = NULL,
  end = NULL,
  time_dim = "time",
  memb_dim = "member",
  sdate_dim = "sdate",
  ncores = NULL
)
}
\arguments{
\item{data}{a multidimensional array with named dimensions.}

\item{dates}{a vector of dates or a multidimensional array of dates with named dimensions matching the dimensions on parameter 'data'. By default it is NULL, to select a period this parameter must be provided.}

\item{start}{an optional parameter to defined the initial date of the period to select from the data by providing a list of two elements: the initial date of the period and the initial month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.}

\item{end}{an optional parameter to defined the final date of the period to select from the data by providing a list of two elements: the final day of the period and the final month of the period. By default it is set to NULL and the indicator is computed using all the data provided in \code{data}.}

\item{time_dim}{a character string indicating the name of the temporal dimension. By default, it is set to 'ftime'. More than one dimension name matching the dimensions provided in the object \code{data$data} can be specified. This dimension is required to subset the data in a requested period.}

\item{memb_dim}{a character string indicating the name of the dimension in which the ensemble members are stored.}

\item{sdate_dim}{a character string indicating the name of the dimension in which the initialization dates are stored.}

\item{ncores}{an integer indicating the number of cores to use in parallel computation.}
}
\value{
A multidimensional array with named dimensions.
}
\description{
The Cumulative Distribution Function of a forecast is used to obtain the probabilities of each value in the ensemble. If multiple initializations (start dates) are provided, the function will create the Cumulative Distribution Function excluding the corresponding initialization.
}
\examples{
exp <- CSTools::lonlat_prec$data
exp_probs <- AbsToProbs(exp)
data <- array(rnorm(5 * 2 * 61 * 1),
                   c(member = 5, sdate = 2, ftime = 61, lon = 1)) 
Dates <- c(seq(as.Date("01-05-2000", format = "\%d-\%m-\%Y"), 
                    as.Date("30-06-2000", format = "\%d-\%m-\%Y"), by = 'day'),
                seq(as.Date("01-05-2001", format = "\%d-\%m-\%Y"), 
                    as.Date("30-06-2001", format = "\%d-\%m-\%Y"), by = 'day'),
                seq(as.Date("01-05-2002", format = "\%d-\%m-\%Y"), 
                    as.Date("30-06-2002", format = "\%d-\%m-\%Y"), by = 'day'))
exp_probs <- AbsToProbs(exp, start = list(21, 4), end = list(21, 6))
}