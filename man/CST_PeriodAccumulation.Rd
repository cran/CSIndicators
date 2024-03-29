% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PeriodAccumulation.R
\name{CST_PeriodAccumulation}
\alias{CST_PeriodAccumulation}
\title{Period Accumulation on 's2dv_cube' objects}
\usage{
CST_PeriodAccumulation(
  data,
  start = NULL,
  end = NULL,
  time_dim = "time",
  rollwidth = NULL,
  sdate_dim = "sdate",
  frequency = "monthly",
  na.rm = FALSE,
  ncores = NULL
)
}
\arguments{
\item{data}{An 's2dv_cube' object as provided function \code{CST_Start} or 
\code{CST_Load} in package CSTools.}

\item{start}{An optional parameter to defined the initial date of the period 
to select from the data by providing a list of two elements: the initial 
date of the period and the initial m   onth of the period. By default it is 
set to NULL and the indicator is computed using all the data provided in 
\code{data}.}

\item{end}{An optional parameter to defined the final date of the period to 
select from the data by providing a list of two elements: the final day of 
the period and the final month of the period. By default it is set to NULL 
and the indicator is computed using all the data provided in \code{data}.}

\item{time_dim}{A character string indicating the name of the dimension to 
compute the indicator. By default, it is set to 'time'. More than one 
dimension name matching the dimensions provided in the object 
\code{data$data} can be specified.}

\item{rollwidth}{An optional parameter to indicate the number of time 
steps the rolling sum is applied to. If it is positive, the rolling sum is 
applied backwards 'time_dim', if it is negative, it will be forward it. When 
this parameter is NULL, the sum is applied over all 'time_dim', in a 
specified period. It is NULL by default.}

\item{sdate_dim}{(Only needed when rollwidth is used). A character string 
indicating the name of the start date dimension to compute the rolling 
accumulation. By default, it is set to 'sdate'.}

\item{frequency}{(Only needed when rollwidth is used). A character string 
indicating the time frequency of the data to apply the rolling accumulation.  
It can be 'daily' or 'monthly'. If it is set to 'monthly', values from
continuous months will be accumulated; if it is 'daliy', values from 
continuous days will be accumulated. It is set to 'monthly' by default.}

\item{na.rm}{A logical value indicating whether to ignore NA values (TRUE) or 
not (FALSE).}

\item{ncores}{An integer indicating the number of cores to use in parallel 
computation.}
}
\value{
An 's2dv_cube' object containing the accumulated data in the element
\code{data}. If parameter 'rollwidth' is not used, it will have the dimensions
of the input parameter 'data' except the dimension where the accumulation has
been computed (specified with 'time_dim'). If 'rollwidth' is used, it will be 
of same dimensions as input data. The 'Dates' array is updated to the 
dates corresponding to the beginning of the aggregated time period. A new 
element called 'time_bounds' will be added into the 'attrs' element in the 
's2dv_cube' object. It consists of a list containing two elements, the start 
and end dates of the aggregated period with the same dimensions of 'Dates' 
element. If 'rollwidth' is used, it will contain the same dimensions of 
parameter 'data' and the other elements of the 's2dv_cube' will not be 
modified.
}
\description{
Period Accumulation computes the sum (accumulation) of a given variable in a 
period. Providing precipitation data, two agriculture indices can be obtained 
by using this function:
\itemize{
 \item{'SprR', Spring Total Precipitation: The total precipitation from 
       April 21th to June 21st.}
 \item{'HarR', Harvest Total Precipitation: The total precipitation from 
       August 21st to October 21st.}
}
}
\details{
There are two possible ways of performing the accumulation. The default one 
is by accumulating a variable over a dimension specified with 'time_dim'. To 
chose a specific time period, 'start' and 'end' must be used. The other method 
is by using 'rollwidth' parameter. When this parameter is a positive integer, 
the cumulative backward sum is applied to the time dimension. If it is 
negative, the rolling sum is applied backwards. This function is build to  
be compatible with other tools in that work with 's2dv_cube' object class. The 
input data must be this object class. If you don't work with 's2dv_cube', see 
PeriodAccumulation.
}
\examples{
exp <- NULL
exp$data <- array(rnorm(216)*200, dim = c(dataset = 1, member = 2, sdate = 3, 
                 ftime = 9, lat = 2, lon = 2))
class(exp) <- 's2dv_cube'
TP <- CST_PeriodAccumulation(exp, time_dim = 'ftime')
exp$data <- array(rnorm(5 * 3 * 214 * 2), 
                 c(memb = 5, sdate = 3, ftime = 214, lon = 2)) 
Dates <- c(seq(as.Date("01-05-2000", format = "\%d-\%m-\%Y"), 
              as.Date("30-11-2000", format = "\%d-\%m-\%Y"), by = 'day'),
          seq(as.Date("01-05-2001", format = "\%d-\%m-\%Y"), 
              as.Date("30-11-2001", format = "\%d-\%m-\%Y"), by = 'day'),
          seq(as.Date("01-05-2002", format = "\%d-\%m-\%Y"), 
              as.Date("30-11-2002", format = "\%d-\%m-\%Y"), by = 'day'))
dim(Dates) <- c(sdate = 3, ftime = 214)
exp$attrs$Dates <- Dates
SprR <- CST_PeriodAccumulation(exp, start = list(21, 4), end = list(21, 6),
                              time_dim = 'ftime')
dim(SprR$data)
head(SprR$attrs$Dates)
HarR <- CST_PeriodAccumulation(exp, start = list(21, 8), end = list(21, 10),
                              time_dim = 'ftime')
dim(HarR$data)
head(HarR$attrs$Dates)

}
