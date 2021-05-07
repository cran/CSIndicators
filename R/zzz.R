.position <- function(dates, ini_day, ini_month, end_day, end_month) {
  days <- as.numeric(format(dates, "%d"))
  months <- as.numeric(format(dates, "%m"))
  pos <- 1:length(dates)
  position <- logical(length(dates))
  if (ini_month != end_month) {
    pos <- sort(unique(c(pos[months == ini_month & days >= ini_day],
                    pos[months < end_month & months > ini_month],
                    pos[months == end_month & days <= end_day])))
    position[pos] <- TRUE
    position[-pos] <- FALSE
  } else {
    pos <- sort(unique(c(pos[months == ini_month &
                    days >= ini_day & days <= end_day])))
    position[pos] <- TRUE
    position[-pos] <- FALSE
  }
  if (!is.null(dim(dates))) {
    dim(position) <- length(position)
    if(!is.null(names(dim(dates)))) {
      names(dim(position)) <- names(dim(dates))
    }
  }
  return(position)
}


#=======================
# Read a powercurve file
# Create the approximation function
#=======================
read_pc <- function(file) {
	pc <- list()

	# Read pc points
	pc$points <- rbind(c(0, 0), read.delim(file, comment.char = "#"))

	# Create an approximating function
	pc$fun <- approxfun(pc$points$WindSpeed, pc$points$Power, method = "linear", yleft = NA, yright = 0)

	# Get the rated power from the power values
	pc$attr$RatedPower <- max(pc$points$Power)
 
	return(pc)
}

#=======================
# Evaluate the linear piecewise approximation function with the wind speed inputs to get wind power
#=======================
wind2power <- function(wind, pc)
{	power <- pc$fun(wind)
	return(power)
}

#=======================
# Convert wind to power, and divide by rated power to obtain Capacity Factor values
#=======================
wind2CF <- function(wind, pc)
{	power <- wind2power(wind, pc)
	CF <- power / pc$attr$RatedPower
	return(CF)
}