#' read_snaphots
#'
#' @param x data read from dts
#' @param power is the power output of the heating unit to calculate thermal conductivity
#' @param start determines start time to subset data to. default as 0, referring to 0 seconds elapsed time after heating
#' @param end determines end time to subset data to. default as 36000, referring to 36000 seconds elapsed time after heating
#' @param smooth determines if data is smoothed depending on noise. default is false
#' @param out provides 3 output options; 1. 'dtsobject', 2. 'temp' and 2. 'TC' or thermal_conductivity
#'
#'
#' @return
#' @export
#'
#' @examples
read_snapshots <- function(x,
                           power=15,
                           smooth=FALSE,
                           out="TC",
                           heating="heating") {



  x$trace_data <- get_data_table(x)[elapsed_time >= 0 & elapsed_time <= 1800]
  x$trace_time <- get_time_table(x)[elapsed_time >= 0 & elapsed_time <= 1800]

  # return the dts object
  if (out=='dtsobject'){
    return(x)
  }

  # create a data.table with distance as columns, temperature as rows
  heat_matrix <- data.table(t(to_matrix(x)))

  # create vector of elapsed time for heating
  elapsed_time <- x$trace_time[type=='heating']$elapsed_time
  # bind elapsed time with heat_matrix as data.table
  data <- data.table(elapsed_time,heat_matrix)
  # remove first row
  data <- data[-1]

  # find duration for slope loop input
  heating_duration <- max(x$trace_time[type=='heating']$elapsed_time)

  # isolating time
  input <- log(data$elapsed_time)
  output <- data[,elapsed_time:=NULL]

  # smooth all columns in output
  if (smooth==TRUE) {
    output <- data.table(apply(output, 2, function(col) smooth.spline(input, col)$y))
  }

  # return matrix of temperatures with elapsed time, else generate thermal conductivities
  if (out=='temp') {
    return(cbind(input, output))
  }

  # making my own function
  depth <- x$trace_distance$distance
  mat <- as.matrix(output)
  slp <- lm(mat~input)
  out <- summary(slp)

  # get coefficients of regression
  slope <- c()
  ste <- c()
  rsq <- c()
  for (col in out) {
    slope <- append(slope, col$coefficients[2,1])
  }
  for (col in out) {
    ste <- append(ste, col$coefficients[2,2])
  }
  for (col in out) {
    rsq <- append(rsq, col$r.squared)
  }

  # format resultant data.table
  therm <- data.table(depth, slope, rsq, ste)
  therm[,Therm_con := ((1.0/slope) * power / (4.0*pi))]
  therm <- therm[,c('depth', 'Therm_con', 'rsq', 'ste')]

  return(therm)
}
