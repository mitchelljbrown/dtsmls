#' Gets instantaneous slopes for each point in time at every depth measurement. Takes a single dts argument or a list of dts arguments to bind output to multiple
#'
#' @param x data read from dts
#' @param power specifies output power of the heating device used to calculate thermal conductivity
#' @param n_knots specifies how to fit the data for fit_convolve()
#'
#' @return
#' @export
#'
#' @examples
get_instantaneous <- function(x, power=15, n_knots=NULL) {

  # generate difference in slopes with fit_convolve
  start_time <- Sys.time()
  df <- fit_convolve(x, n_knots=NULL)
  end_time <- Sys.time()
  total <- end_time - start_time
  print(total)

  df[,instantaneous_slope := delta_temperature/delta_time_log]
  df[,thermal_conductivity := ((1.0/instantaneous_slope) * power / (4.0*pi))]

  return(df)
}

#' gets_instantaneous.list
#'
#' @param dts_list data read from dts
#' @param n_knots specifies how to fit the data for fit_convolve()
#'
#' @return
#' @export
#'
#' @examples
get_instantaneous.list <- function(dts_list, n_knots=NULL) {

  alldata <- list()
  time <- list()
  i <- 1

  for (x in dts_list) {

    # generate difference in slopes with fit_convolve
    start_time <- Sys.time()
    df <- fit_convolve(x, n_knots=NULL)
    end_time <- Sys.time()
    total <- end_time - start_time
    print(total)
    time[i] <- total
    df$instantaneous_slope <- df$delta_temperature/df$delta_time_log
    # df[,cumulative_instansaneous_slope := cumsum(instantaneous_slope)]
    df$type <- x$device$configuration_name
    df$machine <- x$device$type

    alldata[[i]] <- df

    i <- i + 1
  }

  df <- rbindlist(alldata)
  df[,thermal_conductivity := ((1.0/instantaneous_slope) * 15 / (4.0*pi))]


  return(df)
}
