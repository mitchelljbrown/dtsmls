#' best_interval - finds the best interval in early time to use for analysis - returns a time range
#'
#' @param x data read from dts (must be the output of "get_instantaneous" which uses "fit_convolve")
#' @param ports port data from the as-built MLS (must be inparticular format)
#' @param plot displays graphical information
#'
#' @return
#' @export
#'
#' @examples

best_time <- function(x, ports, plot=TRUE) {

  # if single test or multiple test
  test_name <- unique(x$type)[1]

  single_test <- x[type==test_name & elapsed_time < 500]

  # create table of average differences between sand and bentonite
  data <- data.table()

  for (time in unique(single_test$elapsed_time)) {
    if (time != '0') {

      # isolate single time
      single_time <- single_test[elapsed_time == as.character(time)]

      # give average slope for each material
      x1 <- single_time[,.(averages=mean(instantaneous_slope)), by=material]

      # subtract average slope for
      x2 <- abs(x1[1,2] - x1[2,2])

      # create data entry
      vec <- data.table(time, as.numeric(x2))

      # append to data.table
      data <- rbind(data, vec)
    }
  }

  # get the time with the biggest difference
  best_time <- data[order(-V2)]$time[1]

  # table of highest to lowest differences
  if (plot==TRUE) {
    plot(data$time, data$V2)
    data[order(-V2)]

    # filter single test and plot profile
    single_test %>%
      filter(elapsed_time == best_time) %>%
      with(plot(x=distance, y=instantaneous_slope))
  }

  return(best_time)

}


# pre processing


# setwd("C:/Users/Mitchell/Documents/dts-data-analysis")
#
# ports <- data.table(read.csv("data/SEN6/ports.csv"))[,X := NULL]
#
# inst_all <- readRDS("data/SEN6/all_inst.rds")
# inst_all[, type := word(type, 1)]
#
# # add column of material*
# inst_all[,material:= ifelse(
#   distance %inrange% list(ports$top, ports$bottom)==TRUE,
#   "Sand", "Bentonite")]
#
# # change thermal conductivity for 2018_08 because of power output*
# inst_all[type=="2018-08-16", thermal_conductivity := ((1.0/instantaneous_slope) * 10 / (4.0*pi))]
#
# inst_single <- inst_all[type=="2020-02-25"]
# inst_single <- drop_na(inst_single)

