#' get_instantaneous - Gets instantaneous slopes for each point in time at every depth measurement. Takes a single dts argument or a list of dts arguments to bind output to multiple
#'
#' @param x data read from dts
#' @param power specifies output power of the heating device used to calculate thermal conductivity
#' @param n_knots specifies how to fit the data for fit_convolve()
#' @param dts_list
#'
#' @return
#' @export
#'
#' @examples

get_instantaneous <- function(x, power=15, n_knots=NULL) {

  # generate difference in slopes with fit_convolve
  start_time <- Sys.time()
  df <- fit_convolve(x, n_knots=n_knots)
  end_time <- Sys.time()
  total <- end_time - start_time
  print(total)

  df[,instantaneous_slope := delta_temperature/delta_time_log]
  df[,thermal_conductivity := ((1.0/instantaneous_slope) * power / (4.0*pi))]

  return(df)
}

#' get_instantaneous.list
#'
#' @param dts_list list of dts data from different snapshots
#' @param n_knots specifies how to fit the data for fit_convolve()
#'
#' @return
#' @export
#'
#' @examples

get_instantaneous.list <- function(dts_list, ports, n_knots=12) {

  alldata <- list()
  time <- list()
  i <- 1

  for (x in dts_list) {

    # generate instantaneous slope with fit_convolve
    start_time <- Sys.time()

    df <- fit_convolve(x, n_knots=n_knots)

    end_time <- Sys.time()
    total <- end_time - start_time
    print(total)
    time[i] <- total

    df$instantaneous_slope <- df$delta_temperature/df$delta_time_log

    # set the name of the test as the date
    test_name <- x$device$configuration_name
    df$type <- word(test_name, 1)

    # determine machine type (xt or ultima)
    df$machine <- x$device$type

    # append to list of data.tables
    alldata[[i]] <- df

    i <- i + 1
  }

  df <- rbindlist(alldata)
  df[,thermal_conductivity := ((1.0/instantaneous_slope) * 15 / (4.0*pi))]

  # add column of sand/bentonite as per MLS as-built design
  df[,material:= ifelse(
    distance %inrange% list(ports$top, ports$bottom)==TRUE,
    "Sand", "Bentonite")]


  return(df)
}

# test

# setwd("D:/Sentry Well DTS Data/SEN6")
#
# input <- "2022_07/channel 3/out5/dts_data/dts.rds"
#
# dts <- readRDS(input)
#
# string <- dts$device$configuration_name[[1]]
#
# string <- strsplit(string, " ")[[1]]
#
#
# inputs <- c(c("2018_08/channel_1/allfiles/dts_data/dts.rds", 10),
#             c("2019_10/channel_1/allfiles/dts_data/dts.rds", 15),
#             c("2020_02/channel_2/allfiles/dts_data/dts.rds", 15),
#             c("2021_02/channel_2/allfiles/dts_data/dts.rds", 15),
#             c("2021_08/channel_1/allfiles/dts_data/dts.rds", 15),
#             c("2022_07/channel 3/out5/dts_data/dts.rds", 15))
#
# dts <- readRDS(inputs[1])
#
#
#
#
# for (input in inputs) {
#   dts <- readRDS(input)
#
#   string <- dts$device$configuration_name[[1]]
#
#   string <- strsplit(string, " ")[[1]]
#
#   for (x in string) {
#     if (grepl("Wm", x)==TRUE) {
#       power <- as.numeric(gsub("([0-9]+).*$", "\\1", x))
#       print(power)
#     }
#   }
#
# }
#
#
#
#
#
#
#
# for (x in string) {
#   if (grepl("Wm", x)==TRUE) {
#     power <- as.numeric(gsub("([0-9]+).*$", "\\1", x))
#     print(power)
#   }
# }
















