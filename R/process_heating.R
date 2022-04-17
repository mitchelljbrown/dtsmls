#' process_heating
#'
#' @param x data read from dts
#' @param TOC the depth of top of casing to truncate the borehole
#' @param set_back allow you to set the start of heating before the actual start to see the initial temperature rise
#' @param BOC the depth of bottom of casting to truncate the borehole
#' @param resample set to TRUE if we want to interpolate the distance to another dts test for comparison, ussually a higher resolution test
#' @param ultima specifies the vector or dts dataset to resample to
#'
#' @import data.table
#' @import dplyr
#' @import dts
#'
#' @return
#' @export
#'
#' @examples
process_heating <- function(x, TOC, BOC = 70,
                            resample=FALSE, ultima=NULL,
                            set_back=0, heating='heating', buffer = 0.05) {

  if (resample==TRUE) {
    # interpolate to specified test
    x <- resample_distance(x, ultima)
    # remove na
    x$trace_data <- na.omit(get_data_table(x))
    x$trace_distance <- na.omit(get_distance_table(x))
  }

  #get rid of zero values if present in x
  if (to_matrix(x)[1,1] == '0') {

    x$trace_distance <- get_distance_table(x)[with(x$trace_distance, distance > 0),]
    x$trace_data <- get_data_table(x)[with(x$trace_data, distance > 0),]
  }

  # step find water bath
  x <- find_water_bath(x, buffer = 0.05)

  # step shift to
  x <- bath_calibration(x, smooth = TRUE)

  # step find heating times
  x <- heating_time(x, heating_type = heating)

  # make heating start before actual to visualize initial temp rise

  # find index where heating starts
  num <- x$trace_time[type==heating, which=TRUE]

  # split time data
  heating_start <- x$trace_time[1:(num[1]-(set_back+1)),]
  heating_end <- x$trace_time[(num[1]-set_back):nrow(x$trace_time),]
  heating_end$type <- heating
  x$trace_time <- rbind(heating_end, heating_start)


  # find heating distances
  x <- heating_distance(x, heating_type = heating)

  # subset to borehole
  #x <- subset_distance(x, TOC, BOC)

  #isolate only heating times
  x <- get_time_type(x, time_type = heating)

  #make vectors for elapsed time and log elapsed time
  x$trace_time[type =='heating', log_elapsed_time := log(elapsed_time)]
  x$trace_data$distance <- get_data_table(x)$distance - TOC
  x$trace_distance$distance <- get_distance_table(x)$distance - TOC
  x <- subset_distance(x, 0, (BOC-TOC))

  return(x)
}
