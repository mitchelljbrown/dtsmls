#' optimal_time - finds the time to choose in early data based on the max difference between sand and bentonite slopes
#'
#' @param x data read from dts (must be the output of "get_instantaneous" which uses "fit_convolve")
#' @return
#' @export
#'
#' @examples

optimal_time <- function(x) {

  full_dat <- data.table()

  for (test in unique(x$date)) {

    inst_single <- x[date==test]
    inst_single <- drop_na(inst_single)

    data <- data.table()

    for (time in unique(inst_single$e_time)) {
      if (time != '0') {

        single_time <- inst_single[e_time == time]
        # print(nrow(single_time))

        avg <- single_time[,.(averages=mean(slope)), by=material]

        difference <- avg[material=="Bentonite"]$averages - avg[material=="Sand"]$averages

        machine <- inst_single$machine[1]

        row <- data.table(time, difference, test, machine)

        data <- rbind(data, row)
      }
    }
    full_dat <- rbind(full_dat, data)
  }

  # plot
  plt_difference <- ggplotly(ggplot(full_dat, aes(time, difference, group=test, color=test)) +
                               geom_line() +
                               labs(title = "Average Slope Difference Between Bentonite and Sand",
                                    y = "Slope Difference (Δ°C/Δs)",
                                    x = "Log Elapsed Time (s)"))


  # extract time
  max <- full_dat %>% group_by(test) %>%
    slice_max(difference, with_ties = FALSE)

  # filter original data.table for optimal times taken from "max"
  all_tests <- data.table()

  for (i in seq_len(nrow(max))){
    print(max[i,]$time[[1]])
    print(max[i,]$test[[1]])

    time <- max[i,]$time[[1]]
    date_isolate <- max[i,]$test[[1]]

    single_test <- x[e_time==time & date==date_isolate]

    all_tests <- rbind(all_tests, single_test)
  }

  plt_all_tests <- ggplot(all_tests, aes(distance, slope, group=date)) +
    geom_line(aes(color=date)) +
    ylim(c(0,3))


  print(max)
  print(plt_difference)
  print(plt_all_tests)

  return(list(differences=full_dat, best_time=max, all.tests=all_tests))
}



# # version 1 of funciton
# best_time <- function(x, ports, plot=TRUE) {
#
#   # if single test or multiple test
#   test_name <- unique(x$type)[1]
#
#   single_test <- x[type==test_name & elapsed_time < 500]
#
#   # create table of average differences between sand and bentonite
#   data <- data.table()
#
#   for (time in unique(single_test$elapsed_time)) {
#     if (time != '0') {
#
#       # isolate single time
#       single_time <- single_test[elapsed_time == as.character(time)]
#
#       # give average slope for each material
#       x1 <- single_time[,.(averages=mean(instantaneous_slope)), by=material]
#
#       # subtract average slope for
#       x2 <- abs(x1[1,2] - x1[2,2])
#
#       # create data entry
#       vec <- data.table(time, as.numeric(x2))
#
#       # append to data.table
#       data <- rbind(data, vec)
#     }
#   }
#
#   # get the time with the biggest difference
#   best_time <- data[order(-V2)]$time[1]
#
#   # table of highest to lowest differences
#   if (plot==TRUE) {
#     plot(data$time, data$V2)
#     data[order(-V2)]
#
#     # filter single test and plot profile
#     single_test %>%
#       filter(elapsed_time == best_time) %>%
#       with(plot(x=distance, y=instantaneous_slope))
#   }
#
#   return(best_time)
#
# }
