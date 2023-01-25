#' generate_slope - generate instantaneous slopes using moving window method
#'
#' @param dts data read from dts
#' @param ports
#' @param n set the distance to calcualte slope
#' @return
#' @export
#'
#' @examples

generate_slopes <- function(dts, ports, n=1) {


  alldata <- data.table()

  for (x in dts) {

    start_time <- Sys.time()

    # temperature table
    dat <- get_data_table(x)
    # times table
    tm  <- get_time_table(x)

    # join to match elapsed time
    a <- dat[tm[type == 'heating', list(start,
                                        e_time = elapsed_time,
                                        l_time = log(elapsed_time))], on = 'start']

    # remove log(0)
    a <- a[is.finite(l_time)]

    # get the measurement midpoint
    tmid <- sort(unique(a$l_time))
    slp <- list()
    for(i in 1:(length(tmid)-1)) {
      # one log cycle on each side of measurement point
      tmp <- a[between(l_time, tmid[i]-n, tmid[i] + n)]
      # fit model can use lm, rlm, glmnet
      slp[[i]] <- tmp[,
                      list(slope = pmax(0, coef(MASS::rlm(temperature~l_time))[2]),
                           e_time = exp(tmid[i]),
                           l_time=tmid[i]),
                      by = distance]


      dat$elapsed_time <- round(dat$elapsed_time, digits=3)
      slp[[i]]$e_time <- round(slp[[i]]$e_time, digits=3)

      time <- slp[[i]]$e_time[1]
      slp[[i]]$temperature <- dat[elapsed_time==time]$temperature
    }

    end_time <- Sys.time()
    total <- end_time - start_time
    print(total)

    slp <- rbindlist(slp)

    # join to add temperatures
    slp[,material:= ifelse(
      distance %inrange% list(ports$top, ports$bottom)==TRUE,
      "Sand", "Bentonite")]

    # add machine type (XT or Ultima)
    slp$machine <- x$device$type

    # add date
    slp$date <- word(x$device$configuration_name, 1)

    # bind to master DT
    alldata <- rbind(alldata, slp)

  }

  return(alldata)
}


generate_slopes.single <- function(dts, ports, n) {


  x <- dts

  start_time <- Sys.time()

  # temperature table
  dat <- get_data_table(x)
  # times table
  tm  <- get_time_table(x)

  # join to match elapsed time
  a <- dat[tm[type == 'heating', list(start,
                                      e_time = elapsed_time,
                                      l_time = log(elapsed_time))], on = 'start']

  # remove log(0)
  a <- a[is.finite(l_time)]

  # get the measurement midpoint
  tmid <- sort(unique(a$l_time))
  slp <- list()
  for(i in 1:(length(tmid)-1)) {
    # one log cycle on each side of measurement point
    tmp <- a[between(l_time, tmid[i]-n, tmid[i] + n)]
    # fit model can use lm, rlm, glmnet
    slp[[i]] <- tmp[,
                    list(slope = pmax(0, coef(MASS::rlm(temperature~l_time))[2]),
                         e_time = exp(tmid[i]),
                         l_time=tmid[i]),
                    by = distance]


    dat$elapsed_time <- round(dat$elapsed_time, digits=3)
    slp[[i]]$e_time <- round(slp[[i]]$e_time, digits=3)

    time <- slp[[i]]$e_time[1]
    slp[[i]]$temperature <- dat[elapsed_time==time]$temperature
  }

  end_time <- Sys.time()
  total <- end_time - start_time
  print(total)

  slp <- rbindlist(slp)

  # join to add temperatures
  slp[,material:= ifelse(
    distance %inrange% list(ports$top, ports$bottom)==TRUE,
    "Sand", "Bentonite")]

  # add machine type (XT or Ultima)
  slp$machine <- x$device$type

  # add date
  slp$date <- word(x$device$configuration_name, 1)

  return(slp)
}
