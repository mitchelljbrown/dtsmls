
library(dts)
library(dtsmls)
library(earthtide)
library(waterlevel)
library(hydrorecipes)
library(dplyr)
library(tibble)
library(duckdb)
library(data.table)
library(viridis)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(pracma)
library(ggbeeswarm)
library(plotly)

# Casing measurements
TOC <- 29.5782 + 0.57
BOC <- 96.9976

# read in ports data
setwd("C:/Users/Mitchell/Documents/dts-data-analysis")
ports <- data.table(read.csv("data/SEN6/ports.csv"))  # change column 'name' to 'port'

# process from raw
setwd("E:/Sentry Well DTS Data/SEN6")

file_name <- "out5"

inputs <- paste("2022_07/channel 3/", file_name, "/dts_data/dts.rds", sep="")

inputs <- paste("2021_08/channel_1/", file_name, "/dts_data/dts.rds", sep="")

resample <- FALSE
ultima <- NULL
duration <- 36000


process <- list()
i <- 1
for (input in inputs) {
  x <- readRDS(input)

  # account for improperly named test
  if (input == paste("2021_02/channel_2/", file_name, "/dts_data/dts.rds", sep="")) {
    x$device$configuration_name <- "2021-02-25"
  }

  # this test does not work unless heating <- both
  if (input == "2019_05_MLS/channel_1/allfiles/dts_data/dts.rds") {
    heating <- "both"
  } else {
    buffer = 0.1
  }

  # subset distance, bath calibration, find heating time etc.
  df <- dtsmls::process_heating(x, TOC=TOC, BOC=BOC,
                                resample=resample, ultima=ultima)

  # subset time
  df <- dtsmls::read_snapshots(df, out='dtsobject', duration=duration)

  process[[i]] <- df

  print(input)

  i <- i + 1
}

dts <- process[[1]]
generate_slopes.single <- function(dts, ports) {


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
    tmp <- a[between(l_time, tmid[i]-1, tmid[i] + 1)]
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
