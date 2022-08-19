# testing new functions
library(dts)
library(gridExtra)
library(dplyr)
library(tibble)
library(duckdb)
library(data.table)
library(viridis)
library(tidyverse)
library(plotly)
library(ggplot2)
library(mmand)
library(earthtide)
library(waterlevel)
library(hydrorecipes)
library(splines)
library(gifski)
library(cowplot)
library(gganimate)
library(dtsmls)
setwd("C:/Users/Mitchell/Documents/dts-data-analysis")


# Backfill Data
ports <- data.table(read.csv("data/SEN6/ports.csv"))[,X := NULL]

#' *Get instantaneous for All*

TOC <- 29.5782 + 0.57
BOC <- 96.9976

setwd("C:/Users/Mitchell/Desktop/Thesis Data/rawSenData/SEN6")
inputs <- c(#"2019_05_MLS/channel_1/allfiles/dts_data/dts.rds",
            "2018_08_MLS/channel_1/allfiles/dts_data/dts.rds",
            "2019_10_MLS/channel_1/allfiles/dts_data/dts.rds",
            "2020_02_MLS/channel_2/allfiles/dts_data/dts.rds",
            "2021_02_MLS/channel_2/allfiles/dts_data/dts.rds",
            "2021_08_MLS/channel_1/allfiles/dts_data/dts.rds")

setwd("D:/Sentry Well DTS Data/SEN6")

input <- "2019_10_MLS/channel_1/allfiles/dts_data/dts.rds"


x <- readRDS(input)

if (input == "2021_02_MLS/channel_2/allfiles/dts_data/dts.rds") {
  x$device$configuration_name <- "2021-02-25"
}

if (input == "2019_05_MLS/channel_1/allfiles/dts_data/dts.rds") {
  heating <- "both"
} else {
  buffer = 0.1
}

df <- process_heating(x, TOC=TOC, BOC=BOC)

df <- read_snapshots(df, out='dtsobject', duration = 36000)

process[[i]] <- df

print(input)



d <- get_instantaneous.list(process, n_knots=12)
setwd("C:/Users/Mitchell/Documents/dts-data-analysis")
saveRDS(d, "data/SEN6/all_inst.rds")
