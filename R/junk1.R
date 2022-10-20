setwd("C:/Users/Mitchell/Documents/dts-data-analysis/data")


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

data <- fread('head data.csv')

data[2]
colnames(data) <- gsub(" ", "",as.character(data[2]))

# remove first 2 rows as they are redundant and reduce dataframe to relevant columns
data <- data[c(-1,-2)][,c('StationID', 'Date(m/d/a)', 'PortID', "DTW", "MidPortDepth")]

# subset to only sentry wells
data <- data[StationID == 'SEN-05'| StationID == 'SEN-06' | StationID == 'SEN-07']

colnames(data)[2] <- "Date"


# add most recent data
data[Date==""] <- "29/09/2022"

library(ggplot2)
library(plotly)


to_plot <- data[StationID=="SEN-07"]


plt <- ggplot(to_plot, aes(MidPortDepth, DTW, group=Date)) +
  geom_line(aes(color=Date))


ggplotly(plt)


to_save <- data[Date=="9/15/2020"]



write.csv(to_save, "head data single test.csv")







# dates to choose!

# SEN-06
# 11/13/2017
# 02/20/2019
# 6/16/2020
