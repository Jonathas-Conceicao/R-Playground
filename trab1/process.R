#! /usr/bin/Rscript
library(ggplot2)

process <- function(file = "raw_data.csv") {
  table <- read.csv(file, header=FALSE)
  table
}
