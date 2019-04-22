#!/usr/bin/Rscript
library(ggplot2)

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1) {
  stop("Input file should be supplely as first argument", call.=FALSE)
}
file <- args[1]
table <- read.csv(file, header=FALSE)
table <- data.frame(table[order(table$V1),])
names(table) <- c("V1")
## table

ggplot(table, aes(y=V1)) +
  scale_x_continuous(name = "X", limits=c(-2,2)) +
  scale_y_continuous(name = "Y") +
  geom_boxplot()

stem(table$V1)
