#!/usr/bin/Rscript
library(ggplot2)
library(plyr)

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1) {
  stop("Input file should be supplely as first argument", call.=FALSE)
}
file <- args[1]
table <- read.csv(file, header=FALSE)
table <- data.frame(table[order(table$V1),])
names(table) <- c("V1")

n <- nrow(table)
k <- 1 + (3.32 * log10(n))
at <- max(table$V1) - min(table$V1)
i <- at/k
intervals <- seq(min(table$V1), max(table$V1) + i, by=i)

classes <- findInterval(table$V1, intervals)
table$"Classes"=classes
fTable <- count(table, "Classes")

## table
fTable
library(summarytools)
summ <- summarytools::freq(table$Classes)
summ

mean(table$V1)
var(table$V1)
sd(table$V1)

ggplot(table, aes(x=V1)) +
  scale_x_continuous(name = "Class") +
  scale_y_continuous(name = "Frequency") +
  geom_histogram(breaks=intervals)

ggplot(table, aes(x=V1)) +
  scale_x_continuous(name = "Class") +
  scale_y_continuous(name = "Frequency") +
  geom_freqpoly(breaks=intervals)
