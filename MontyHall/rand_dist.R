#! /usr/bin/Rscript
library(ggplot2)
library(random)

## runs <- data.frame(randomNumbers(n=10000, min=1, max=100, col=1))
runs <- data.frame(sample(1:100, 10000, replace=TRUE))
names(runs) <- c("Rand")

plotRuns <- function() {
  ggplot(runs, aes(x=Rand)) +
    scale_x_continuous(limits=c(1, 100), name = "Valor aleatório") +
    scale_y_continuous(name = "Frequencia") +
    geom_freqpoly(bins=100)
}
