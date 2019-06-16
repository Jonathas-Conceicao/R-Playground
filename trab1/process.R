#! /usr/bin/Rscript
library(ggplot2)

process <- function(file = "clean_data.csv") {
  table <- read.csv(file, header=TRUE)
  table
}

graph_fn <- function(df, col=1) {
  df <- data.frame(df[,col])
  names(df) <- c("V1")
  ggplot(df, aes(x=V1)) +
  scale_y_continuous(name = "Frequência") +
  geom_histogram(stat="count")
}

t <- process()
for (col in 1:11) {
  print(graph_fn(t, col))
}
