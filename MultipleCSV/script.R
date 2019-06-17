#!/usr/bin/Rscript
library(ggplot2)

path <- "inputs"
files <- list.files(path = path, pattern = "*.csv", full.names=TRUE)

###################

## One way is to generate a dataframe from all data read
read_fn <- function (file) { read.csv(file, header=FALSE) }
table <- data.frame(lapply(files, read_fn))

ggplot(stack(table), aes(x = ind, y = values)) +
  scale_x_discrete(name = "Each file") +
  scale_y_discrete(name = "Values") +
  geom_boxplot()

###################

## Another way is to generate multiple dataframes and plot them togeter
library(plyr)
read_fn <- function (file) {
  aux <- data.frame(read.csv(file, header=FALSE))
  aux$class <- "Any Other Class"
  aux
}
list <- lapply(files, read_fn)

ggplot(
    dplyr::bind_rows(list, .id="file"),
    aes(y=V1, x=file)
) +
  scale_x_discrete(name = "Each file") +
  scale_y_discrete(name = "Values") +
  geom_boxplot()
