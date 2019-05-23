#!/usr/bin/Rscript
library(ggplot2)

for (i in 1:10) {
  write.table(
      sample(1:100, 1000, replace=TRUE),
      col.names=FALSE,
      row.names=FALSE,
      file=sprintf("inputs/example_%02i.csv", i)
  )
}
