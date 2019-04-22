#! /usr/bin/Rscript

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1) {
  stop("Input file should be supplely as first argument", call.=FALSE)
}
file <- args[1]
table <- read.csv(file, header=FALSE)

print("Media:")
mean (table$V1)
print("Mediana")
median (table$V1)
print ("Moda")
subset (table (table), table (table) == max (table(table)))
