rm(list = ls())
graphics.off()
setwd("Documents/Aflatoxin_project/data/")

#import the csv file
data <- read.csv(file = "Aflatoxin_literature.csv", header = FALSE)
