# written by kenan arica and kevin elliott for CS 424 proj3

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(grid)
library(scales)
library(leaflet)
library(data.table)


# load in our data
start = proc.time()
files = list.files("datachunks/", ".csv", full.names = TRUE)
data <- do.call(rbind, lapply(files, function(x) {
  fread(file = x, sep = ",", header = TRUE) }))
end = proc.time() - start
print(end)