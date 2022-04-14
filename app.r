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
library(plyr)
# load in our data
start = proc.time()

files = list.files("datachunks/", ".csv", full.names = TRUE)
zeta <- do.call(rbind, lapply(files, function(x) {
  fread(file = x, sep = ",", header = TRUE, quote = FALSE) }))

zeta$hour <- substr(zeta$timestamp, 8, 12)
zeta$date <- substr(zeta$timestamp, 2, 6)
zeta$month <- strtoi(substr(zeta$date, 0, 2), base = 10L)
zeta$day <- strtoi(substr(zeta$date, 4, 5))

# zeta$weekday <- wday(paste("2019/", zeta$date))

end = proc.time() - start

date_breaks <- unique(zeta$date)[1:365*14]
date_breaks <- date_breaks[!is.na(date_breaks)]

print(end)
# by day of year
# ggplot(data=count(zeta$date), aes(x = x, y=freq)) + geom_bar(stat="identity", fill = "#098CF9") + ggtitle("Daily Rides") + labs(x = "Day", y = "Rides") + scale_x_discrete(breaks = date_breaks)
# add breaks val to scale_x_discrete that's just every 14th date in unique(zeta$date)

# months
monthNums <- seq(1, 12)
months <- month.abb[monthNums]

weekdayNums <- seq(1, 7)
weekdayNums <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

# by month
# ggplot(data=count(zeta$month), aes(x=x, y=freq)) + geom_bar(stat = "identity", fill="#098CF9") + ggtitle("Monthly Rides") + scale_y_continuous(labels = scales::comma) + scale_x_discrete("Months", limits = months) + labs(x="Month", y = "Rides")

# by hour
# ggplot(data=count(zeta$hour), aes(x=x, y=freq)) + geom_bar(stat = "identity", fill="#098CF9") + ggtitle("Hourly Rides") + scale_y_continuous(labels = scales::comma) + scale_x_discrete("Hour", limits = unique(zeta$hour)) + labs(x="Hour", y = "Rides")

# ggplot(data=count(zeta$weekday), aes(x=x, y=freq)) + geom_bar(stat = "identity", fill="#098CF9") + ggtitle("Weekly Rides") + scale_y_continuous(labels = scales::comma) + scale_x_discrete("Hour", limits = weekdayNums) + labs(x="Hour", y = "Rides")


# make binned graphs for mileage and minutes spent (need to make minutes spent col in zeta)
# this histogram works, but is not great at all. 
# ggplot(data=zeta, aes(zeta$miles)) + geom_histogram(bins = 6, binwidth = 2,  fill="#098CF9") + ggtitle("Binned Mileage of Rides") + scale_y_continuous(labels = scales::comma) + scale_x_discrete(limits = c(0, 5, 10, 15, 20, 25)) + labs(x="Miles traveled", y = "Number of Rides")

zeta$minutes <- round(zeta$seconds / 60)
# change limits of this X axis
# ggplot(data=zeta, aes(zeta$minutes)) + geom_histogram(bins = 6, binwidth = 2,  fill="#098CF9") + ggtitle("Binned Length of Rides") + scale_y_continuous(labels = scales::comma) + scale_x_discrete(limits = c(0, 5, 10, 15, 20, 25)) + labs(x="Minutes elapsed", y = "Number of Rides") + scale_x_continuous()


ui <- fluidPage(
  "Hello, world!"
)
server <- function(input, output, session) {
}
shinyApp(ui, server)

