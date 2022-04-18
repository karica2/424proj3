# written by kenan arica >:( and kevin elliott for CS 424 proj3

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

## Kevin libraries for map
# if this doesn't work on your side you might have to install sf and here
library(sf)
library(here)


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

# Community area map code
# commented out but if we wanted the data from the tsv its here, probably not needed though
#commAreas <- read.table(file = "CommAreas.tsv", sep = "\t", header = TRUE, quote = "" , fill=TRUE, row.names = NULL)
# this command only works if all 4 of the geo_export files are in the project even though it only uses the .shp one
comm_areas <- here("geo_export_a2580528-8f69-4499-9852-36e55c978a3b.shp") %>%
  st_read()

# p is testing to see plot without map
p <- ggplot(data = comm_areas)+ geom_sf()
#p <- p + geom_point()
#print(p)
# printPlot is used for leaflet map
printPlot <- st_transform(comm_areas)

# Text display on community area Based off the Leaflet for R tutorial found at
# https://rstudio.github.io/leaflet/choropleths.html
labels <- sprintf(
  "<strong>Community Area: %s</strong><br/> %s",
  comm_areas$area_num_1, comm_areas$community
) %>% lapply(htmltools::HTML)


ui <- fluidPage(
  leafletOutput("map")
)
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    #including dasharray in the highlight options breaks the map
    
    leaflet(printPlot) %>%
      addPolygons(
        stroke = TRUE,
        fillOpacity = 0.1, smoothFactor = 0.9,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#600",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      addTiles() %>%  
      setView(lng =-87.658323, lat = 41.879036, zoom = 12) %>%
      addProviderTiles("Esri.WorldGrayCanvas")
    
    
  })
}
shinyApp(ui, server)

