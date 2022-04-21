# written by kenan arica and kevin elliott for CS 424 proj3
# TODO: Add in tables for the mileage and hour graphs. the mileage/km table needs to convert from miles to km (miles * 1.6) and the hour table simply needs to add a col with 24hr labels instead of 12HR labels.


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
library(rgdal)

# load in our data

start = proc.time()
files = list.files("datachunks/", ".csv", full.names = TRUE)
zeta <- do.call(rbind, lapply(files, function(x) {
  fread(file = x, sep = ",", header = TRUE, quote = FALSE) }))
end = proc.time() - start


comm_areas <- rgdal::readOGR("bound.geojson")
# kevin local code line
#comm_areas <- rgdal::readOGR("Boundaries - Community Areas (current).geojson") 
comm_areas$testData <- as.integer(comm_areas$area_num_1)

bins <- c(0, 10, 20, 30, 40, 50, 60, 70, Inf)
pal <- colorBin("YlOrRd", domain = comm_areas$testData, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%s",
  comm_areas$area_num_1, comm_areas$community
) %>% lapply(htmltools::HTML)



date_breaks <- unique(zeta$date)[1:365*14]
date_breaks <- date_breaks[!is.na(date_breaks)]


print(end)
# by day of year

monthNums <- seq(1, 12)
months <- month.abb[monthNums]

weekdayNums <- seq(1, 7)
weekdayNums <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

hrs_24 <- paste(1:24, ":00", sep = "")
companies <- read.csv("taxi_companies.csv")
companies$X <- NULL
companies <- companies[order(companies$company), ]

communities <- read.csv("communities.csv")
# make separate reactive for % of rides in a community area 
# copy/paste the other reactive code

ui <- dashboardPage(
  dashboardHeader(title = "Chicago Taxi Rides "),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
                title = "Chicagoland Area", 
                         column(1, fluidRow(box(height = "40%")),
                                   fluidRow(box(width = 12, height = "40%",
                                                background = "black",
                                                radioButtons(label = "See data in Mi or Km?", inputId = "kmOrMi", choices = c("Mi", "Km"), selected = "Mi"),
                                                radioButtons(label = "See data in 12HR or 24HR?", inputId  = "hourType", choices = c("12HR", "24HR"), selected = "12HR"),
                                                selectizeInput(label = "Select a Taxi company", inputId = "company", choices = c("All", companies$company), selected = "All"),
                                                selectizeInput(label = "Select a Community Area", inputId = "comm", choices = c("ALL", communities$name), selected = "ALL")
                                                    )
                                            )
                                ),
                         column(8, 
                                fluidRow(
                                  
                                  column(5, column(10, plotOutput("basicDay", height = "35vh")), column(2, dataTableOutput("basicDayTable", height = "35vh"))),
                                  column(5, column(10, plotOutput("toAndFrom", height = "35vh")), column(2, dataTableOutput("toAndFromTable"))),
                                  column(2, column(10, plotOutput("basicMonth", height = "35vh")), column(2, dataTableOutput("basicMonthTable", height = "35vh")))
                                  
                                  ),
                                fluidRow(
                                  column(3, box(column(10, plotOutput("basicWeek")), column(2, dataTableOutput("basicWeekTable")), background = "blue", width = 12)),
                                  column(3, box(conditionalPanel(condition = "input.hourType == '12HR'", column(10, plotOutput("basicHour")), column(2, dataTableOutput("basicHourTable"))),
                                         conditionalPanel(condition = "input.hourType == '24HR'", column(10, plotOutput("basicHour24")), column(2, dataTableOutput("basic24HourTable"))), background = "blue", width = 12)
                                         ),
                                  column(3, box(conditionalPanel(condition = "input.kmOrMi == 'Mi'", column(10, plotOutput("basicMileage")), column(2, dataTableOutput("basicMileageTable"))),
                                         conditionalPanel(condition = "input.kmOrMi == 'Km'", column(10, plotOutput("basicKM")), column(2, dataTableOutput("basicKMTable"))), background = "blue", width = 12)
                                  ),
                                  column(3, box(column(10, plotOutput("basicMinutes")), column(2, dataTableOutput("basicMinsTable")), background = "blue", width = 12))
                                )
                                ),
                          column(3, box(width = 12, leafletOutput("map", height="85vh")))
                          
                ))
server <- function(input, output, session) {
  
  dataReactive <- reactive({
    
    if(input$comm == "ALL" & input$company == "All") { 
      
      zeta

      } 
    else if(input$company == "All" & input$comm != "ALL") { 
      
      # just subset for the community
      subset(zeta, zeta$pickup == communities[communities$name == input$comm, ]$code | zeta$dropoff == communities[communities$name == input$comm, ]$code)
      
    }
    else if(input$company != "All" & input$comm == "ALL") { 
      
      # just subset for the company
      subset(zeta, zeta$company == companies[companies$company == input$company, ]$company_number)        
    }
    else if(input$company != "All" & input$comm != "ALL") { 
      
      # just subset for the company
      subset(zeta, (zeta$pickup == communities[communities$name == input$comm, ]$code | zeta$dropoff == communities[communities$name == input$comm, ]$code) & zeta$company == companies[companies$company == input$company, ]$company_number)
      
    }
    
  })
  
  toAndFromReactive <- reactive({
    # if "All" taxi companies are selected
    if(input$company == "All") { 
        # print("here")
        vals <- data.frame(code=count(zeta$pickup)$x, rides=count(zeta$pickup)$freq + count(zeta$dropoff)$freq)
        vals$percents <- round((vals$rides / sum(vals$rides)) * 100, 3)
        # print("here too")
        vals <- merge(vals, communities, by="code")
        vals <- vals[order(vals$name), ]
        
        vals
    } else { 
      
      zeta_subset <- subset(zeta, zeta$company == companies[companies$company == input$company, ]$company_number)
      # print(head(zeta_subset))
      from <- count(zeta_subset$pickup)
      to <- count(zeta_subset$dropoff)
      
      # cast <- data.frame(x=1:77, freq=0)
      z <- merge(to, from, by="x", all.x = TRUE)
      z[is.na(z)] <- 0
      
      z$freq2 <- z$freq.x + z$freq.y
      print(z)
      #print(to)
      
      vals <- data.frame(code=z$x, rides=z$freq2)
      # print(head(zeta_subset))
      vals$percents <- round((vals$rides / sum(vals$rides)) * 100, 3)
      vals <- merge(vals, communities, by="code")
      vals <- vals[order(vals$name), ]
      
      vals
      }
    
    
  })
  
  # for page 1, chicagoland area. no reactives needed
  # insert plots here 
  output$basicDay <- renderPlot({
    
    mydata <- dataReactive()
    ggplot(data=count(mydata$date), aes(x = x, y=freq)) + geom_bar(stat="identity", fill = "#098CF9") + ggtitle("Daily Rides") + labs(x = "Day", y = "Rides") + scale_x_discrete(breaks = date_breaks)
  })
  output$basicMonth <- renderPlot({
    mydata <- dataReactive()
    ggplot(data=count(mydata$month), aes(x=x, y=freq)) + geom_bar(stat = "identity", fill="#098CF9") + ggtitle("Monthly Rides") + scale_y_continuous(labels = scales::comma) + scale_x_discrete("Months", limits = months) + labs(x="Month", y = "Rides")
  })
  output$basicWeek <- renderPlot({
    mydata <- dataReactive()
    ggplot(data=count(mydata$weekday), aes(x=x, y=freq)) + geom_bar(stat = "identity", fill="#098CF9") + ggtitle("Weekly Rides") + scale_y_continuous(labels = scales::comma) + scale_x_discrete("Day of Week", limits = weekdayNums) + labs(x="Hour", y = "Rides")
  })
  output$basicHour <- renderPlot({
    mydata <- dataReactive()
    ggplot(data=count(mydata$hour), aes(x=x, y=freq)) + geom_bar(stat = "identity", fill="#098CF9") + ggtitle("Hourly Rides") + scale_y_continuous(labels = scales::comma) + scale_x_discrete("Hour", limits = unique(zeta$hour)) + labs(x="Hour", y = "Rides")
  })
  output$basicHour24  <- renderPlot({
    mydata <- dataReactive()
    hrs_24 <- paste(1:24, ":00", sep = "")
    ggplot(data=count(mydata$hour), aes(x=x, y=freq)) + geom_bar(stat = "identity", fill="#098CF9") + ggtitle("Hourly Rides (24HR)") + scale_y_continuous(labels = scales::comma) + scale_x_discrete("Hour", limits = unique(zeta$hour), labels = hrs_24) + labs(x="Hour", y = "Rides")
  })
  
  output$basicMileage <- renderPlot({
    mydata <- dataReactive()
    miles <- cut(mydata$miles, breaks = 10)
    ggplot(count(miles), aes(x = x, y = freq)) + geom_bar(stat = "identity", fill="#098CF9") + scale_y_continuous(labels = scales::comma) + ggtitle("Rides By Mileage") + labs(x = "Miles", y = "Rides")
  })
  
  output$basicKM <- renderPlot({
    mydata <- dataReactive()
    kilo <- mydata$miles * 1.6
    miles <- cut(kilo, breaks = 10)
    ggplot(count(miles), aes(x = x, y = freq)) + geom_bar(stat = "identity", fill="#098CF9") + scale_y_continuous(labels = scales::comma) + ggtitle("Rides By Kilometer") + labs(x = "KM", y = "Rides")
  })
  output$basicMinutes <- renderPlot({
    mydata <- dataReactive()
    mins <- cut(mydata$minutes, breaks = 10)
    ggplot(count(mins), aes(x = x, y = freq)) + geom_bar(stat = "identity", fill="#098CF9") + scale_y_continuous(labels = scales::comma) + ggtitle("Rides By Minutes") + labs(x = "Minutes", y = "Rides")
      })
 
  output$toAndFrom  <- renderPlot({
    mydata <- toAndFromReactive()
    ggplot(data = mydata, aes(x = name, y = percents)) + geom_bar(stat = "identity", fill = "#098CF9") + ggtitle("% Rides to/from each community") + scale_y_continuous(labels = scales::comma) + labs(x="Community", y = "% of Rides") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  })
  
  ############## TABLES 
  output$basicDayTable <- DT::renderDataTable(DT::datatable({
    
    
    mydata <- dataReactive()
    countedMetric <- count(mydata$date)
    table_df <- data.frame(Day=countedMetric$x, Rides=countedMetric$freq)

    data <- table_df
    data
    }, options = list(searching = FALSE, pageLength = 10, lengthMenu = c(7, 10, 20)), rownames = FALSE))
  
  output$basicMonthTable <- DT::renderDataTable(DT::datatable({
    
    mydata <- dataReactive()
    countedMetric <- count(mydata$month)
    table_df <- data.frame(Month=countedMetric$x, Rides=countedMetric$freq)
    data <- table_df
    data
  }, options = list(searching = FALSE, pageLength = 10, lengthMenu = c(7, 10, 20)), rownames = FALSE))
  
  output$basicWeekTable <- DT::renderDataTable(DT::datatable({
    
    mydata <- dataReactive()
    countedMetric <- count(mydata$weekday)
    table_df <- data.frame(Weekday=countedMetric$x, Rides=countedMetric$freq)
    data <- table_df
    data
  }, options = list(searching = FALSE, pageLength = 10, lengthMenu = c(7, 10, 20)), rownames = FALSE))
  
  output$basicHourTable <- DT::renderDataTable(DT::datatable({
    
    mydata <- dataReactive()
    countedMetric <- count(mydata$hour)
    table_df <- data.frame(Hour=countedMetric$x, Rides=countedMetric$freq)
    data <- table_df
    data
  }, options = list(searching = FALSE, pageLength = 10, lengthMenu = c(7, 10, 20)), rownames = FALSE))
  
  output$basic24HourTable <- DT::renderDataTable(DT::datatable({
    
    mydata <- dataReactive()
    countedMetric <- count(mydata$hour)
    table_df <- data.frame(Hour24=hrs_24, Rides=countedMetric$freq)
    data <- table_df
    data
  }, options = list(searching = FALSE, pageLength = 10, lengthMenu = c(7, 10, 20)), rownames = FALSE))
  
  output$basicMileageTable <- DT::renderDataTable(DT::datatable({
    
    mydata <- dataReactive()
    MI <- cut(mydata$miles, breaks = 10)
    countedMetric <- count(MI)
    table_df <- data.frame(Mileage=countedMetric$x, Rides=countedMetric$freq)
    data <- table_df
    data
  }, options = list(searching = FALSE, pageLength = 10, lengthMenu = c(7, 10, 20)), rownames = FALSE))
  
  output$basicKMTable <- DT::renderDataTable(DT::datatable({
    
    mydata <- dataReactive()
    mydata$KM <- mydata$miles * 1.6
    KM <- cut(mydata$KM, breaks = 10)
    countedMetric <- count(KM)
    table_df <- data.frame(Kilometers=countedMetric$x, Rides=countedMetric$freq)
    data <- table_df
    data
  }, options = list(searching = FALSE, pageLength = 10, lengthMenu = c(7, 10, 20)), rownames = FALSE))
  
  output$basicMinsTable <- DT::renderDataTable(DT::datatable({
    
    mydata <- dataReactive()
    
    mins <- cut(mydata$minutes, breaks = 10)
    countedMetric <- count(mins)
    table_df <- data.frame(Minutes=countedMetric$x, Rides=countedMetric$freq)
    data <- table_df
    data
  }, options = list(searching = FALSE, pageLength = 10, lengthMenu = c(7, 10, 20)), rownames = FALSE))
  
  output$toAndFromTable <- DT::renderDataTable(DT::datatable({
    
    mydata <- toAndFromReactive()
    
    table_df <- data.frame(Community=mydata$name, Percent=mydata$percents)
    data <- table_df
    data
  }, options = list(searching = FALSE, pageLength = 10, lengthMenu = c(7, 10, 20)), rownames = FALSE))  
  
  output$map <- renderLeaflet({
    #including dasharray in the highlight options breaks the map
    
    leaflet(comm_areas) %>%
      addTiles() %>%
      addPolygons(stroke = TRUE,
                  fillOpacity = 0.7,
                  #smoothFactor = 0.9,
                  fillColor = ~pal(testData),
                  weight = 2,
                  opacity = 1,
                  dashArray = 3,
                  color = "black",
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textSize = "15px",
                    direction = "auto"
                  )
                  # fillColor = "#ADD8E6"#,
                  #label = ~paste0(area_num_1, ": ", community)
      ) %>%
      addLegend(pal = pal, values = ~testData, opacity = .8, title = "Rides", position = "bottomright") %>%
      addTiles() %>%  
      setView(lng =-87.658323, lat = 41.859036, zoom = 10) %>%
      addProviderTiles("Esri.WorldGrayCanvas")
    
    
  })
}
shinyApp(ui, server)

# add conditionalPanes for 24HR / KM, add tables, and upload to shiny 


