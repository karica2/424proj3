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
# load in our data
start = proc.time()

files = list.files("datachunks/", ".csv", full.names = TRUE)
zeta <- do.call(rbind, lapply(files, function(x) {
  fread(file = x, sep = ",", header = TRUE, quote = FALSE) }))

zeta$hour <- substr(zeta$timestamp, 8, 12)
zeta$date <- substr(zeta$timestamp, 2, 6)
zeta$month <- strtoi(substr(zeta$date, 0, 2), base = 10L)
zeta$day <- strtoi(substr(zeta$date, 4, 5))


end = proc.time() - start

print(end)
# by day of year
date_breaks <- unique(zeta$date)[1:365*14]
date_breaks <- date_breaks[!is.na(date_breaks)]

monthNums <- seq(1, 12)
months <- month.abb[monthNums]

weekdayNums <- seq(1, 7)
weekdayNums <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

zeta$minutes <- round(zeta$seconds / 60)

# ggplot(data=count(zeta$hour), aes(x=x, y=freq)) + geom_bar(stat = "identity", fill="#098CF9") + ggtitle("Hourly Rides") + scale_y_continuous(labels = scales::comma) + scale_x_discrete("Hour", limits = unique(zeta$hour)) + labs(x="Hour", y = "Rides")

hrs_24 <- paste(1:24, ":00", sep = "")
# ggplot(data=count(zeta$hour), aes(x=x, y=freq)) + geom_bar(stat = "identity", fill="#098CF9") + ggtitle("Hourly Rides") + scale_y_continuous(labels = scales::comma) + scale_x_discrete("Hour", limits = unique(zeta$hour), labels = hrs_24) + labs(x="Hour", y = "Rides")
companies <- read.csv("taxi_companies.csv")
companies$X <- NULL
companies <- companies[order(companies$company), ]

communities <- read.csv("communities.csv")


ui <- dashboardPage(
  dashboardHeader(title = "Chicago Taxi Rides "),
  dashboardSidebar(disable = TRUE),
  dashboardBody("Taxi data tracker",
                title = "Chicagoland Area", 
                         column(2, fluidRow(box(height = "40%")),
                                   fluidRow(box(height = "40%",
                                                background = "black",
                                                radioButtons(label = "See data in Mi or Km?", inputId = "kmOrMi", choices = c("Mi", "Km"), selected = "Mi"),
                                                radioButtons(label = "See data in 12HR or 24HR?", inputId  = "hourType", choices = c("12HR", "24HR"), selected = "12HR"),
                                                selectizeInput(label = "Select a Taxi company", inputId = "company", choices = c("All", companies$company), selected = "All"),
                                                selectizeInput(label = "Select a Community Area", inputId = "comm", choices = c("ALL", communities$name), selected = "ALL")
                                                    )
                                            )
                                ),
                         column(10, 
                                fluidRow(
                                  
                                  column(8, box(column(10, plotOutput("basicDay")), column(2, dataTableOutput("basicDayTable")), background="blue", width = 12)),
                                  column(4, box(column(10, plotOutput("basicMonth")), column(2, dataTableOutput("basicMonthTable")), background = "blue", width = 12))
                                  
                                  ),
                                fluidRow(
                                  column(3, box(column(10, plotOutput("basicWeek")), column(2, dataTableOutput("basicWeekTable")), background = "blue", width = 12)),
                                  column(3, box(conditionalPanel(condition = "input.hourType == '12HR'", column(10, plotOutput("basicHour")), column(2, dataTableOutput("basicHourTable"))),
                                         conditionalPanel(condition = "input.hourType == '24HR'", column(10, plotOutput("basicHour24")), column(2, dataTableOutput("basic24HourTable"))), background = "blue", width = 12)
                                         ),
                                  column(3, box(conditionalPanel(condition = "input.kmOrMi == 'Mi'", plotOutput("basicMileage")),
                                         conditionalPanel(condition = "input.kmOrMi == 'Km'", plotOutput("basicKM")), background = "blue", width = 12)
                                  ),
                                  column(3, box(plotOutput("basicMinutes"), background = "blue", width = 12))
                                )
                                ),
                          
                ))
server <- function(input, output, session) {
  
  dataReactive <- reactive({
    # if both of them are default 
    # if(input$company == "All") { 
    #   
    #   zeta
    # }
    # 
    # else
    #   { 
    #     subset(zeta, zeta$company == companies[companies$company == input$company, ]$company_number)  
    #   }
    # case where company is all 
    # case where community is all 
    
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
    table_df <- data.frame(Hour24=countedMetric$x, Rides=countedMetric$freq)
    data <- table_df
    data
  }, options = list(searching = FALSE, pageLength = 10, lengthMenu = c(7, 10, 20)), rownames = FALSE))
  
  output$basicMileageTable <- DT::renderDataTable(DT::datatable({
    
    mydata <- dataReactive()
    countedMetric <- count(mydata$miles)
    table_df <- data.frame(Mileage=countedMetric$x, Rides=countedMetric$freq)
    data <- table_df
    data
  }, options = list(searching = FALSE, pageLength = 10, lengthMenu = c(7, 10, 20)), rownames = FALSE))
  
  output$basicKMTable <- DT::renderDataTable(DT::datatable({
    
    mydata <- dataReactive()
    countedMetric <- count(mydata$miles)
    table_df <- data.frame(Mileage=countedMetric$x, Rides=countedMetric$freq)
    data <- table_df
    data
  }, options = list(searching = FALSE, pageLength = 10, lengthMenu = c(7, 10, 20)), rownames = FALSE))
}
shinyApp(ui, server)

# add conditionalPanes for 24HR / KM, add tables, and upload to shiny 


