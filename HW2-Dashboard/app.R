# Loading libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(data.table)
library(DT)
library(dplyr)
library(shinydashboard)
library(plotly)


# Loading dataset
data <- read.csv("flight_data.csv")


# App Header
header <- dashboardHeader(title="Flight Data Analysis")


# Sidebar
sidebar <- dashboardSidebar(

    sidebarMenu(
        id = "tabs",

        # Menu items
        menuItem("Plot", icon = icon("chart-bar"), tabName = "plot"),
        menuItem("Table", icon = icon("table"), tabName = "table"),

        # Selection Inputs
        # Flight Origin
        selectInput(inputId = "origin", label = "Flight Origin:", choices = unique(data$Origin), selected = c("PIT"), multiple = FALSE),

        # Month
        sliderInput(inputId = "month", label = "Month:",
                    min = min(data$Month),
                    max = max(data$Month),
                    value = c(min(data$Month), max(data$Month)),
                    step = 1),

        # Year
        selectInput(inputId = "year", label = "Year:", choices = unique(data$Year), selected = c("2018","2019"), multiple = TRUE)
    )
)



# Dashboard body
body <- dashboardBody(tabItems(
    # Plot Tab
    tabItem("plot",
            # Value boxes
            fluidRow(
                valueBoxOutput("valueBox1"),
                valueBoxOutput("valueBox2"),
                valueBoxOutput("valueBox3")
            ),

            # Plots
            fluidRow(
                tabBox(title = "Plots",
                       width = 25,
                       tabPanel("Flight Frequency by Carrier", plotlyOutput("boxPlot")),
                       tabPanel("Flight Frequency by Month", plotlyOutput("linePlot")),
                       tabPanel("Delay Correlation Plot", plotlyOutput("corrPlot"))
                )
            )
    ),

    # Data Table Tab
    tabItem("table",
            fluidPage(
                box(title = "Flight Data", DT::dataTableOutput("table"))
            )
    )
))


ui <- dashboardPage(header, sidebar, body, skin = "black")


# Server Function
server <- function(input, output) {

    # Filtering Data
    filtered_data <- reactive({
        req(input$origin)
        req(input$year)
        req(input$month[1])
        req(input$month[2])
        filter(data,
               Origin == input$origin & Month >= input$month[1] & Month <= input$month[2] & Year %in% input$year)
        })

    # Data table
    output$table <- DT::renderDataTable(
        DT::datatable(data = filtered_data(),
                      options = list(pageLength = 10),
                      rownames = FALSE)
        )

    # Flight Frequency by Carrier Plot
    output$boxPlot <- renderPlotly({
        ggplot(data = filtered_data(), aes(x = Reporting_Airline)) +
            geom_histogram(stat = "count") +
            labs(title = "Flight Frequency by Carrier")

    })

    # Flight Frequency by Month Plot
    output$linePlot <- renderPlotly({
        filtered_data() %>%
            group_by(Month) %>%
            summarise(count = n()) %>%
            as.data.frame() %>%
            ggplot(aes(x = Month, y = count)) +
                geom_line() +
                scale_x_continuous(breaks = seq(1,12,1),
                                   labels=c("1" = "Jan",
                                          "2" = "Feb",
                                          "3" = "Mar",
                                          "4" = "Apr",
                                          "5" = "May",
                                          "6" = "June",
                                          "7" = "July",
                                          "8" = "Aug",
                                          "9" = "Sept",
                                          "10" = "Oct",
                                          "11" = "Nov",
                                          "12" = "Dec"),
                                 limits = c(1, 12)) +
                labs(title = "Flight Frequency by Month")
    })

    # Delay Correlation Plot
    output$corrPlot <- renderPlotly({
        ggplot(data = filtered_data(), aes(x = DepDelay, y = ArrDelay)) +
            geom_point(size = 0.3) +
            labs(title = "Delay Correlation Plot")
    })

    # Value Box 1
    output$valueBox1 <- renderValueBox({
        avgAirTime <- round(mean(filtered_data()$AirTime, na.rm = TRUE), 3)
        valueBox("Average Air Time", value = avgAirTime, icon = icon("chart-pie"), color = "yellow")
    })

    # Value Box 2
    output$valueBox2 <- renderValueBox({
        numCancelled <- sum(filtered_data()$Cancelled, na.rm = TRUE)
        valueBox("Number of flights cancelled", value = numCancelled, icon = icon("chart-pie"), color = "red")
    })

    # Value Box 3
    output$valueBox3 <- renderValueBox({
        totalFlights <- nrow(filtered_data())
        valueBox("Total number of flights", value = totalFlights, icon = icon("chart-pie"), color = "blue")
    })

}


# Run the application
shinyApp(ui = ui, server = server)