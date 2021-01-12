### Shiny data structures


library(mosaicData)
library(shiny)
library(tidyverse)


# App 1 

# displaying the average temp as well as the precipitation for a given city on a given day


ui <- fluidPage(
  dateInput(
    "date",
    "Date:",
    mi = min(Weather$date),
    ma = max(Weather$date),
    value = min(Weather$date)
  ),
  selectInput("place", "Choose you place (i.e. city)", choices = unique(Weather$city)),
  textOutput("text")
)


server <- function(input, output) {
  output$text <- renderText({
    paste0(
      "On ",
      input$date,
      ", the average temperature in ",
      input$place,
      " was ",
      round(Weather$avg_temp[Weather$date == input$date &
                                Weather$city == input$city]), " degrees Celsius.")  
  })
}

shinyApp(ui, server)



##  App 2 

# vizualising the average temperature for a given city for a given time frame 

# using a submit buttom 

library(shiny)
library(ggplot2)


ui_2 <- fluidPage(
  dateRangeInput(
    "date",
    "Date:",
    mi = min(Weather$date),
    ma = max(Weather$date),
    start = min(Weather$date),
    end = max(Weather$date),
    startview = "year"
  ),
  selectInput("place", "Chose the places:", choices = unique(Weather$city)),
  plotOutput("plot"), 
  submitButton("Plot for a different city!", icon = icon("refresh"))
)


##

server_2 <- function(input, output) {
  output$plot <- renderPlot({
    df_x <- subset(Weather, city == input$place)
    ggplot(df_x, aes(date,  avg_temp)) +
      geom_line() +
      coord_cartesian(xlim = c(input$date[1], input$date[2]))
  })
}



shinyApp(ui_2, server_2)

      