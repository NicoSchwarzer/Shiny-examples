

library("shiny")
library("plotly")
library("tidyverse")




# creating some (fake) data on cties

cities <- c('Berlin', 'Munich', 'Hamburg', 'Cologne', 'Frankfurt', 'Stuttgart', 'Dusseldorf')
pop <- c(3000000, 1700000, 1500000, 100000, 700000, 600000, 550000)
pop_1990 <- c(1000000,1200000,900000, 800000, 600000, 500000, 400000)
square_miles <- c(30,24,20,17,14,19,15)


df1 <- data.frame(cities, pop, square_miles)


View(df1)



##
## example 1 - infos on the city
##


ui <- fluidPage(
  titlePanel("Give me some info on ...?"),
  selectInput('city', 'Select City', df1$cities),
  DT::DTOutput('table_city')
)


server <- function(input, output, session){
  output$table_city <- DT::renderDT({
    df1 %>% 
      filter(cities == input$city)
  })
}

shinyApp(ui = ui, server = server)


##
## example 2 - infos on a city with at least a certain population
##

ui <- fluidPage(
  titlePanel("Give me some info on ...?"),
  selectInput('city', 'Select City', df1$cities),
  numericInput('pop_min', 'Select min population', 1000000),
  DT::DTOutput('table_city')
)



server <- function(input, output, session){
  output$table_city <- DT::renderDT({
    df1 %>% 
      filter(pop >= input$pop_min) %>%
      filter(cities == input$city)
  })
}

shinyApp(ui = ui, server = server)



### example 3 
## plotting the populations, respectively

ui <- fluidPage(
  titlePanel("Plotting the population"),
  DT::DTOutput('table_city'),
  plotOutput('plot1')
)


server <- function(input, output, session) {
  output$table_city <- DT::renderDT(df1) 
  output$plot1 <- renderPlot({
    ggplot(data=df1, aes(x=cities, y= pop)) +
      geom_bar(colour = "blue", stat="identity")
  })
}

shinyApp(ui = ui, server = server)


### example 4 
## plotting pop and the size of certain cities


ui <- fluidPage(
  titlePanel("Population vs size"),
  numericInput('min_size', 'select min size',  20),
  plotOutput('plot2')
)


server <- function(input, output, session) {
  output$plot2 <- renderPlot({
    df2 <- df1 %>%
      filter(square_miles >= input$min_size)
    ### 
    ggplot(data=df2, aes(x= pop, y= square_miles)) +
      geom_point(colour = "red", size=5, shape=23)
  })
}


shinyApp(ui = ui, server = server)







