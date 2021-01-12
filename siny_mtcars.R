## shiny app with slider input


library(tidyverse)
library(shiny)
library(DT)

data("mtcars")
mtcars


# Slider Bar for carb 
# output1: a text telling the amount of carb selected
# output2 : plot of cyl vs. mpg for the corresponding cars
# output3: Table of all elements for the cor. cars - proper table 


ui <- fluidPage(
  verticalLayout(
    align = "center",
  titlePanel("App with Slider Input"), 
  sliderInput("Ca", "Carb:", min = 1, max = 8, value = 2, step = 1),
  textOutput("text"),
  plotOutput("scatter"),
  tableOutput("table")
  )
)


server <- function(input, output) {
  output$text <- renderText({ paste0("You have selected ", input$Ca)
    })
  output$scatter <- renderPlot({ data_plot <- data.frame(subset(mtcars, carb == input$Ca))
                          ggplot(data_plot, aes(cyl, mpg)) + 
                            geom_point(alpha = 1/2) + 
                            theme_minimal() +
                            ggtitle("MPG per cylinder for cars with a given carb") + 
                            theme(plot.title = element_text(hjust = 0.5, size = 20), 
                                  panel.border = element_rect(colour = "black", fill=NA, size=1/2),
                                  axis.text.y   = element_text(size=18),
                                  axis.text.x   = element_text(size=18),
                                  axis.title.y  = element_text(size=18),
                                  axis.title.x  = element_text(size=18)
                            ) 
  })
  output$table <- renderTable({subset(mtcars, carb == input$Ca) })
}


shinyApp(ui, server)




### App 2 

##  input as dario buttons
# output 1 : text telling one which columns were ticked
# output 2 : table of corresponding columns

rn <- rownames(mtcars)
mtcars$cars <- rn


ui_2 <- fluidPage(
  sidebarLayout(
  sidebarPanel(
  radioButtons("va", "Select column", colnames(mtcars), inline = TRUE),
  textOutput("text")),
  mainPanel(tableOutput("table"))
  )
)

server_2 <- function(input, output) {
  output$text <- renderText({ paste0("You have choosen ", toupper(input$va)   )})
  output$table <- renderTable({data.frame(mtcars %>%
                                            select(cars, input$va) )
  })
}

shinyApp(ui_2, server_2)




### App 3 


# Selecting two vars

## On Page 1 

# Running a regression 
# plotting a regression line 
# updated only when called

## On Page 2 : Table with both vars 


ui_3 <- fluidPage(navbarPage(
  title = "Navbar Shiny App",
  tabPanel(
    "Regression",
    sidebarPanel(
      selectInput("var1", "Select var 1 (x):", choices = names(mtcars) ),
      selectInput("var2", "Select Var 2 (y):", choices = names(mtcars) )), 
    mainPanel(
      plotOutput("plot")
      ))
    ,
  tabPanel("Table",
    tableOutput("sum_table"))
))


##

server_3 <-  function(input, output) {
  output$plot <- renderPlot({
    data_plot <- mtcars %>% 
      select(input$var1, input$var_2)
    colnames(data_plot) <- c("one", "two")
    ggplot(data_plot, aes(two, one)) +
      geom_point()
    #  geom_smooth() 
  })
  output$sum_table <- renderTable({
    data <- mtcars %>% 
      select(input$var_1, input_var_2)
    model <- lm(data[, 2] ~ data[, 1])
    summary(model)[4]
  })
}


shinyApp(ui_3, server_3)




### App 4 - hoovering in a plot 


# selecting two  input variables 
# creating a scatterplot





