
library(tidyverse)
library(shiny)

stars <- read_csv("data/stars.csv")

glimpse(stars)

ui <- fluidPage(
  sliderInput(inputId = "num", # ID name for the input
              label = "Choose a number", # Label above the input
              value = 25, min = 1, max = 100 # values for the slider
  ),
  textInput(inputId = "title", # new Id is title
            label = "Write a title",
            value = "Histogram of Random Normal Values"), # starting title
  plotOutput("hist"), # Creates space for plot called hist
  verbatimTextOutput("stats") # create a space for stats
)

server <- function(input, output){
  output$hist <- renderPlot({
   
    data <- reactive({
      tibble(x = rnorm(input$num)) # number of points set by the slider
    })
    
     # {} allows us to put all our R code in one nice chunk
    ggplot(data(), aes(x = x)) + # make a histogram
      geom_histogram() +
      labs(title = input$title)
  })
  
  output$stats <- renderPrint({
    summary(data()) # calculate summary stats based on the numbers
  })
}

shinyApp(ui = ui, server = server)
