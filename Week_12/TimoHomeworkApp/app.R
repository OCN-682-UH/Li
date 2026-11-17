##### This is my Shiny App Assignment #####
##### Created by Timothy Li #####
##### Created on 2026-11-15 #####
###################################################


##### Load Libraries #####
library(tidyverse)
library(shiny)
library(here)


##### Read in Data #####
cafe <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-08/cafe.csv")

# cafe <- read.csv(here("Week_12", "TimoHomeworkApp", "Data", "cafe.csv"))
# cafe <- read.csv("Week_12/TimoHomeworkApp/Data/cafe.csv")
# I'm not sure why these kept giving me errors saying that no file or directory exists
# so I loaded in the data directly from github


##### Define UI #####
ui <- fluidPage(
  titlePanel("Exploring The Price of a Cup of Coffee Compared to Hourly Wages Worldwide"), # Title of app
  sidebarLayout(
    sidebarPanel( # Places these selection inputs as a side bar
      selectInput( 
        inputId = "selected_country",
        label = "Choose a Country:",
        choices = c("All", sort(unique(cafe$country))), # Creates "All" as an option and then a label for each country in dataset
        selected = "All" # Start with all
      ),
      selectInput(
        inputId = "currency_choice", 
        label = "Choose Currency:",
        choices = c("GBP (£)" = "gbp", "Original Local Currency" = "local"), # Choose either GBP or currency of country
        selected = "gbp"
      ),
      textInput(inputId = "title", # To create a title for the graph
                label = "Write a title",
                value = "Scatter Plot of Coffee Prices and Wages"),
    ),
    mainPanel(
      plotOutput("cafe_map"), # Have the plot and table in the main panel
      tableOutput("cafe_table")
    )
  )
)

##### Define Server #####
server <- function(input, output) {
  
  filtered_cafe <- reactive({
    req(input$selected_country)
    
    cafe_2 <- cafe |> # Not sure if I need to create another object for this? 
      mutate(
        area_type = ifelse(urban, "Urban", # Adds a column to the table for area type. If TRUE for urban and FALSE for suburban/rural, then area_type = Urban
                    ifelse(suburban, "Suburban",
                    ifelse(rural, "Rural", "Unknown")))
     )
    
    if (input$selected_country == "All") { # When all is selected, use "cafe_2" as displayed table
      cafe_2
    } else { 
      cafe_2 |> filter(country == input$selected_country) # When a specific country is chosen, filter "cafe_2" to only list values from that selected country
    }
  })
  
  output$cafe_table <- renderTable({ # Renaming column headers
    filtered_cafe() |>
      rename(
        "Country" = country,
        "Hourly Wage (GBP)" = hourly_wage_gbp,
        "Coffee Price (GBP)" = price_gbp,
        "Area Type" = area_type,
        "Original Currency" = original_currency,
        "Hourly Wage" = hourly_wage,
        "City" = city,
        "Coffee Price" = price,
        "Urban" = urban,
        "Suburban" = suburban,
        "Rural" = rural
      )
  })
  
  output$cafe_map <- renderPlot({
    req(input$currency_choice) # Requires currency choice as input to determine plot characteristics
    
    if (input$currency_choice == "gbp") { # If in gbp, this first plot
      ggplot(filtered_cafe(), aes(x = price_gbp, y = hourly_wage_gbp)) +
        geom_point(aes(color = area_type), size = 3, alpha = .7) +
        labs(
          title = (title = input$title),
          x = "Coffee Price (GBP)",
          y = "Hourly Wage (GBP)",
          color = "Area Type"
        ) +
        theme_bw() +
        theme(
          plot.title = element_text(face = "bold", size = 15, hjust = .5),
          axis.title.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 12, face = "bold"),
          legend.title = element_text(size = 12, face = "bold"),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.text = element_text(size = 10)
        )
    } else { # If in local currency, this second plot
      ggplot(filtered_cafe(), aes(x = price, y = hourly_wage)) +
        geom_point(aes(color = area_type), size = 3, alpha = .7) + # Color of dot represented by area type
        labs(
          title = (title = input$title),
          x = "Coffee Price (Local Currency)",
          y = "Hourly Wage (Local Currency)",
          color = "Area Type"
        ) + 
        theme_bw() +
        theme(
          plot.title = element_text(face = "bold", size = 15, hjust = .5),
          axis.title.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 12, face = "bold"),
          legend.title = element_text(size = 12, face = "bold"),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.text = element_text(size = 10)
        )
    }
  })
}

##### Run the Application #####
shinyApp(ui = ui, server = server)


