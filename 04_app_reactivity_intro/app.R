#' app goals
#'  - filter a data table and two graphs by species using a reactive function
#'  
#' app anti-aims
#'  - tidyeval (?)

# set up ----
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

species_names <- c("Adelie", "Chinstrap", "Gentoo")

# ui ----
ui <- fluidPage(
  
  titlePanel("Penguin Playground 004"), 
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId = "penguin_species",
                  label = "Select a penguin species",
                  choices = species_names)
    ),
    
    mainPanel(
      DTOutput("penguin_table"),
      plotOutput("bar_graph"),
      plotOutput("scatterplot")
    )
  )
)

# server ----
server <- function(input, output, session) {
  
  species_filter <- reactive({
    penguins %>% 
      filter(species == input$penguin_species)
  })
  
  output$penguin_table <- renderDT(
    species_filter()
  )
  
  output$bar_graph <- renderPlot(
    ggplot(species_filter(), aes(x = island)) +
      geom_bar()
  )
  
  output$scatterplot <- renderPlot(
    ggplot(species_filter(), 
           aes(x = flipper_length_mm, y = body_mass_g)) + 
      geom_point()
  )
}

# run app ----
shinyApp(ui, server)