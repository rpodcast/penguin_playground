# # app goals ----
#' using the palmerpenguins dataset, create an app that:
#'  - shows an input selection for species
#'  - updates a table with the corresponding input selection

# set up ----
library(shiny)
library(dplyr)
library(palmerpenguins)
library(DT)

# ui ----
ui <- fluidPage(
  titlePanel("Penguin Playground 002"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "penguin_species",
                  label = "Select a penguin species",
                  choices = c("Adelie", "Chinstrap", "Gentoo"))
    ),
    
    mainPanel(
      DTOutput("penguin_table")
    )
  )
)
 # server ----
server <- function(input, output, session) {
  
  output$penguin_table <- renderDT({
    penguins %>% 
      filter(species == input$penguin_species)
  })
    
}

# run app ----
shinyApp(ui, server)