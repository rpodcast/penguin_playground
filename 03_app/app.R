#' app goals:
#'  - filter table and two graphs based on species
#'  - motivate reactivity
#' 
#' app anti-aims:
#'  - reactivity
#'  - tidyeval

# set up ----
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# ui ----
ui <- fluidPage(
  
  titlePanel("Penguin Playground 003"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId = "penguin_species",
                  label = "Select a penguin species",
                  choices = c("Adelie", "Chinstrap", "Gentoo"))
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
  
  output$penguin_table <- renderDT(
    penguins %>% 
      filter(species == input$penguin_species)
  )
  
  output$bar_graph <- renderPlot(
    penguins %>% 
      filter(species == input$penguin_species) %>% 
      ggplot(aes(x = island)) +
      geom_bar()
  )
  
  output$scatterplot <- renderPlot(
    penguins %>% 
      filter(species == input$penguin_species) %>% 
      ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
      geom_point()
  )
  
}

# run app ----
shinyApp(ui, server)