# app goals ----
#' use the palmer penguins data set to show:
#'  - a data table with the penguins
#'  - at least two graphs with the penguins
#'
#' app anti-aims
#'  - reactivity
#'  - tidyeval
#'  

# set up ----
library(shiny)
library(palmerpenguins)
library(ggplot2)
library(DT)

# ui ----
ui <- fluidPage(
  titlePanel("Penguin Playground 001"),
  DTOutput(outputId = "penguin_table"),
  plotOutput(outputId = "bar_graph"),
  plotOutput(outputId = "scatterplot")
)

# server ----
server <- function(input, output, session) {
  output$penguin_table <- renderDT(penguins)
  
  output$bar_graph <- renderPlot(ggplot(penguins,
                                        aes(x = island, fill = species)) +
                                   geom_bar(alpha  = 0.8))
  
  output$scatterplot <- renderPlot(ggplot(penguins, 
                                          aes(x = flipper_length_mm, y = body_mass_g)) +
                                     geom_point(aes(color = species,
                                                    shape = species)))
}

# run app ----
shinyApp(ui, server)
