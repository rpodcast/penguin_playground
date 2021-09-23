# app goals ----
#' use the palmer penguins data set to show:
#'  - a data table with the penguins
#'  - at least two graphs with the penguins
#'
#' app anti-aims
#'  - reactivity
#'  

# set up ----
library(shiny)
library(palmerpenguins)
library(ggplot2)
library(DT)

ui <- fluidPage(
  titlePanel("Penguin Playground 001"),
  dataTableOutput(outputId = "penguin_table"),
  plotOutput(outputId = "bar_graph"),
  plotOutput(outputId = "scatterplot")
)


server <- function(input, output, session) {
  output$penguin_table <- renderDataTable(penguins)
  
  output$bar_graph <- renderPlot(ggplot(penguins,
                                        aes(x = island, fill = species)) +
                                   geom_bar(alpha  = 0.8))
  
  output$scatterplot <- renderPlot(ggplot(penguins, 
                                          aes(x = flipper_length_mm, y = body_mass_g)) +
                                     geom_point(aes(color = species,
                                                    shape = species)))
}

shinyApp(ui, server)
