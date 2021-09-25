# set up ----
library(shiny)
library(dplyr) ## may not need
library(ggplot2)
library(DT)
library(palmerpenguins)

# user interface ====
ui <- fluidPage(
  
  titlePanel("Palmer Penguins basic app"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # select x-axis value for scatterplot ----
      #tags$h2("Scatterplot"),
      selectizeInput(inputId = "x_axis",
                  label = "Select a value for the x-axis: ",
                  choices = c(
                    "",
                    "Bill length (in mm)" = "bill_length_mm",
                    "Bill depth (in mm)" = "bill_depth_mm",
                    "Flipper length (in mm)" = "flipper_length_mm",
                    "Body mass (in g)" = "body_mass_g",
                    "Year" = "year"
                  ),
                  selected = "bill_length_mm"),
      
      # select y-axis value for scatterplot ----
      selectizeInput(inputId = "y_axis",
                     label = "Select a value for the y-axis: ",
                     choices = c(
                       "",
                       "Bill length (in mm)" = "bill_length_mm",
                       "Bill depth (in mm)" = "bill_depth_mm",
                       "Flipper length (in mm)" = "flipper_length_mm",
                       "Body mass (in g)" = "body_mass_g",
                       "Year" = "year"
                     ),
                     selected = "bill_depth_mm"),
      
      # select graph color-by ====
      selectizeInput(inputId = "scatterplot_color",
                     label = "Color points by: ",
                     choices = c(
                       "",
                       "Species" = "species",
                       "Island" = "island", 
                       "Sex" = "sex"
                     ),
                     selected = "island"),
      
      # select scatterplot point size ====
      sliderInput(inputId = "point_size",
                  label = "Select a point size: ",
                  min = 1, max = 10, value = 5),
      
      # select scatterplot point alpha ====
      sliderInput(inputId = "point_alpha",
                  label = "Select an alpha value: ",
                  min = 0, max = 1, value = 0.8, 
                  step = 0.1),
      
      #tags$h2("Data table"),
      # filter by species here ====
      # using for both scatterplot and table -- will create a reactive function
      checkboxGroupInput(inputId = "penguin_species",
                         label = "Filter by species: ",
                         choices = c("Adelie", "Chinstrap", "Gentoo"),
                         selected = c("Adelie", "Chinstrap", "Gentoo")),
      
      # show or hide data table ====
      checkboxInput(inputId = "show_table",
                    label = "Check box to show data table",
                    value = TRUE)
      
    ),
    
    mainPanel(
      
      # summary text
      htmlOutput(outputId = "summary_text"),
      
      # scatterplot (ui) ----
      plotOutput(outputId = "penguin_scatterplot")),
      
      # datatable (ui) ----
      DTOutput(outputId = "penguin_datatable")
    )
  )
)

# server function ----
server <- function(input, output, session) {
  
  # scatterplot (server) ----
  
  # reactive function for penguin species
  filtered_penguins <- reactive({
    req(input$penguin_species)
    filter(penguins, species %in% input$penguin_species)
  })
  
  output$summary_text <- renderUI({
    tags$h3("You're viewing data for ", nrow(filtered_penguins()), 
            "Palmer penguins")
  })
  
  output$penguin_scatterplot <- renderPlot({
    
    # req() to remove error when nothing is selected
    # note what happens when include input$point_size in req()
    req(input$x_axis, input$y_axis, input$scatterplot_color)
    
    # data-masking to allow us to use our selected variables
    ggplot(filtered_penguins(),
           aes(x = .data[[input$x_axis]], y = .data[[input$y_axis]],
               size = input$point_size),
           shape = species) +
      geom_point(aes(color = .data[[input$scatterplot_color]]),
                 # worth mentioning why we don't need data-masking here
                 size = input$point_size,
                 alpha = input$point_alpha) +
      scale_color_manual(values = c("darkorange", "darkorchid", "cyan4")) +
      theme_bw()
  })
  
  # datatable (server) ----

  output$penguin_datatable <- renderDT({
        if(input$show_table){
      DT::datatable(filtered_penguins(),
                    options = list(pageLength = 10))
    }
  })
}

# run Shiny app ====
shinyApp(ui, server)