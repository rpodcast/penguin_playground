# set up ----
library(shiny)
library(stringr)
library(dplyr)
library(ggplot2)
library(readr)
library(DT)
library(tools)
library(palmerpenguins)

# user interface ====
ui <- fluidPage(
  fillPage(padding = c(20, 100, 20, 100)),
  
  title = "Palmer Penguin app",
  
  titlePanel(tags$img(src = "https://allisonhorst.github.io/palmerpenguins/man/figures/palmerpenguins.png", height = "100px", " Palmer Penguins basic app")),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # select x-axis value for scatterplot ----
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
      
      # filter by species  ====
      checkboxGroupInput(inputId = "penguin_species",
                         label = "Filter by species: ",
                         choices = c("Adelie", "Chinstrap", "Gentoo"),
                         selected = c("Adelie", "Chinstrap", "Gentoo")),
      
      # add a plot title ====
      textInput(inputId = "plot_title",
                label = "Plot title",
                placeholder = "Create a title for your plot"),
      
      # show or hide data table ====
      checkboxInput(inputId = "show_table",
                    label = "Check box to show data table",
                    value = TRUE),
      
      # write data to csv ----
      downloadButton(outputId = "download_csv",
                     label = "Download CSV")
      
    ),
    
    mainPanel(
      
      # summary text
      htmlOutput(outputId = "summary_text"),
      br(),
      
      # plot title
      htmlOutput(outputId = "plot_description"),
      
      # scatterplot (ui) ----
      plotOutput(outputId = "penguin_scatterplot"),
      
      # datatable (ui) ----
      DTOutput(outputId = "penguin_datatable"),
      
    )
  )
)

# server function ----
server <- function(input, output, session) {
  
  # scatterplot (server) ----
  
  # reactive function for penguin species ----
  filtered_penguins <- reactive({
    penguins %>% filter(species %in% input$penguin_species)
  })
  
  # # reactive function for plot title ----
  title_case_plot <- reactive({
    tags$h3(toTitleCase(input$plot_title))
  })
  
  # summary text ----
  output$summary_text <- renderUI({
    tags$h4("You're viewing data for ", nrow(filtered_penguins()), 
            "Palmer penguins")
  })
  
  # scatterplot ----
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
      # point out why not using data masking
      labs(x = str_replace_all(input$x_axis, "_", " "),
           y = str_replace_all(input$y_axis, "_", " ")) +
      theme_bw()
  })
  
  # datatable (server) ----
  
  output$penguin_datatable <- renderDT({
    if(input$show_table){
      DT::datatable(filtered_penguins(),
                    options = list(pageLength = 10))
    }
  })
  
  # plot title ----
  # output$plot_description <- renderText({
  #   title_case_plot()
  # })
  output$plot_description <- renderUI({
    title_case_plot()
  })
  
  
  # download .csv of filtered data ----
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("penguins_",
             str_replace_all(Sys.time(), ":|\ ", "_"),
             ".csv")
    },
    content = function(file) {
      write_csv(filtered_penguins(), file)
    }
  )
}

# run Shiny app ====
shinyApp(ui, server)