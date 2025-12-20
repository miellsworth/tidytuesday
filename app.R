# Load packages
library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(htmltools)
library(glue)
library(shinythemes)

# Read data directly from GitHub
all_weeks <- read_csv("https://raw.githubusercontent.com/miellsworth/tidytuesday/refs/heads/main/data/all_weeks.csv")

# Save plot titles to an object for use in a dropdown menu
all_titles <- all_weeks$title

# Save package names to an object for use in radio buttons
all_pkgs <- dplyr::select(all_weeks, -c(year, week, title, pkgs, code_fpath, img_fpath))
all_pkgs <- colnames(all_pkgs)

# Create the user interface object ----
ui <- page_sidebar(
  
  # Create a title panel
  title = "Michael's Tidy Tuesdays :)",
  
  # Create a sidebar for user input
  sidebar = sidebar(
    
      # Markdown text to add in some details about the application
      markdown(
      "[Michael Ellsworth](https://github.com/miellsworth)
      
      Explore my Tidy Tuesday viz!"
      ),
      # Select a plot based on title
      shiny::uiOutput("select_img"),
      # display information
      shiny::textOutput("pkgs_used"),
      shiny::htmlOutput("code_link"),
      width = 350
  ),
    # Create a main panel for the plot images
    card(
      shiny::htmlOutput("plot_img")
    )
)

# Create the server object ----

server <- function(input, output) {
  # Get list of available plots
  all_titles <- all_weeks$title
  
  # Select a plot based on title
  output$select_img <- renderUI({
    shiny::selectInput(
      inputId = "plot_title",
      label = "Select a plot:",
      choices = all_titles,
      width = "100%"
    )
  })
  
  # Get data for the selected plot
  week_data <- reactive({
    req(input$plot_title)
    dplyr::filter(all_weeks, title == input$plot_title)
  })
  
  ## Image display
  img_path <- shiny::reactive({
    glue::glue("https://raw.githubusercontent.com/miellsworth/tidytuesday/main/{week_data()$img_fpath}")
  })
  
  output$plot_img <- shiny::renderText({
    c('<img src="', img_path(), '" width="100%">')
  })
  
  ### List of packages
  output$pkgs_used <- shiny::renderText({
    glue::glue(
      "This plot uses the following packages: {week_data()$pkgs}"
    )
  })
  
  ### Code link
  code_path <- shiny::reactive({
    glue::glue("https://github.com/miellsworth/tidytuesday/tree/main/{week_data()$code_fpath}"
    )
  })
  
  output$code_link <- shiny::renderText({
    glue::glue(
      '<b>Code available at: </b> <a href="{code_path()}"  target="_blank">Github</a>'
    )
  })
}

shinyApp(ui = ui, server = server)