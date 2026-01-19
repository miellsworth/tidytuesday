# Load libraries
library(tidytuesdayR)
library(shiny)
library(dplyr)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2026-01-20" # Date in YYYY-MM-DD format

# View README
tt_output <- tt_load_gh(last_tues)
readme(tt_output)

# Get the Data
tuesdata <- tidytuesdayR::tt_load(last_tues)

# Tidy data
apod_images <- tuesdata$apod |>
  filter(media_type == "image") |>
  filter(!is.na(url))

# ---- UI ----
ui <- fluidPage(
  titlePanel("Random Picture from the Astronomy Picture of the Day (APOD) Archive"),
  
  actionButton("newImage", "Show Random Image"),
  br(), br(),
  
  uiOutput("img"),
  textOutput("explanation"),
  textOutput("date")
)

# ---- Server ----
server <- function(input, output, session) {
  
  # Reactive value to store the index of the selected image
  currentIndex <- reactiveVal(NULL)
  
  observeEvent(input$newImage, {
    # Pick a random row index
    currentIndex(sample(1:nrow(apod_images), 1))
  })
  
  # Render the image
  output$img <- renderUI({
    req(currentIndex())
    tags$img(
      src = apod_images$url[currentIndex()],
      height = "500px",
      style = "border: 1px solid #ccc; margin-bottom: 10px;"
    )
  })
  
  # Render image explanation
  output$explanation <- renderText({
    req(currentIndex())
    apod_images$explanation[currentIndex()]
  })
  
  # Render image date
  output$date <- renderText({
    req(currentIndex())
    format(apod_images$date[currentIndex()], "%b %d, %Y")
  })
}

# ---- Run app ----
shinyApp(ui, server)
