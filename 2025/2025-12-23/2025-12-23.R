# Load libraries
library(tidytuesdayR)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(stringr)
library(patchwork)
library(sf)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2025-12-23" # Date in YYYY-MM-DD format

# View README
tt_output <- tt_load_gh(last_tues)
readme(tt_output)

# Create directories
## Main
dir.create(here(year(last_tues), last_tues))

## Drafts
dir.create(here(year(last_tues), last_tues, "drafts"))

# Get the Data
tuesdata <- tidytuesdayR::tt_load(last_tues)

# Tidy data
endangered_status <- tuesdata$endangered_status
families <- tuesdata$families
languages <- tuesdata$languages

canada_languages <- languages |> 
  left_join(families, join_by(family_id == id)) |>
  left_join(endangered_status, by = "id") |>
  filter(str_detect(countries, "CA"))

# Plot data


# Save draft plots
ggsave(here(drafts, paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")))

# Save final
plot_title <- "" # Include informative title
ggsave(
  here(year(last_tues), last_tues, paste0(last_tues, "_", plot_title, ".png")), 
  plot
)