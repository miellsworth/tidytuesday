# Load libraries
library(tidytuesdayR)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(readr)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2026-01-06" # Date in YYYY-MM-DD format

# Create directories
## Main
dir.create(here(year(last_tues), last_tues))

## Drafts
dir.create(here(year(last_tues), last_tues, "drafts"))

# Get the Data (bring your own)
tracklist <- read_csv("2026/2026-01-06/beat_salad_tracklist.csv")

# Tidy data


# Plot data


# Save draft plots
ggsave(here(drafts, paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")))

# Save final
plot_title <- "" # Include informative title
ggsave(
  here(year(last_tues), last_tues, paste0(last_tues, "_", plot_title, ".png")), 
  plot
)