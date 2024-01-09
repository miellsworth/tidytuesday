# Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytuesdayR)
library(here)
library(stringr)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2024-01-09"

# Creating directory for draft plots
plt_dir <- paste0("plt_", last_tues)
dir.create(here("plots", "drafts", plt_dir))

# Get the Data
tuesdata <- tidytuesdayR::tt_load('2024-01-09')

# Tidy data

# Transform data

# Plot data

# Save draft
ggsave(
  here("plots", "drafts", paste0("plt_", last_tues), paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")),
  plot
)

# Save final
plot_title <- ""
ggsave(
  here("plots", paste0(last_tues, "_", plot_title, ".png")), 
  plot
)