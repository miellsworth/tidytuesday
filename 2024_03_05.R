# Load libraries
library(tidytuesdayR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2024-03-05"

# View README
tt_output <- tt_load_gh(last_tues)
readme(tt_output)

# Creating directory for draft plots
plt_dir <- paste0("plt_", last_tues)
dir.create(here("plots", "drafts", plt_dir))

# Get the Data
tuesdata <- tidytuesdayR::tt_load(last_tues)

# Tidy data
glimpse(tuesdata$trashwheel)
df <- tuesdata$trashwheel

# Transform data


# Plot data
plot <- ggplot()

# Save draft
# ggsave(here("plots", "drafts", paste0("plt_", last_tues), paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")))

# Save final
plot_title <- ""
ggsave(
  here("plots", paste0(last_tues, "_", plot_title, ".png")), 
  plot
)