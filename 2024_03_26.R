# Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytuesdayR)
library(here)
library(janitor)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2024-03-26"

# Creating directory for draft plots
plt_dir <- paste0("plt_", last_tues)
# dir.create(here("plots", "drafts", plt_dir))

# Get the Data
tuesdata <- tidytuesdayR::tt_load(last_tues)

# Tidy data
results <- tuesdata$`team-results` %>% 
  janitor::clean_names()
  
picks <- tuesdata$`public-picks` %>% 
  janitor::clean_names()

# Transform data
glimpse(results)
glimpse(picks)

# Plot data


# Save draft
# ggsave(
#   here("plots", "drafts", paste0("plt_", last_tues), paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")),
#   plot
# )

# Save final
plot_title <- ""
ggsave(
  here("plots", paste0(last_tues, "_", plot_title, ".png")), 
  plot
)
