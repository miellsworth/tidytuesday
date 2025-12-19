# Load libraries
library(tidytuesdayR)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2025-10-14" # Date in YYYY-MM-DD format

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
food_security <- tuesdata$food_security
food_security$Item |> unique()

# Plot data
food_security |>
  filter(
    Item == "Prevalence of severe food insecurity in the total population (percent) (3-year average)",
    Area %in% c("Asia", "Africa", "Oceania", "South America", "Central America", "Europe", "North America")
  ) |>
  ggplot(aes(x = Year_End, y = Value, col = Area)) +
  geom_line()

# Save draft plots
ggsave(here(drafts, paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")))

# Save final
plot_title <- "" # Include informative title
ggsave(
  here(year(last_tues), last_tues, paste0(last_tues, "_", plot_title, ".png")), 
  plot
)