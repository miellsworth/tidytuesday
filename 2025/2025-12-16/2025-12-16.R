# Load libraries
library(tidytuesdayR)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(stringr)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2025-12-16" # Date in YYYY-MM-DD format

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
roundabouts <- tuesdata$roundabouts_clean
roundabout_types <- roundabouts |>
  filter(year_completed != 0) |>
  group_by(year_completed) |>
  count(type) |>
  ungroup() |>
  group_by(type) |>
  mutate(
    running_total = cumsum(n)
  )

# Plot data
plot <- roundabout_types |>
  ggplot(aes(x = year_completed, y = running_total, col = type)) +
  geom_line() +
  labs(
    title = str_wrap(""),
    x = "",
    caption = "Chart: Michael Ellsworth | Data: Kittelson & Associates Roundabouts Data",
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "#fbfae4", color = NA),
    plot.background = element_rect(fill = "#fbfae4", color = NA),
    legend.background = element_rect(fill = "#fbfae4", color = NA),
    legend.box.margin = margin(0, 10, 0, 0),
    legend.position = "right"
  )
plot

# Save draft plots
ggsave(here(drafts, paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")))

# Save final
plot_title <- "roundabouts" # Include informative title
ggsave(
  here(year(last_tues), last_tues, paste0(last_tues, "_", plot_title, ".png")), 
  plot
)
