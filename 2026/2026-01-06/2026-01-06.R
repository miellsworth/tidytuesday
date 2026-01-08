# Load libraries
library(tidytuesdayR)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(readr)
library(stringr)

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
top_10_artists <- tracklist |>
  filter(artist != "Bird Peterson") |>
  count(artist, sort = TRUE) |>
  slice_head(n = 10)

# Plot data
plot <- top_10_artists |>
  ggplot(aes(x = reorder(artist, n), y = n)) +
  geom_col(fill = "#6B8E23") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = str_wrap("Top 10 most played artists on the radio program Beat Salad"),
    subtitle = str_wrap("Plenty of Canadian content in this list including the Calgary local duo, Sargeant X Comrade"),
    y = "Play Count",
    caption = "Chart and Data: Michael Ellsworth",
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "#fbfae4", color = NA),
    plot.background = element_rect(fill = "#fbfae4", color = NA),
    legend.background = element_rect(fill = "#fbfae4", color = NA),
    legend.box.margin = margin(0, 10, 0, 0),
    axis.ticks.y = element_blank(),
    legend.position = "right",
    axis.title.y = element_blank()
  )
plot

# Save draft plots
ggsave(here(drafts, paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")))

# Save final
plot_title <- "beat_salad_most_played" # Include informative title
ggsave(
  here(year(last_tues), last_tues, paste0(last_tues, "_", plot_title, ".png")), 
  plot
)
