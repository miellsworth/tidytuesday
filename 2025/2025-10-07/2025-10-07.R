# Load libraries
library(tidytuesdayR)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(janitor)
library(forcats)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2025-10-07" # Date in YYYY-MM-DD format

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
euroleague <- tuesdata$euroleague_basketball |>
  clean_names()
glimpse(euroleague)

# Plot data
plot <- euroleague |>
  filter(final_four_appearances > 0) |>
  ggplot(aes(x = fct_reorder(team, final_four_appearances), y = final_four_appearances)) +
  geom_col() +
  geom_text(
    aes(
      x = team, 
      y = 0, 
      label = team,
      hjust = 0,
    ),
    col = "white",
    nudge_y = 0.1
  ) +
  geom_text(
    aes(
      x = team, 
      y = final_four_appearances, 
      label = final_four_appearances,
      hjust = 0,
    ),
    col = "black",
    nudge_y = 0.1
  ) +
  coord_flip() +
  theme_classic() +
  labs(
    title = str_wrap("Euroleague Men's Basketball - Final Four Appearances"),
    caption = "Chart: Michael Ellsworth | Data:  EuroLeague Basketball"
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.title.x = element_blank(),
    panel.background = element_rect(fill = "#fbfae4", color = NA),
    plot.background = element_rect(fill = "#fbfae4", color = NA),
    legend.background = element_rect(fill = "#fbfae4", color = NA),
  )
plot

# Save draft plots
ggsave(here(drafts, paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")))

# Save final
plot_title <- "euroleague" # Include informative title
ggsave(
  here(year(last_tues), last_tues, paste0(last_tues, "_", plot_title, ".png")), 
  plot
)
