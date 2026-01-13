# Load libraries
library(tidytuesdayR)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2026-01-13" # Date in YYYY-MM-DD format

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
africa_languages <- tuesdata$africa

# Plot data
plot <- africa_languages |>
  group_by(country) |>
  summarise(num_languages = n()) |>
  ggplot(aes(x = reorder(country, num_languages), y = num_languages)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Number of Languages Spoken per Country in Africa",
    x = "Country",
    y = "Number of Languages"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
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
plot_title <- "" # Include informative title
ggsave(
  here(year(last_tues), last_tues, paste0(last_tues, "_", plot_title, ".png")), 
  plot
)