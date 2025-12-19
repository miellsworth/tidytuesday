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
plot_data <- food_security |>
  filter(
    Item == "Prevalence of severe food insecurity in the total population (percent) (3-year average)",
    Area %in% c("Asia", "Africa", "Oceania", "South America", "Central America", "Europe", "Northern America")
  )

plot_data_point <- plot_data |>
  filter(Year_End %in% c(2016, 2024))

plot <- ggplot() +
  geom_line(aes(x = Year_End, y = Value / 100, col = Area), data = plot_data, linewidth = 1.5) +
  geom_point(aes(x = Year_End, y = Value / 100, col = Area), data = plot_data_point, size = 5) +
  geom_text(aes(x = Year_End, y = Value / 100, label = Area, col = Area), data = plot_data_point |> filter(Year_End == 2024), hjust = 0, nudge_x = 0.2) +
  scale_y_continuous(
    labels = scales::percent,
    expand = expansion(mult = c(0.05, 0.05))
    ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.05, 0.2))
    ) +
  scale_colour_hue(c = 40) +
  labs(
    title = str_wrap("Over the last 8 years, the proportion of populations experiencing food insecurty has generally increased across all continents."),
    x = "Year",
    y = "Prevalence (%)",
    color = "Continent",
    caption = "Chart: Michael Ellsworth | Data:  The Food and Agriculture Organization of the United Nations (FAO)",
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "#fbfae4", color = NA),
    plot.background = element_rect(fill = "#fbfae4", color = NA),
    legend.background = element_rect(fill = "#fbfae4", color = NA),
    legend.position = "none"
  )
plot

# Save draft plots
ggsave(here(drafts, paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")))

# Save final
plot_title <- "food_insecurity" # Include informative title
ggsave(
  here(year(last_tues), last_tues, paste0(last_tues, "_", plot_title, ".png")), 
  plot
)
