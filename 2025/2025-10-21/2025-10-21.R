# Load libraries
library(tidytuesdayR)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(stringr)
library(forcats)
library(patchwork)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2025-10-21" # Date in YYYY-MM-DD format

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
station <- tuesdata$historic_station_met
station_meta <- tuesdata$station_meta

top_10_rain <- station |>
  mutate(date = ym(paste(year, month)), station = str_to_title(station)) |>
  group_by(station) |>
  summarise(avg_rain = mean(rain, na.rm = TRUE)) |>
  arrange(desc(avg_rain)) |>
  head(10)

top_10_tmax <- station |>
  mutate(date = ym(paste(year, month)), station = str_to_title(station)) |>
  group_by(station) |>
  summarise(avg_tmax = mean(tmax, na.rm = TRUE)) |>
  arrange(desc(avg_tmax)) |>
  head(10)

top_10_tmin <- station |>
  mutate(date = ym(paste(year, month)), station = str_to_title(station)) |>
  group_by(station) |>
  summarise(avg_tmin = mean(tmin, na.rm = TRUE)) |>
  arrange(desc(avg_tmin)) |>
  head(10)

# Plot data
plot1 <- top_10_rain |>
  ggplot(aes(x = fct_reorder(station, avg_rain), y = avg_rain)) +
  geom_col(width = 0.05) +
  geom_point(size = 12) +
  geom_text(aes(label = round(avg_rain, 0)), color = "white") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  coord_flip() +
  labs(
    subtitle = str_wrap("Top 10 rainfall stations"),
    x = "Weather stations",
    y = "Average rain (mm)"
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "#fbfae4", color = NA),
    plot.background = element_rect(fill = "#fbfae4", color = NA),
    legend.background = element_rect(fill = "#fbfae4", color = NA),
    axis.ticks.y = element_blank()
  )
plot1

plot1 <- top_10_rain |>
  ggplot(aes(x = fct_reorder(station, avg_rain), y = avg_rain)) +
  geom_col(width = 0.05) +
  geom_point(size = 12) +
  geom_text(aes(label = round(avg_rain, 0)), color = "white") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  coord_flip() +
  labs(
    subtitle = str_wrap("Top 10 rainfall stations"),
    x = "Weather stations",
    y = "Average rain (mm)"
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "#fbfae4", color = NA),
    plot.background = element_rect(fill = "#fbfae4", color = NA),
    legend.background = element_rect(fill = "#fbfae4", color = NA),
    axis.ticks.y = element_blank()
  )
plot1

# Save draft plots
ggsave(here(drafts, paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")))

# Save final
plot_title <- "" # Include informative title
ggsave(
  here(year(last_tues), last_tues, paste0(last_tues, "_", plot_title, ".png")), 
  plot
)