# Load libraries
library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapproj)
library(tidyr)
library(tidytuesdayR)
library(here)
library(janitor)
library(stringr)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2024-04-09"

# View README
tt_output <- tt_load_gh(last_tues)
readme(tt_output)

# Create directory
# dir.create(here("2024", last_tues))

# Get the Data
tuesdata <- tidytuesdayR::tt_load(last_tues)
states <- map_data("state")

# Tidy data
glimpse(tuesdata$eclipse_total_2024)
total_2024 <- tuesdata$eclipse_total_2024

# Transform data
total_2024 <- total_2024 %>%
  mutate(totality_duration = eclipse_4 - eclipse_3)

total_2024_summary <- total_2024 %>%
  group_by(state) %>%
  summarise(avg_totality = mean(totality_duration))

# Plot data
total_2024 %>%
  ggplot(aes(x = totality_duration)) +
  geom_histogram()

total_2024_summary %>%
  ggplot(aes(x = avg_totality, y = reorder(state, avg_totality))) +
  geom_col()

plot <- states %>%
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group)) +
  geom_point(
    aes(x = lon, y = lat, color = totality_duration), 
    data = total_2024,
    alpha = 0.5
  ) +
  geom_point(
    aes(x = lon, y = lat),
    data = total_2024 %>% filter(name == "Radar Base"),
    size = 3
  ) +
  coord_map("albers",  lat0 = 45.5, lat1 = 29.5) +
  scale_color_continuous(name = str_wrap("Totality Duration (seconds)", width = 20)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#fbfae4"),
    plot.background = element_rect(fill = "#fbfae4"),
    legend.background = element_rect(fill = "#fbfae4"),
    strip.background = element_rect(fill = "#fbfae4")
  )
plot

# Save final
plot_title <- "totality_duration"
ggsave(
  here("2024", last_tues, paste0(last_tues, "_", plot_title, ".png")), 
  plot
)
