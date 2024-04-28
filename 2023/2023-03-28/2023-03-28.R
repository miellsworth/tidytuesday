# Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytuesdayR)
library(here)
library(lubridate)
library(htmltools)
library(svglite)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2023-03-28"

# View README
tt_output <- tt_load_gh(last_tues)
readme(tt_output)

# Creating directory for draft plots
# dir.create(here("drafts", "plt_2023_03_28"))

# Get the Data
tuesdata <- tidytuesdayR::tt_load(last_tues)

transitions <- tuesdata$transitions
timezones <- tuesdata$timezones
timezone_countries <- tuesdata$timezone_countries
countries <- tuesdata$countries

# Explore the data
transitions %>% distinct(zone) %>% nrow()
timezones %>% distinct(zone) %>% nrow()
timezone_countries %>% distinct(zone) %>% nrow()

# Tidy transitions data
zones <- timezones %>% distinct(zone) %>% pull()
df_transitions <- transitions %>%
  filter(zone %in% zones) %>%  # Remove zones in transitions that don't exist in other datasets
  mutate(
    begin = lubridate::as_datetime(begin),
    end = lubridate::as_datetime(end)
  ) %>%
  mutate(duration = end - begin) %>%
  mutate(time_type = if_else(dst == FALSE, "standard", "daylight"))

# List of countries where there were no timezone transitions
no_transitions <- df_transitions %>%
  group_by(zone) %>%
  summarise(transition_count = n()) %>%
  arrange(transition_count) %>%
  filter(transition_count <= 1) %>%
  pull(zone)

no_transition_country_codes <- timezone_countries %>%
  filter(zone %in% no_transitions) %>%
  pull(country_code)

no_transition_countries <- countries %>% 
  filter(country_code %in% no_transition_country_codes) %>%
  pull(place_name)

# Count number of transitions per time zone
df_transitions %>% 
  group_by(zone) %>%
  summarise(transition_count = n()) %>%
  left_join(timezone_countries) %>%
  group_by(country_code) %>%
  summarise(avg_transitions = mean(transition_count)) %>%
  arrange(desc(avg_transitions)) %>%
  left_join(countries)

# Plot countries with no time transitions
thismap = map_data("world")

# Set colors
thismap <- mutate(thismap, fill = ifelse(region %in% no_transition_countries, "red", "white"))

# Use scale_fiil_identity to set correct colors
plot <- ggplot(thismap, aes(long, lat, group=group, fill = fill)) + 
  geom_polygon(colour = "grey") + 
  scale_fill_identity() +
  theme_minimal() +
  annotate(geom = "text", x = 60, y = -40, label = "Papua New Guinea") +
  annotate(
    "segment", 
    x = 60, xend = 140, 
    y = -35, yend = -10,
    size = 1,
    colour = "black",
    arrow = arrow(length = unit(0.5, "cm"))) +
  labs(
    title = "Papau New Guinea is one of the only countries with no recorded timezone changes",
    caption = "Chart: @mb_ellsworth | Data: IANA tz database"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.background = element_rect(fill = "white")
  )
plot

# Save final
plot_title <- "papau_new_guinea"
ggsave(
  here("2023", last_tues, paste0(last_tues, "_", plot_title, ".png")), 
  plot,
  width = 8, 
  height = 6
)
