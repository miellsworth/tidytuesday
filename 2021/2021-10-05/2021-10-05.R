# Load libraries
library(tidyverse)
library(tidytuesdayR)
library(here)
library(sf)

# Creating directory for draft plots
# dir.create(here("plots", "drafts", "plt_2021_wk41"))

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load(2021, week = 41)

nurses <- tuesdata$nurses

# Tidy data
nurses_tidy_2020 <- nurses %>%
  janitor::clean_names() %>%
  filter(year == 2020)

# Download the Hexagones boundaries at geojson format here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.

spdf <- sf::st_read("data/us_states_hexgrid.geojson") %>% 
  mutate(google_name = gsub(" \\(United States\\)", "", google_name)) %>%
  left_join(. , nurses_tidy_2020, by=c("google_name"="state")) %>%
  mutate(bin = cut(hourly_wage_median, breaks=c(seq(25, 50, 5), Inf), labels=c("25-30", "30-35", "35-40", "40-45", "45-50", "50+"), include.lowest = TRUE))

library(viridis)
my_palette <- rev(magma(8))[c(2:7)]

ggplot(spdf) +
  geom_sf(aes(fill=bin), color="white") +
  geom_sf_text(aes(label = iso3166_2), color="white", size = 5) +
  coord_sf(crs = 3857, datum = NA) +
  theme_void() +
  scale_fill_manual(
    values=my_palette,
    name="Median Hourly Wage 2020",
    guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1)
  ) +
  ggtitle( "A map of Nurse salaries, state by state" ) +
  theme(
    legend.position = c(0.5, 0.9),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  ) +
  # ggsave(here("plots", "drafts", "plt_2021_wk41", paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")))
  ggsave(here("2021", "2021-10-05", "2021-10-05_median_salary.png"))
