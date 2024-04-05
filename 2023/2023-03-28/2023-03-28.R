# Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytuesdayR)
library(here)
library(lubridate)
library(scales)
library(ggrepel)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()

# Creating directory for draft plots
dir.create(here("plots", "drafts", "plt_2023_03_28"))

# Get the Data
tuesdata <- tidytuesdayR::tt_load('2023-03-28')

transitions <- tuesdata$transitions
timezones <- tuesdata$timezones
timezone_countries <- tuesdata$timezone_countries
countries <- tuesdata$countries

# Tidy transitions data
transitions$begin <- lubridate::as_datetime(transitions$begin)
transitions$end <- lubridate::as_datetime(transitions$end)
transitions$year <- lubridate::floor_date(transitions$begin, "year")
transitions <- transitions %>%
  filter(!is.na(transitions$year))

# Create a sequence of years based on min and max years in transitions
min_year <- min(transitions$year)
max_year <- max(transitions$year)
years <- tibble(year = seq(min_year, max_year, "years"))

# Expand dataframe
test_df <- expand_grid(years, zone = unique(transitions$zone)) %>%
  left_join(transitions, by = c("year", "zone")) %>%
  group_by(zone) %>%
  fill(offset, dst, abbreviation)
