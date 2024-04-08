# Load libraries
library(dplyr)
library(ggplot2)
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
dir.create(here("2024", last_tues))

# Get the Data
tuesdata <- tidytuesdayR::tt_load(last_tues)

# Tidy data
glimpse(tuesdata$eclipse_total_2024)
