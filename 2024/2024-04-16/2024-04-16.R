# Load libraries
library(dplyr)
library(ggplot2)
library(tidytuesdayR)
library(janitor)
library(here)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2024-04-16"

# View README
tt_output <- tt_load_gh(last_tues)
readme(tt_output)

# Create directory
# dir.create(here("2024", last_tues))

# Get the Data
tuesdata <- tidytuesdayR::tt_load(last_tues)
revdeps <- tuesdata$shiny_revdeps
package_details <- tuesdata$package_details %>% janitor::clean_names()

# View data
glimpse(revdeps)
glimpse(package_details)

revdeps %>% count(dependency_type)

revdeps %>% filter(child == "AFheritability")

shiny_children <- revdeps %>% 
  filter(parent == "shiny") %>%
  select(child) %>%
  pull()

revdeps %>% filter(child %in% shiny_children) %>% filter(child == "AFheritability")
