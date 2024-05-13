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
last_tues <- "2024-05-14"

# View README
tt_output <- tt_load_gh(last_tues)
readme(tt_output)

# Creating directory for draft plots
plt_dir <- paste0("plt_", last_tues)
# dir.create(here("plots", "drafts", plt_dir))

# Get the Data
coffee_survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv')
glimpse(coffee_survey)

# Tidy data


# Plot data
plot <- coffee_survey %>%
  ggplot(aes(x = cups)) +
  geom_bar()
plot

# Save final
plot_title <- "cups_per_day"
ggsave(
  here("2024", last_tues, paste0(last_tues, "_", plot_title, ".png")), 
  plot
)