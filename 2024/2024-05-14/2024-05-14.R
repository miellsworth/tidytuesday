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
coffee_survey_clean <- coffee_survey |>
  mutate(cups = case_when(
    is.na(cups) ~ "0",
    cups == "Less than 1" ~ "<1",
    cups == "1" ~ "1",
    cups == "2" ~ "2",
    cups == "3" ~ "3",
    cups == "4" ~ "4",
    cups == "More than 4" ~ ">4"
  )) |>
  mutate(cups = factor(cups, levels = c("0", "<1", "1", "2", "3", "4", ">4")))

# Plot data
plot <- coffee_survey_clean %>%
  ggplot(aes(x = cups)) +
  geom_bar() +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  labs(
    title = str_wrap("Viewers of the Great American Coffee Taste Test typically drink 2 cups of coffee per day", width = 75),
    y = "Number of survey responses",
    x = "",
    caption = "Chart: Michael Ellsworth | Data: Robert McKeon Aloe's The Great American Coffee Taste Test dataset",
  ) +
  theme_classic() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.background = element_rect(fill = "#fbfae4"),
    plot.background = element_rect(fill = "#fbfae4"),
    legend.background = element_rect(fill = "#fbfae4"),
  ) +
  coord_flip()
plot

# Save final
plot_title <- "cups_per_day"
ggsave(
  here("2024", last_tues, paste0(last_tues, "_", plot_title, ".png")), 
  plot
)
