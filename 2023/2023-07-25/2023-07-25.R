# Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytuesdayR)
library(here)
library(stringr)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2023-07-25"

# Creating directory for draft plots
plt_dir <- paste0("plt_", last_tues)
dir.create(here("plots", "drafts", plt_dir))

# Get the Data
# tuesdata <- tidytuesdayR::tt_load('2023-07-25')
# scurvy <- tuesdata$scurvy
scurvy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-25/scurvy.csv')

# Tidy data
scurvy_tidy <- scurvy %>%
  mutate(
    gum_rot = as.integer(str_sub(gum_rot_d6, 1, 1)),
    skin_sores = as.integer(str_sub(skin_sores_d6, 1, 1)),
    knee_weakness = as.integer(str_sub(weakness_of_the_knees_d6, 1, 1)),
    lassitude = as.integer(str_sub(lassitude_d6, 1, 1))
    )

# Transform data
scurvy_trans <- scurvy_tidy %>%
  pivot_longer(
    cols = c(gum_rot, skin_sores, knee_weakness, lassitude),
    names_to = "symptom_severity"
    ) %>%
  group_by(treatment, symptom_severity) %>%
  summarise(mean_severity = mean(value))

# Plot data
plot <- scurvy_trans %>%
  ggplot(aes(x = symptom_severity, y = mean_severity)) +
  geom_col() +
  facet_wrap(facets = vars(treatment), nrow = 2, ncol = 3)
plot

# Save draft
# ggsave(
#   here("plots", "drafts", "plt_2023-07-25", paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")),
#   plot
#   )

# Save final
plot_title <- "scurvy_symptoms"
ggsave(
  here("2023", "2023-07-25", paste0(last_tues, "_", plot_title, ".png")), 
  plot
  )
