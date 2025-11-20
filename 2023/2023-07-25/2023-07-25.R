# Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytuesdayR)
library(here)
library(stringr)
library(grid)

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

# Clean value names
scurvy_clean <- scurvy_trans %>%
  mutate(symptom_severity = case_when(
    symptom_severity == "gum_rot" ~ "Gum Rot",
    symptom_severity == "skin_sores" ~ "Skin Sores",
    symptom_severity == "knee_weakness" ~ "Knee Weakness",
    symptom_severity == "lassitude" ~ "Lassitude",
    TRUE ~ symptom_severity
    )
  ) %>%
  mutate(treatment = case_when(
    treatment == "cider" ~ "Cider",
    treatment == "citrus" ~ "Citrus",
    treatment == "dilute_sulfuric_acid" ~ "Dilute Sulfuric Acid",
    treatment == "purgative_mixture" ~ "Purgative Mixture",
    treatment == "sea_water" ~ "Sea Water",
    treatment == "vinegar" ~ "Vinegar",
    TRUE ~ treatment
  )
  )

# Plot data
plot <- scurvy_clean %>%
  ggplot(aes(x = str_wrap(symptom_severity, width = 8), y = mean_severity)) +
  geom_col() +
  facet_wrap(facets = vars(treatment), nrow = 2, ncol = 3) +
  xlab("Symptom Severity") +
  ylab("Mean Severity") +
  theme(
    panel.background = element_rect(fill = "#fbfae4"),
    plot.background = element_rect(fill = "#fbfae4"),
  )
plot

# Save draft
# ggsave(
#   here("2023", "2023-07-25", paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")),
#   width = 8, height = 6, dpi = 300
#   )

# Save final
plot_title <- "scurvy_symptoms"
ggsave(
  here("2023", "2023-07-25", paste0(last_tues, "_", plot_title, ".png")),
  width = 8, height = 6, dpi = 300,
  plot
  )
