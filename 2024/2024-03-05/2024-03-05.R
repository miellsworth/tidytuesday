# Load libraries
library(tidytuesdayR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(janitor)
library(lubridate)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2024-03-05"

# View README
tt_output <- tt_load_gh(last_tues)
readme(tt_output)

# Creating directory for draft plots
plt_dir <- paste0("plt_", last_tues)
# dir.create(here("drafts", plt_dir))

# Get the Data
tuesdata <- tidytuesdayR::tt_load(last_tues)

# Tidy data
glimpse(tuesdata$trashwheel)
df <- tuesdata$trashwheel

# Transform data
df_long <- df %>%
  janitor::clean_names() %>%
  select(-c(weight, volume, homes_powered)) %>%
  pivot_longer(
    cols = c(
      plastic_bottles, 
      polystyrene, 
      cigarette_butts, 
      glass_bottles,
      plastic_bags,
      wrappers,
      sports_balls
      ),
    names_to = "trash_type",
    values_to = "trash_count"
  ) %>%
  mutate(trash_count = replace_na(trash_count, 0)) %>%
  mutate(date = lubridate::mdy(date)) %>%
  arrange(date, trash_type) %>%
  group_by(trash_type) %>%
  mutate(cumulative_trash = cumsum(trash_count))

df_long
# Plot data
plot <- df_long %>%
  filter(! trash_type %in% c("cigarette_butts", "sports_balls", "glass_bottles")) %>%
  mutate(trash_type = case_match(
    trash_type,
    "plastic_bags" ~ "Plastic Bags",
    "plastic_bottles" ~ "Plastic Bottles",
    "polystyrene" ~ "Polystyrene",
    "wrappers" ~ "Wrappers"
    )
  ) %>%
  ggplot(aes(x = date, y = cumulative_trash, color = trash_type)) +
  geom_line(size = 2) +
  theme_classic() +
  labs(
    title = "The most commonly collected trash by Mr. Trash Wheel",
    x = "",
    y = "Cumulative Trash Count"
  ) +
  scale_color_brewer(name = "Type of Trash", palette = "BrBG") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    legend.position = "bottom"
  )
plot

# Save draft
# ggsave(here("plots", "drafts", paste0("plt_", last_tues), paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")))

# Save final
plot_title <- "cumulative_trash"
ggsave(
  here("2024", last_tues, paste0(last_tues, "_", plot_title, ".png")), 
  plot
)
