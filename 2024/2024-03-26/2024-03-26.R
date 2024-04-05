# Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytuesdayR)
library(here)
library(janitor)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2024-03-26"

# Creating directory for draft plots
plt_dir <- paste0("plt_", last_tues)
# dir.create(here("plots", "drafts", plt_dir))

# Get the Data
tuesdata <- tidytuesdayR::tt_load(last_tues)

# Tidy data
results <- tuesdata$`team-results` %>% 
  janitor::clean_names()
  
picks <- tuesdata$`public-picks` %>% 
  janitor::clean_names()

# Transform data
glimpse(results)
glimpse(picks)

# Plot data
plot <- picks %>%
  mutate(finals = as.numeric(sub("%", "", finals))/100) %>%
  arrange(desc(finals)) %>%
  head(n = 10) %>%
  ggplot(aes(x = reorder(team, finals), y = finals)) +
  geom_col() +
  geom_text(
    aes(label = paste0(as.character(round(finals * 100, 0)), '%')), 
    hjust = 1.1,
    color = "white"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = c(0, 0)
  ) +
  labs(
    title = "Connecticut is by far the most picked team to make the NCAA tournament finals",
    y = "Proportion of picks that have a team making the finals (%)",
    x = ""
  ) +
  theme_classic() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  coord_flip()
plot

# Save draft
# ggsave(
#   here("plots", "drafts", paste0("plt_", last_tues), paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")),
#   plot
# )

# Save final
plot_title <- "top_10_finals_picks"
ggsave(
  here("2024", last_tues, paste0(last_tues, "_", plot_title, ".png")), 
  plot
)
