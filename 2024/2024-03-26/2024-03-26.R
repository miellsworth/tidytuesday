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
last_tues <- "2024-03-26"

# View README
tt_output <- tt_load_gh(last_tues)
readme(tt_output)

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
    title = str_wrap("Connecticut is by far the most picked team to make the NCAA Men's March Madness tournament finals", width = 75),
    y = "Proportion of finals picks (%)",
    x = "",
    caption = "Chart: Michael Ellsworth | Data: Nishaan Amin's NCAA Men's March Madness Kaggle dataset",
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
