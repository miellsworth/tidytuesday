# Load libraries
library(tidytuesdayR)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2025-11-18" # Date in YYYY-MM-DD format

# View README
tt_output <- tt_load_gh(last_tues)
readme(tt_output)

# Create directories
## Main
dir.create(here(year(last_tues), last_tues))

## Drafts
dir.create(here(year(last_tues), last_tues, "drafts"))

# Get the Data
tuesdata <- tidytuesdayR::tt_load(last_tues)

# Tidy data
holmes <- tuesdata$holmes
glimpse(holmes)
df_holmes <- holmes |>
  mutate(length = str_length(text)) |>
  group_by(book) |>
  summarise(avg_sentence_length = mean(length, na.rm = TRUE))

# Plot data
plot <- df_holmes |>
  ggplot(aes(avg_sentence_length)) +
  geom_histogram() +
  labs(
    title = str_wrap("Sherlock Holmes Books have a fairly consistent sentence length."),
    x = "Average Sentence Length",
    y = "Count",
    caption = "Chart: Michael Ellsworth | Data: Emil Hvitfeldt's sherlock R package",
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "#fbfae4", color = NA),
    plot.background = element_rect(fill = "#fbfae4", color = NA),
    legend.background = element_rect(fill = "#fbfae4", color = NA)
  )
plot

# Save draft plots
ggsave(here(drafts, paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")))

# Save final
plot_title <- "holmes" # Include informative title
ggsave(
  here(year(last_tues), last_tues, paste0(last_tues, "_", plot_title, ".png")), 
  plot
)
