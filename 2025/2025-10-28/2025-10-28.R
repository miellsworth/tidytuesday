# Load libraries
library(tidytuesdayR)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(stringr)
library(patchwork)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2025-10-28" # Date in YYYY-MM-DD format

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
prizes <- tuesdata$prizes

# Plot data
probably_white <- c(
  "American British",
  "Anglo Norwegian",
  "Belarusian",
  "British", 
  "British Australian",
  "British Irish Australian",
  "English", 
  "English Canadian",
  "French British",
  "German Australian British",
  "Irish",
  "Irish Canadian",
  "Irish British Australian",
  "Jewish",
  "Jewish American",
  "Jewish Australian",
  "Jewish British",
  "Northern Irish",
  "Scottish", 
  "Slavic Australian",
  "Ukrainian British",
  "Welsh", 
  "Welsh British", 
  "White American", 
  "White American British",
  "White American Canadian",
  "White Australian",
  "White Australian American",
  "White Canadian", 
  "White New Zealander",
  "White South African",
  "White South African British"
)

prizes_categorized <- prizes |>
  mutate(ethnicity_simplified = case_when(ethnicity %in% probably_white ~ "White", .default = "Non-White"))

plot1 <- prizes_categorized |>
  filter(ethnicity_simplified == "White") |>
  ggplot(aes(x = prize_genre)) +
  geom_bar(aes(y = (..count..) / sum(..count..)), position = "dodge", col = "black", fill = "white") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.75), expand = expansion(mult = c(0, 0)))  +
  coord_flip() +
  labs(
    title = str_wrap("Compared with white authors, ethnically diverse authors' path to literary prizes is through fiction."),
    subtitle = str_wrap("White"),
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "#fbfae4", color = NA),
    plot.background = element_rect(fill = "#fbfae4", color = NA),
    legend.background = element_rect(fill = "#fbfae4", color = NA),
    legend.box.margin = margin(0, 10, 0, 0),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

plot2 <- prizes_categorized |>
  filter(ethnicity_simplified == "Non-White") |>
  ggplot(aes(x = prize_genre)) +
  geom_bar(aes(y = (..count..) / sum(..count..)), position = "dodge", col = "white", fill = "black") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.75), expand = expansion(mult = c(0, 0)))  +
  coord_flip() +
  labs(
    subtitle = str_wrap("Ethnically Diverse"),
    y = "Percent of shortlist/winners by genre",
    caption = "Chart: Michael Ellsworth | Data: Post45 Data Collective's Selected British Literary Prizes",
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "#fbfae4", color = NA),
    plot.background = element_rect(fill = "#fbfae4", color = NA),
    legend.background = element_rect(fill = "#fbfae4", color = NA),
    legend.box.margin = margin(0, 10, 0, 0),
    axis.title.y = element_blank()
  )
plot1 / plot2

# Save draft plots
ggsave(here(drafts, paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")))

# Save final
plot_title <- "literary_prizes" # Include informative title
ggsave(
  here(year(last_tues), last_tues, paste0(last_tues, "_", plot_title, ".png")), 
  plot
)
