# Load libraries
library(tidytuesdayR)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(stringr)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2025-12-02" # Date in YYYY-MM-DD format

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
sechselaeuten <- tuesdata$sechselaeuten

# Plot data
glimpse(sechselaeuten)
plot <- sechselaeuten |>
  filter(year != 1923) |>
  ggplot(aes(x = year, y = duration)) +
  geom_point() + 
  geom_smooth(method = "lm")  +
  labs(
    title = str_wrap("Boeoeg appears to be exploding a little later each year at the Sechselaeuten spring festival"),
    x = "Festival Year",
    y = "Duration to explosion! (minutes)",
    caption = "Chart: Michael Ellsworth | Data: econmatt's Sechselaeuten dataset",
  ) +
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
plot_title <- "exploding_snowman" # Include informative title
ggsave(
  here(year(last_tues), last_tues, paste0(last_tues, "_", plot_title, ".png")), 
  plot
)
