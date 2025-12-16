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
last_tues <- "2025-12-09" # Date in YYYY-MM-DD format

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
qatarcars <- tuesdata$qatarcars
glimpse(qatarcars)

# Plot data
plot <- qatarcars |>
  filter(price < 400000) |>
  ggplot(aes(x = price, y = performance, col = mass)) +
  scale_x_continuous(labels = scales::number) +
  geom_point() +
  labs(
    title = str_wrap("Want an expensive car in Qatar? She's going to be fast! (And light)"),
    x = "Price (Qatari riyals)",
    y = "Performance (seconds to 100 km/h)",
    caption = "Chart: Michael Ellsworth | Data: Paul Musgrave's Qatar Cars",
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "#fbfae4", color = NA),
    plot.background = element_rect(fill = "#fbfae4", color = NA),
    legend.background = element_rect(fill = "#fbfae4", color = NA),
    legend.box.margin = margin(0, 10, 0, 0),
    legend.position = "right"
  )
plot

# Save draft plots
ggsave(here(drafts, paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")))

# Save final
plot_title <- "qatar_cars" # Include informative title
ggsave(
  here(year(last_tues), last_tues, paste0(last_tues, "_", plot_title, ".png")), 
  plot
)
