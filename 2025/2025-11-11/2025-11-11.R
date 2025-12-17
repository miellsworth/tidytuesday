# Load libraries
library(tidytuesdayR)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2025-11-11" # Date in YYYY-MM-DD format

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
tuberculosis <- tuesdata$who_tb_data
glimpse(tuberculosis)

# Plot data
plot <- tuberculosis |>
  group_by(year) |>
  summarise(c_cdr = mean(c_cdr, na.rm = TRUE) / 100) |>
  ggplot(aes(x = year, y = c_cdr)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = str_wrap("Tuberculosis case detection rate is slowly on the rise."),
    x = "",
    y = "Case Detection Rate (%)",
    caption = "Chart: Michael Ellsworth | Data: Sam Abbott's getTBinR package",
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
plot_title <- "tuberculosis" # Include informative title
ggsave(
  here(year(last_tues), last_tues, paste0(last_tues, "_", plot_title, ".png")), 
  plot
)
