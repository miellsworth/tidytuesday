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
last_tues <- "2025-11-04" # Date in YYYY-MM-DD format

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
flint_mdeq <- tuesdata$flint_mdeq |>
  mutate(source = "mdeq")
flint_vt <- tuesdata$flint_vt  |>
  mutate(source = "vt")

flint_combined <- flint_mdeq |>
  bind_rows(flint_vt)

# Plot data
plot_multi_histogram <- function(df, feature, label_column) {
  plt <- ggplot(df, aes(x = eval(parse(text = feature)), fill = eval(parse(text = label_column)))) +
    geom_histogram(alpha = 0.7, position = "identity", aes(y = after_stat(density)), color="black") +
    geom_density(alpha = 0.7) +
    geom_vline(aes(xintercept = mean(eval(parse(text = feature)))), color = "black", linetype = "dashed", linewidth = 1) +
    labs(x = feature, y = "Density")
  plt + guides(fill = guide_legend(title = label_column))
}

plot_multi_histogram(flint_combined, "lead", "source")  +
  labs(
    title = str_wrap("Lead sample distribution comparison - MDEQ and VT"),
    x = "Lead level (ppb)",
    caption = "Chart: Michael Ellsworth | Data: Loux and Gibson's Flint Michigan dataset",
  ) +
  theme_classic() +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme(
    panel.background = element_rect(fill = "#fbfae4", color = NA),
    plot.background = element_rect(fill = "#fbfae4", color = NA),
    legend.background = element_rect(fill = "#fbfae4", color = NA),
    legend.position = "right"
  )

# Save draft plots
ggsave(here(drafts, paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")))

# Save final
plot_title <- "" # Include informative title
ggsave(
  here(year(last_tues), last_tues, paste0(last_tues, "_", plot_title, ".png")), 
  plot
)