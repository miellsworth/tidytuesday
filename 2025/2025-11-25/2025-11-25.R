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
last_tues <- "2025-11-25" # Date in YYYY-MM-DD format

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
spi_indicators <- tuesdata$spi_indicators

# Plot data
plot <- spi_indicators |>
  filter(country == "Canada") |>
  rename(
    overall = overall_score,
    use = data_use_score,
    services = data_services_score,
    products = data_products_score,
    sources = data_sources_score,
    infrastructure = data_infrastructure_score
  ) |>
  pivot_longer(
    cols = c(
      overall,
      use, 
      services, 
      products, 
      sources, 
      infrastructure
      ),
    names_to = "score_type",
    values_to = "score"
  ) |>
  ggplot(aes(year, score, col = score_type)) +
  geom_line() +
  labs(
    title = str_wrap("Canadian SPI indicators"),
    x = "",
    caption = "Chart: Michael Ellsworth | Data: Nicola Rennie's World Bank Statistical Performances Indicators",
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
plot_title <- "spi_indicators_canada" # Include informative title
ggsave(
  here(year(last_tues), last_tues, paste0(last_tues, "_", plot_title, ".png")), 
  plot
)
