# Load libraries
library(tidyverse)
library(tidytuesdayR)
library(here)
library(lubridate)
library(scales)
library(ggrepel)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()

# Creating directory for draft plots
dir.create(here("plots", "drafts", "plt_2023_02_07"))

# Get the Data
tuesdata <- tidytuesdayR::tt_load('2023-02-07')

stock_prices <- tuesdata$big_tech_stock_prices
companies <- tuesdata$big_tech_companies

# Top 5 companies by volume
top_5_volume <- stock_prices %>%
  mutate(date = lubridate::as_date(date)) %>%
  filter(date > "2019-12-31") %>%
  group_by(stock_symbol) %>%
  summarise(total_vol = sum(volume)) %>%
  arrange(desc(total_vol)) %>%
  head(5) %>%
  pull(stock_symbol)
  
# Tidy stock data
monthly_volume <- stock_prices %>%
  left_join(companies) %>%
  mutate(date = lubridate::as_date(date)) %>%
  filter(
    stock_symbol %in% top_5_volume,
    date > "2019-12-31"
    ) %>%
  mutate(month = floor_date(date, unit = "month")) %>%
  group_by(month, company) %>%
  summarise(avg_volume = mean(volume)) %>%
  ungroup()
  
# Plot data
monthly_volume %>%
  mutate(label = if_else(month == max(month), company, NA_character_)) %>%
  ggplot(aes(x = month, y = avg_volume, group = company, col = company)) +
  geom_line() +
  theme_void() +
  scale_y_continuous(labels = label_number(suffix = "MM", scale = 1e-6)) +
  ylab("Monthly Average Volume") +
  theme(
    plot.margin = margin(10, 20, 5, 20),
    panel.grid.minor.y = element_line(colour = "grey50", size = 0.1),
    panel.grid.major.y = element_line(colour = "grey50", size = 0.1),
    plot.title = element_text(size = 28, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, margin = margin(b = 5)),
    plot.caption = element_text(size = 8),
    axis.ticks.length = unit(3, "pt"),
    axis.ticks.x.bottom = element_line(size = 0.5),
    axis.text.x = element_text(size = 10, margin = margin(t = 2)),
    axis.text.y = element_text(size = 10, margin = margin(t = 2)),
    axis.title.x.bottom = element_blank(),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    axis.title.x = element_text(size = 12, margin = margin(t = 5)),
    axis.title.y = element_text(angle = 90, size = 12, margin = margin(r = 10)),
    axis.line.x.bottom = element_line(colour = "black", size = 0.5),
    text = element_text(family = "Tahoma"),
    legend.position="bottom",
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA)
  ) +
  ggsave(here("plots", "2023_02_07_monthly_vol.png"))
