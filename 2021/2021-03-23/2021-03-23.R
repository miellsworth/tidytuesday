# Load libraries
library(tidyverse)
library(here)

# Creating directory for draft plots
dir.create(here("drafts", "plt_2021_wk13"))

# Load data
unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')

# List of G20 countries
g20_list <- list(
 "Argentina", "Australia", "Brazil",  "Canada", "China", "Germany", "France",
 "India", "Indonesia",   "Italy", "Japan", "Mexico",   "Russia", "Saudi Arabia",
 "South Africa", "South Korea", "Turkey", "United Kingdom", "United States"
)

# Rank countries by participation in important votes

sort_important <- function(country_list){
  unvotes %>%
    filter(country %in% country_list) %>%
    left_join(issues, by = "rcid") %>%
    left_join(roll_calls, by = "rcid") %>%
    filter(importantvote == 1) %>%
    group_by(country, country_code, vote) %>%
    summarise(n = n()) %>%
    mutate(prop = 1 - (n / sum(n))) %>%
    filter(vote == "abstain") %>%
    ungroup() %>%
    mutate(ranking = rank(prop))
}

g20 <- sort_important(g20_list)

g20 %>% ggplot() +
  geom_col(aes(y = reorder(country, prop), x = prop*100), fill = "#3482D5") +
  geom_text(aes(y = reorder(country, prop), x = prop*100, label = country), hjust = 1.1, colour = "white", fontface = "bold") +
  labs(
    title = "United Nations General Assembly Voting",
    subtitle = "Participation amongst G20 countries in votes classified as important by the U.S. State Department\nreport 'Voting Practices in the United Nations'",
    caption = "Chart: @mb_ellsworth | Data: dataverse.harvard.edu"
  ) +
  ylab(element_blank()) +
  xlab("Participation in important votes (%)") +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0), breaks = c(seq(0, 100, 10))) +
  theme_void() +
  theme(
    plot.margin = margin(10, 20, 5, 20),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 28, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, margin = margin(b = 5)),
    plot.caption = element_text(size = 8),
    axis.text.y = element_blank(),
    axis.ticks.length = unit(3, "pt"),
    axis.ticks.x.bottom = element_line(size = 0.5),
    axis.text.x = element_text(size = 10, margin = margin(t = 2)),
    axis.title.x = element_text(size = 12, margin = margin(t = 5)),
    axis.line.x.bottom = element_line(colour = "black", size = 0.5),
    text = element_text(family = "Tahoma")
  ) +
  # ggsave(here("plots", "drafts", "plt_2021_wk13", paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")))
  ggsave(here("2021", "2021-03-23", "2021-03-23_unvotes.png"))

