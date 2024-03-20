# Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytuesdayR)
library(here)
library(stringr)
library(lubridate)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2024-01-09"

# Creating directory for draft plots
plt_dir <- paste0("plt_", last_tues)
# dir.create(here("plots", "drafts", plt_dir))

# Get the Data
tuesdata <- tidytuesdayR::tt_load(last_tues)

# Tidy data
can_births <- tuesdata$canada_births_1991_2022
nhl_births <- tuesdata$nhl_player_births

# Transform data
can_monthly_births <- can_births %>% 
  group_by(month) %>%
  summarise(total_monthly_births = sum(births)) %>%
  mutate(monthly_proportion = total_monthly_births/sum(total_monthly_births)) %>%
  mutate(sample = "gen_pop")

nhl_monthly_births <- nhl_births %>% count(birth_month) %>%
  mutate(monthly_proportion = n/sum(n)) %>%
  rename(
    total_monthly_births = n,
    month = birth_month
  ) %>%
  mutate(sample = "nhl")

monthly_births <- rbind(can_monthly_births, nhl_monthly_births) %>%
  mutate(month = as.factor(month))

monthly_birth_diffs <- monthly_births %>% 
  pivot_wider(
    id_cols = c(month),
    values_from = c(total_monthly_births, monthly_proportion),
    names_from = sample
  ) %>%
  mutate(prop_diff = monthly_proportion_nhl - monthly_proportion_gen_pop) %>%
  mutate(positive = if_else(prop_diff >= 0, TRUE, FALSE))
monthly_birth_diffs

# Plot data
plot <- nhl_monthly_births %>%
  ggplot(aes(x = month, y = monthly_proportion)) +
  geom_col() +
  geom_line(aes(x = month, y = monthly_proportion), data = can_monthly_births)

plot

plot2 <- monthly_birth_diffs %>%
  ggplot(aes(x = month, y = prop_diff, fill = positive)) +
  geom_col()

plot2

# Save draft
ggsave(
  here("plots", "drafts", paste0("plt_", last_tues), paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")),
  plot
)

# Save final
plot_title <- ""
ggsave(
  here("plots", paste0(last_tues, "_", plot_title, ".png")), 
  plot
)