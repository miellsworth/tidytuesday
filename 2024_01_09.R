# Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytuesdayR)
library(here)
library(forcats)

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
  mutate(num_month = month) %>%
  mutate(month = month.abb[month])

monthly_birth_diffs <- monthly_births %>% 
  pivot_wider(
    id_cols = c(month, num_month),
    values_from = c(total_monthly_births, monthly_proportion),
    names_from = sample
  ) %>%
  mutate(prop_diff = monthly_proportion_nhl - monthly_proportion_gen_pop) %>%
  mutate(positive = if_else(prop_diff >= 0, TRUE, FALSE))
monthly_birth_diffs

# Plot data
plot <- monthly_birth_diffs %>%
  ggplot(aes(x = fct_inorder(month), y = monthly_proportion_gen_pop)) +
  geom_rect(
    aes(
      x = fct_inorder(month),
      xmin = num_month - 0.45,
      xmax = num_month + 0.45,
      ymin = monthly_proportion_gen_pop,
      ymax = monthly_proportion_gen_pop + prop_diff,
      fill = positive
    )
  ) +
  geom_col(
    aes(
      x = fct_inorder(month), 
      y = monthly_proportion_gen_pop,
      ),
    color = "black",
    size = 1,
    alpha = 0.2
  ) +
  scale_fill_manual(
    values = c("#FF6962", "#5BB300"),
    labels = c("Lower", "Higher"),
    name = "Relative Proportion of NHL birth months"
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, NA),
    expand = c(0, 0)
  ) +
  ylab("Proportion of births per month (%)") +
  xlab(element_blank()) +
  labs(
    title = "NHL players are typically born in the first 5 months of the year,",
    subtitle = "relative to the general population.") +
  theme_classic() +
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    text = element_text(size = 13),
    legend.position = "bottom"
  )

plot

plot2 <- monthly_birth_diffs %>%
  ggplot(aes(x = fct_inorder(month), y = prop_diff, fill = positive)) +
  geom_col() +
  scale_fill_manual(
    values = c("#FF6962", "#5BB300"),
    labels = c("Lower", "Higher"),
    name = "Relative proportion of NHL birth months"
  ) +
  scale_y_continuous(
    labels = scales::percent,
    expand = c(0, 0)
  ) +
  ylab("Difference in proportion (%) (NHL - General Population)") +
  xlab(element_blank()) +
  labs(title = "Proportion of NHL player births by month, relative to the general population.") +
  theme_classic() +
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    text = element_text(size = 13),
    legend.position = "bottom"
  )

plot2

# Save draft
# ggsave(
#   here("plots", "drafts", paste0("plt_", last_tues), paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")),
#   plot
# )

# Save final
plot_title <- "proportion_comparison"
ggsave(
  here("plots", paste0(last_tues, "_", plot_title, ".png")), 
  plot
)

plot_title <- "proportion_diff_comparison"
ggsave(
  here("plots", paste0(last_tues, "_", plot_title, ".png")), 
  plot2
)