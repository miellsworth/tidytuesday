# Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytuesdayR)
library(here)
library(forcats)
library(ggbeeswarm)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2024-01-23"

# Creating directory for draft plots
plt_dir <- paste0("plt_", last_tues)
# dir.create(here("plots", "drafts", plt_dir))

# Get the Data
tuesdata <- tidytuesdayR::tt_load(last_tues)

# Tidy data
df <- tuesdata$english_education

# Transform data
glimpse(df)

# Plot data
df %>%
  filter(size_flag %in% c("Small Towns", "Medium Towns", "Large Towns")) %>%
  ggplot(aes(x = size_flag, y = education_score)) +
  geom_violin(
    alpha = 0.1, 
    color = NA,
    fill = "black",
  ) +
  ggbeeswarm::geom_quasirandom(
    shape = 21, 
    # size = 2, 
    dodge.width = 0.75, 
    alpha = 0.25,
    fill = "black"
  ) +
  stat_summary(fun.y = mean, geom = "crossbar", width = 0.8, size = 0.4) +
  theme_classic() + 
  xlab("") +
  ylab("Educational attainment index score") +
  theme(
    legend.position = "none",
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  ) +
  coord_flip()

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