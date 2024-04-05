# Load libraries
library(tidytuesdayR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2024-03-12"

# View README
tt_output <- tt_load_gh(last_tues)
readme(tt_output)

# Creating directory for draft plots
plt_dir <- paste0("plt_", last_tues)
dir.create(here("plots", "drafts", plt_dir))

# Get the Data
tuesdata <- tidytuesdayR::tt_load(last_tues)

# Tidy data
glimpse(tuesdata$fiscal_sponsor_directory)
df <- tuesdata$fiscal_sponsor_directory

# Transform data
df_buckets <- df %>%
  replace_na(list(n_sponsored = 0)) %>%
  mutate(
    bucket = cut(
      n_sponsored, 
      breaks = c(0, 6, 20, 50, 100, 200, Inf),
      labels = c("Up to 5 projects", "6-19 projects", "20-49 projects", "50-99 projects", "100-199 projects", "200+ projects"),
      right = FALSE
    )
  )

df_buckets_summary <- df_buckets %>% 
  group_by(bucket) %>%
  summarise(
    count = n(),
    bucket_proportion = n() / nrow(df)
  )
df_buckets_summary

# Plot data
plot <- df_buckets_summary %>%
  ggplot(aes(y = reorder(bucket, count), x = count)) +
  geom_col() +
  geom_text(
    aes(y = reorder(bucket, count), x = count, label = count), 
    hjust = 1.1, 
    colour = "white", 
    fontface = "bold"
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(
    title = "Breakdown of directory sponsors by number of projects served",
    subtitle = "Updated March 2024",
    caption = "Chart: @ellsworthh@data-folks.masto.host | Data: fiscalsponsordirectory.org",
    x = "",
    y = ""
  ) +
  theme_classic() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    text = element_text(family = "Tahoma")
  )
plot

# Save draft
# ggsave(here("plots", "drafts", paste0("plt_", last_tues), paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")))

# Save final
plot_title <- "num_sponsors"
ggsave(
  here("plots", paste0(last_tues, "_", plot_title, ".png")), 
  plot
)
