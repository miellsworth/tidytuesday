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
  ) %>% 
  group_by(bucket) %>%
  summarise(
    count = n(),
    bucket_proportion = n() / nrow(df)
  )
df_buckets

# Plot data
plot <- df_buckets %>%
  ggplot(aes(y = reorder(bucket, count), x = count)) +
  geom_col() +
  geom_text(
    aes(y = reorder(bucket, count), x = count - 20, label = bucket), 
    hjust = 0, 
    colour = "white", 
    fontface = "bold"
  ) +
  labs(
    title = "",
    subtitle = "",
    caption = "Chart: @ellsworthh@data-folks.masto.host | Data: fiscalsponsordirectory.org"
  ) +
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
    axis.line.x.bottom = element_line(colour = "black"),
    text = element_text(family = "Tahoma")
  )
plot

# Save draft
ggsave(here("plots", "drafts", paste0("plt_", last_tues), paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")))

# Save final
plot_title <- ""
ggsave(
  here("plots", paste0(last_tues, "_", plot_title, ".png")), 
  plot
)