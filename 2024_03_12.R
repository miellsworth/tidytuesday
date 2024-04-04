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
  ggplot(aes(x = bucket, y = count)) +
  geom_bar(stat = "identity")
plot

# Save draft
ggsave(here("plots", "drafts", paste0("plt_", last_tues), paste0(format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".png")))

# Save final
plot_title <- ""
ggsave(
  here("plots", paste0(last_tues, "_", plot_title, ".png")), 
  plot
)