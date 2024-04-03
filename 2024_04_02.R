# Load libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyr)
library(tidytuesdayR)
library(here)
library(janitor)

# Find the most recent Tuesday
tidytuesdayR::last_tuesday()
last_tues <- "2024-04-02"

# View README
tt_output <- tt_load_gh(last_tues)
readme(tt_output)

# Creating directory for draft plots
plt_dir <- paste0("plt_", last_tues)
# dir.create(here("plots", "drafts", plt_dir))

# Get the Data
tuesdata <- tidytuesdayR::tt_load(last_tues)

# Tidy data
df <- tuesdata$dubois_week10 %>%
  arrange(desc(Percentage)) %>%
  mutate(
    csum = rev(cumsum(rev(Percentage))),
    pos = Percentage/2 + lead(csum, 1),
    pos = if_else(is.na(pos), Percentage/2, pos)
  )

occupations <- df %>%
  arrange(desc(Percentage)) %>%
  pull(Occupation)

percentages <- df %>%
  arrange(desc(Percentage)) %>%
  pull(Percentage)

df3 <- data.frame(
  person = as.numeric(c(NA, NA, NA, NA, NA, NA)),
  variable = occupations,
  value = as.numeric(c(NA, NA, NA, NA, NA, NA)))

# Plot data
df %>% 
  ggplot() +
  geom_bar(
    data = df, 
    aes(x = "", y = Percentage, fill = reorder(Occupation, -Percentage)),
    stat = "identity", 
    width = 1, 
    show.legend = FALSE
  ) +
  geom_point(
    data = df3, 
    aes(x = person, y = value, color = variable), 
    size = 8
  ) +
  scale_fill_manual(
    values = c("#DF2948", "#FDB01B", "#767EA2", "#817369", "#ECC4B4", "#B6957C")
  ) +
  scale_color_manual(
    values = c("#DF2948", "#FDB01B", "#767EA2", "#817369", "#ECC4B4", "#B6957C")
  ) +  
  coord_polar("y", start = 90) +
  theme_void() +
  theme(
    legend.position = "left",
    legend.title = element_blank()
  ) +
  annotate("text", x = 1.5, y = df$pos, label = paste0(df$Percentage, "%"), size = 5)
           