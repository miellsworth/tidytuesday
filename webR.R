library(dplyr)
library(tidyr)
library(stringr)
library(tidytuesdayR)
library(attachment)

# get list of all #tidytuesday folders
all_folders <- tibble::tibble(
  folders = list.dirs(path = ".", recursive = TRUE)
)

# Get list of all weeks
# NOTE: This will only work if there are no sub-directories in the week directory
all_weeks <- all_folders |>
  mutate(folders = str_remove(folders, "./")) |>
  separate_wider_delim(
    folders,
    delim = "/",
    names = c("year", "week"),
    too_few = "align_start",
    too_many = "drop"
  ) |>
  filter(year %in% c(2020, 2021, 2022, 2023, 2024)) |>
  drop_na(week) |>
  mutate(
    title = NA_character_,
    pkgs = NA_character_,
    code_fpath = NA_character_,
    img_fpath = NA_character_,
    d = NA_real_
  )

# Function to help extract plot names from a README
str_extract_between <- function(x, start, end) {
  pattern <- paste0("(?<=", start, ")(.*?)(?=", end, ")")
  return(stringr::str_extract(x, pattern = pattern))
}

# List of plot titles, plot paths from each Tidy Tuesday week
titles <- c()
plot_paths <- c()
package_list <- c()
for (i in 1:nrow(all_weeks)) {
  row <- all_weeks[i, ]
  readme_path <- list.files(file.path(row$year, row$week),
                          pattern = "\\.md|\\.MD", full.names = TRUE)
  if (length(readme_path) == 0) {
    stop(paste0("Include a README file for ", row$week))
  }
  readme_txt <- readLines(readme_path, warn = FALSE)[1]
  readme_title <- str_extract_between(readme_txt, start = ">", end = "<") |>
    stringr::str_trim("both")
  titles <- append(titles, readme_title)
  
  #TODO: How do I handle multiple plot paths per week
  plot_path <- list.files(file.path(row$year, row$week),
                          pattern = ".png|.PNG|.jpg|.JPG|.jpeg|.JPEG", full.names = TRUE)
  if (length(plot_path) == 0){
    plot_paths <- append(plot_paths, NA)
  }
  plot_paths <- append(plot_paths, plot_path)
  
  file <- list.files(file.path(row$year, row$week, "/"),
                        pattern = ".R", full.names = TRUE)[1]
  packages <- attachment::att_from_rscript(file) |>
    stringr::str_flatten_comma()
  package_list <- append(package_list, list(packages))
}
