library(dplyr)
library(tidyr)
library(stringr)
library(tidytuesdayR)

# get list of all #tidytuesday folders
all_folders <- tibble::tibble(
  folders = list.dirs(path = ".", recursive = TRUE)
)

# get list of all weeks
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

# find README file
tt_readme <- list.files(file.path(tt_week$year, tt_week$week, "/"),
                        pattern = "\\.md|\\.MD", full.names = TRUE
)
# read README file
readme_txt <- readLines(tt_readme, warn = FALSE)[1]
# extract title
readme_title <- str_extract_between(readme_txt, start = ">", end = "<") |>
  stringr::str_trim("both")

# File name for plots
tt_week <- all_weeks[1, ]
tt_imgs <- list.files(file.path(tt_week$year, tt_week$week),
                      pattern = ".png|.PNG|.jpg|.JPG|.jpeg|.JPEG", full.names = TRUE
)

library(stringr)

text <- "2021/2021-03-23/2021-03-23_unvotes.png"
result <- str_extract(text, "_(.+)\\.")

# The extracted text is in the first capture group
print(result)
str_remove(result, ".")
