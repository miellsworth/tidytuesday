# Load libraries
library(tidyverse)
library(tidytuesdayR)
library(here)

# Creating directory for draft plots
dir.create(here("plots", "drafts", "plt_2023_02_07"))

# Get the Data

tuesdata <- tidytuesdayR::tt_load('2023-02-07')

stock_prices <- tuesdata$big_tech_stock_prices
companies <- tuesdata$big_tech_companies

# Tidy data

