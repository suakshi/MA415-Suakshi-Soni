# MA415 Midterm Project R Script
# Run this file in RStudio. Ensure the CSV and helper function file are in the same directory.

# Libraries
library(knitr)
library(tidyverse)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)

# Load data
strawberry <- read_csv("strawb_mar6.csv", col_names = TRUE, show_col_types = FALSE)
data <- strawberry

# Load custom functions
source("my_functions.R")

# [Insert full R analysis code here]
