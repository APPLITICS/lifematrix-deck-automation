# ------ LIBRARIES -------------------------------------------------------------
library(cowplot)
library(DT)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggrepel)
library(ggtext)
library(grid)
library(gridExtra)
library(officer)
library(patchwork)
library(readr)
library(rvg)
library(scales)
library(shinyWidgets)
library(stringr)
library(tibble)
library(tidyr)

# ------ PATHS $ SOURCING ------------------------------------------------------
source("R/helpers_data.R")

data_path <- "data/Corrupted_Simulated_Data.csv"
map_path <- "inputs/mapping_file.csv"
ppt_template_path <- "inputs/template.pptx"
ppt_output_path <- "outputs/generated_slides.pptx"
instr_output_dir <- "outputs/generated_instructions"

if (!dir.exists(instr_output_dir)) {
  dir.create(instr_output_dir, recursive = TRUE)
}

# ------ LOAD DATA -------------------------------------------------------------
pipeline_data <- read_csv(data_path, show_col_types = FALSE)
variable_map <- read_csv(map_path, show_col_types = FALSE)

# ------ NORMALIZE NAs ---------------------------------------------------------
na_equivalent <- c("inf", "")
pipeline_data <- normalize_na_tbl(
  data = pipeline_data,
  na_equivalent = na_equivalent
)

# ------ EXTRACT METADATA ------------------------------------------------------
ALL_COLS <- names(pipeline_data)
NUM_COLS <- names(pipeline_data)[
  vapply(pipeline_data, is.numeric, logical(1))
]
UNIQUE_GROUPS <- unique(pipeline_data$group[!is.na(pipeline_data$group)])

# ------ SLIDE FUNCTIONS -------------------------------------------------------
slide_functions <- c(
  "generate_density_slide",
  "generate_bar_metric_slide",
  "generate_bar_category_slide",
  "generate_circle_slide",
  "generate_donut_slide",
  "generate_line_slide",
  "generate_stacked_vertical_slide",
  "generate_stacked_horizontal_slide",
  "generate_horizontal_bar_slide",
  "generate_scatter_slide",
  "generate_tile_slide"
)
