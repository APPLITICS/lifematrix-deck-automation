# ------ MODULE REGISTRY LIST --------------------------------------------------

#' Module Registry
#'
#' Provides a central mapping between slide generation functions and their
#' corresponding Shiny UI/server modules. Each entry contains:
#' 
#' - `ui`: A function returning the module's UI, with arguments for namespace
#'   and column lists.
#' - `server`: A function binding the module's server logic to the pipeline data.
#' - `id`: A short string identifier for the module, used internally.
#'
#' @format A named list of slide function mappings, where each element is itself
#'   a list containing `ui`, `server`, and `id`.
#' @examples
#' # Access the density module UI
#' module_registry$generate_density_slide$ui("dens")
#'
#' # Access the tile module server
#' module_registry$generate_tile_slide$server("tile")
module_registry <- list(
  # ------ DENSITY SLIDE -------------------------------------------------------
  generate_density_slide = list(
    ui = function(id) mod_density_ui(
      id,
      unique_groups,
      all_columns,
      numerical_columns
    ),
    server = function(id) mod_density_server(id, pipeline_data),
    id = "dens"
  ),
  
  # ------ CIRCLE SLIDE --------------------------------------------------------
  generate_circle_slide = list(
    ui = function(id) mod_circle_ui(
      id,
      unique_groups,
      all_columns,
      numerical_columns
    ),
    server = function(id) mod_circle_server(id, pipeline_data),
    id = "circle"
  ),
  
  # ------ TILE SLIDE ----------------------------------------------------------
  generate_tile_slide = list(
    ui = function(id) mod_tile_ui(
      id,
      unique_groups,
      all_columns,
      numerical_columns
    ),
    server = function(id) mod_tile_server(id, pipeline_data),
    id = "tile"
  ),
  
  # ------ BAR METRIC SLIDE ----------------------------------------------------
  generate_bar_metric_slide = list(
    ui = function(id) mod_bar_metric_ui(
      id,
      unique_groups,
      all_columns,
      numerical_columns
    ),
    server = function(id) mod_bar_metric_server(id, pipeline_data),
    id = "bar_metric"
  ),
  
  # ------ BAR CATEGORY SLIDE --------------------------------------------------
  generate_bar_category_slide = list(
    ui = function(id) mod_bar_category_ui(
      id,
      unique_groups,
      all_columns,
      numerical_columns
    ),
    server = function(id) mod_bar_category_server(id, pipeline_data),
    id = "bar_category"
  ),
  
  # ------ DONUT SLIDE ---------------------------------------------------------
  generate_donut_slide = list(
    ui = function(id) mod_donut_ui(
      id,
      unique_groups,
      all_columns
    ),
    server = function(id) mod_donut_server(id, pipeline_data),
    id = "donut"
  ),
  
  # ------ LINE SLIDE ----------------------------------------------------------
  generate_line_slide = list(
    ui = function(id) mod_line_ui(
      id,
      unique_groups,
      all_columns,
      numerical_columns
    ),
    server = function(id) mod_line_server(id, pipeline_data),
    id = "line"
  ),
  
  # ------ STACKED VERTICAL SLIDE ----------------------------------------------
  generate_stacked_vertical_slide = list(
    ui = function(id) mod_stacked_vertical_ui(
      id,
      unique_groups,
      all_columns,
      numerical_columns
    ),
    server = function(id) mod_stacked_vertical_server(id, pipeline_data),
    id = "stacked_v"
  ),
  
  # ------ STACKED HORIZONTAL SLIDE --------------------------------------------
  generate_stacked_horizontal_slide = list(
    ui = function(id) mod_stacked_horizontal_ui(
      id,
      unique_groups,
      all_columns,
      numerical_columns
    ),
    server = function(id) mod_stacked_horizontal_server(id, pipeline_data),
    id = "stacked_h"
  ),
  
  # ------ HORIZONTAL BAR SLIDE ------------------------------------------------
  generate_horizontal_bar_slide = list(
    ui = function(id) mod_horizontal_bar_ui(
      id,
      unique_groups,
      all_columns,
      numerical_columns
    ),
    server = function(id) mod_horizontal_bar_server(id, pipeline_data),
    id = "hbar"
  ),
  
  # ------ SCATTER SLIDE -------------------------------------------------------
  generate_scatter_slide = list(
    ui = function(id) mod_scatter_ui(
      id,
      unique_groups,
      all_columns,
      numerical_columns
    ),
    server = function(id) mod_scatter_server(id, pipeline_data),
    id = "scatter"
  )
)
