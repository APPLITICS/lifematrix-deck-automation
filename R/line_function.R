#' Generate Line Chart Slide
#'
#' Creates a line chart for one or more metrics across categories, filtered
#' by focal group. Optionally adds it to a PowerPoint slide.
#'
#' @param data Data frame with metrics and grouping variables.
#' @param instruction List with chart settings: metrics, category, title, axis labels, etc.
#' @param ppt_doc Optional PowerPoint object to append the slide to.
#'
#' @return A string with newline characters inserted for line breaks.
generate_line_slide <- function(
    data,
    instruction,
    ppt_doc = NULL
) {
  # ------ HELPER: NICE Y-AXIS STEP FUNCTION -------------------------------
  # Determines a clean interval for y-axis breaks based on min and max
  get_y_step <- function(y_min, y_max) {
    target_lines <- 5
    raw_step <- (y_max - y_min) / (target_lines - 1)
    nice_steps <- c(0.01, 0.02, 0.05, 0.1, 0.2, 0.25, 0.5,
                    1, 2, 5, 10, 20, 25, 50, 100)
    step <- nice_steps[which.min(abs(nice_steps - raw_step))]
    return(step)
  }
  
  # ------ EXTRACT SETTINGS -----------------------------------------------
  # Extracts all relevant parameters from the instruction list
  # ------ EXTRACT SETTINGS ------------------------------------------------
  focal_group   <- instruction$focal_group
  focal_name    <- focal_group$name
  fg_subset_col <- focal_group$subset$title %||% NULL
  fg_subset_val <- focal_group$subset$value %||% NULL
  category_var  <- instruction$category$name
  category_order <- instruction$category$order %||% NULL
  metrics       <- instruction$metric
  chart_title   <- instruction$title %||% ""
  y_axis_title  <- instruction$y_title %||% NULL
  unit          <- instruction$unit %||% NULL
  
  # ------ EARLY VALIDATION ------------------------------------------------
  required_cols <- unique(c(
    metrics %||% character(),
    category_var,
    category_order,
    fg_subset_col
  ))
  
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    message("❌ Missing column(s): ", paste(missing_cols, collapse = ", "), ". Slide skipped.")
    return(NULL)
  }
  
  
  # ------ FILTER FOCAL GROUP DATA ----------------------------------------
  # Filters dataset to keep only focal group and optional subset
  data_focal <- data %>%
    filter(group == focal_name)
  
  if (!is.null(fg_subset_col) &&
      !is.null(fg_subset_val) &&
      fg_subset_col %in% names(data_focal)) {
    data_focal <- data_focal %>%
      filter(.data[[fg_subset_col]] == fg_subset_val)
  }
  # ------ AGGREGATE TO MEAN BY CATEGORY ----------------------------------
  # Reshapes data and computes mean of each metric per category
  plot_data <- data_focal %>%
    select(all_of(c(category_var, category_order, metrics))) %>%
    pivot_longer(
      cols = all_of(metrics),
      names_to = "metric",
      values_to = "value"
    ) %>%
    filter(
      !is.na(.data[[category_var]]),
      !is.na(.data[[category_order]]),
      !is.na(value)
    ) %>%
    group_by(.data[[category_var]], .data[[category_order]], metric) %>%
    summarise(
      value = mean(value, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  # ------ HANDLE CATEGORY ORDERING ---------------------------------------
  # Ensures category appears in desired order in the plot
  if (!is.null(category_order) &&
      all(c(category_var, category_order) %in% names(plot_data))) {
    
    ordered_levels <- plot_data %>%
      distinct(.data[[category_var]], .data[[category_order]]) %>%
      arrange(.data[[category_order]]) %>%
      pull(.data[[category_var]])
    
    plot_data[[category_var]] <- factor(
      plot_data[[category_var]],
      levels = ordered_levels
    )
  }
  
  # ------ CHECK FOR EMPTY OR INVALID DATA --------------------------------
  # Avoids plotting if no usable data is available
  if (nrow(plot_data) == 0 ||
      all(is.na(plot_data[[category_var]])) ||
      all(is.na(plot_data$value))) {
    warning("No valid data available for selected group/subset/category.")
    return(NULL)
  }
  
  # ------ APPLY DISPLAY LABELS TO METRICS --------------------------------
  # Maps variable names to display labels if variable_map exists
  metric_order <- instruction$metric
  
  if (exists("variable_map") &&
      all(c("variable", "label") %in% names(variable_map))) {
    matching <- variable_map %>%
      filter(variable %in% metric_order)
    
    label_levels <- matching$label
    names(label_levels) <- matching$variable
    
    plot_data$metric <- factor(
      plot_data$metric,
      levels = metric_order,
      labels = label_levels[metric_order]
    )
  } else {
    plot_data$metric <- factor(plot_data$metric, levels = metric_order)
  }
  
  # ------ COLOR PALETTE --------------------------------------------------
  # Defines colors for each metric line
  palette_colors <- c("#ff65c1", "#ab6deb", "#02ccfe")
  legend_levels <- levels(plot_data$metric)
  color_values <- setNames(palette_colors[seq_along(legend_levels)], legend_levels)
  
  # ------ Y AXIS SCALING -------------------------------------------------
  # Sets limits and breaks for y-axis using nice step function
  y_min <- floor(min(plot_data$value, na.rm = TRUE))
  y_max <- ceiling(max(plot_data$value, na.rm = TRUE))
  y_step <- get_y_step(y_min, y_max)
  y_min_pad <- floor(y_min / y_step) * y_step
  y_max_pad <- ceiling(y_max / y_step) * y_step
  y_lim <- c(y_min_pad, y_max_pad)
  
  # ------ BUILD PLOT -----------------------------------------------------
  # Constructs the ggplot line chart with theming and labels
  plot_obj <- ggplot(plot_data) +
    geom_line(
      aes(
        x = .data[[category_var]],
        y = value,
        group = metric,
        color = metric
      ),
      linewidth = 2.5
    ) +
    scale_color_manual(values = color_values) +
    scale_y_continuous(
      limits = y_lim,
      breaks = seq(y_min_pad, y_max_pad, by = y_step),
      labels = function(x) {
        if (!is.null(unit)) paste0(x, " ", unit) else x
      }
    ) +
    labs(
      x = NULL,
      y = y_axis_title,
      color = NULL,
      title = ""
    ) +
    global_theme() +
    theme(
      plot.title = element_text(color = "white", face = "bold", size = 26, hjust = 0),
      plot.margin = margin(t = 30, r = 20, b = 15, l = 20),
      legend.position = "bottom",
      legend.text = element_text(color = "white", size = 18, face = "bold")
    )
  
  # ------ EXPORT TO POWERPOINT (OPTIONAL) -----------------------------------
  # Export the plot to a slide if ppt_doc is provided
  if (!is.null(ppt_doc)) {
    ppt_doc <- export_plot_to_slide(
      ppt_doc = ppt_doc,
      plot_obj = plot_obj,
      title_text = chart_title,
      is_first = instruction$is_first
    )
    return(ppt_doc)
  }
  
  return(invisible(NULL))
}
