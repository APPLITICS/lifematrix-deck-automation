# ------ HORIZONTAL BAR SLIDE --------------------------------------------------

#' Generate Horizontal Bar Chart Slide
#'
#' Creates a horizontal bar chart comparing average hours and optionally
#' subjective values across multiple activities. Supports exporting the
#' chart to PowerPoint using a provided `pptx` object.
#'
#' @param data Data frame with numeric values for hours and subjective scores.
#' @param instruction A list with plot settings.
#' @param ppt_doc Optional PowerPoint object
#'
#' @return Updated PowerPoint object (`ppt_doc`) and density plot (`plot_obj`).
generate_horizontal_bar_slide <- function(
    data,
    instruction,
    ppt_doc
) {
  
  # ------ EXTRACT & VALIDATE INSTRUCTION SETTINGS -----------------------------
  metric_ids <- instruction$metric %||% character()
  subj_ids <- instruction$subjective_value %||% character()
  x_titles <- instruction$x_title
  y_title <- instruction$y_title
  chart_title <- instruction$title %||% ""
  focal_group <- instruction$focal_group
  has_subj <- length(subj_ids) > 0
  
  # Collect all columns that need to exist
  all_metrics <- c(metric_ids, subj_ids)
  all_metrics <- all_metrics[!is.null(all_metrics) & !is.na(all_metrics)]
  
  subset_cols <- character()
  if (!is.null(focal_group$subset) &&
      !is.null(focal_group$subset$title) &&
      !is.na(focal_group$subset$title)) {
    subset_cols <- focal_group$subset$title
  }
  
  missing_cols <- setdiff(unique(c(all_metrics, subset_cols)), names(data))
  if (length(missing_cols) > 0) {
    message("❌ Missing column(s): ", 
            aste(missing_cols, collapse = ", "),
            ". Slide skipped."
    )
    return(NULL)
  }
  
  # ------ FILTER TO FOCAL GROUP IF NEEDED -------------------------------------
  if (!is.null(focal_group)) {
    group_filter <- data$group == focal_group$name
    if (!is.null(focal_group$subset)) {
      subset_col <- focal_group$subset$title
      subset_value <- focal_group$subset$value
      group_filter <- group_filter & data[[subset_col]] == subset_value
    }
    data <- data[group_filter, ]
  }
  
  # ------ ENSURE LABELS EXIST FOR ALL METRICS ---------------------------------
  all_ids <- unique(na.omit(c(metric_ids, subj_ids)))
  missing_vars <- setdiff(all_ids, variable_map$variable)
  if (length(missing_vars) > 0) {
    variable_map <- bind_rows(
      variable_map,
      tibble(variable = missing_vars, label = missing_vars)
    ) 
  }
  
  # ------ PREPARE DATA FUNCTION -----------------------------------------------
  prep_data <- function(ids, type_label) {
    data %>%
      select(all_of(ids)) %>%
      summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
      pivot_longer(
        cols = everything(),
        names_to = "variable",
        values_to = "value"
      ) %>%
      left_join(variable_map, by = "variable") %>%
      mutate(type = type_label) %>%
      select(label, value, type)
  }
  
  # ------ GENERATE DATA -------------------------------------------------------
  df_metric <- prep_data(metric_ids, x_titles[[1]])
  df_subj <- if (has_subj) prep_data(subj_ids, x_titles[[2]]) else NULL
  
  # ------ COMBINE & ORDER -----------------------------------------------------
  label_levels <- bind_rows(df_metric, df_subj) %>%
    arrange(value) %>%
    pull(label) %>%
    unique()
  
  df_combined <- bind_rows(df_metric, df_subj) %>%
    mutate(
      label = factor(label, levels = label_levels),
      type = factor(type, levels = x_titles)
    )
  # ------ VISUAL SETTINGS -----------------------------------------------------
  if (has_subj) {
    facet_layer <- facet_grid(
      . ~ type,
      scales = "free_x",
      space = "fixed",
      switch = "x"
    )
    plot_margin <- margin(20, 50, 10, 30)
    x_axis_title <- NULL
    axis_line_x_bottom <- element_line(color = "white", linewidth = 1)
    x_breaks_fun <- function(x) {
      rng <- range(x, na.rm = TRUE)
      if (max(rng) <= 3) {
        seq(0, ceiling(rng[2]), 1)
      } else {
        breaks_extended(n = 4)(x)
      }
    }
    x_labels_fun <- function(x) {
      if (max(x, na.rm = TRUE) <= 3) {
        as.character(x) 
        
      } else {
        label_number(accuracy = 1)(x)
      }
    }
  } else {
    facet_layer <- NULL
    plot_margin <- margin(20, 80, 10, 30)
    x_axis_title <- x_titles[[1]]
    axis_line_x_bottom <- element_line(color = "white", linewidth = 1)
    x_breaks_fun <- function(x) breaks_extended(n = 4)(x)
    x_labels_fun <- label_number(accuracy = 1)
  }
  
  # ------ BUILD PLOT ----------------------------------------------------------
  plot_obj <- ggplot(df_combined, aes(x = value, y = label)) +
    geom_col(fill = "#84d8f6", width = 0.5) +
    facet_layer +
    scale_x_continuous(
      breaks = x_breaks_fun,
      labels = x_labels_fun,
      expand = c(0, 0)
    ) +
    labs(x = x_axis_title, y = y_title) +
    theme_minimal(base_size = 16) +
    theme(
      panel.spacing.x = unit(4, "lines"),
      strip.text = element_text(face = "bold", color = "white", size = 18),
      strip.placement = "outside",
      strip.background = element_blank(),
      panel.background = element_rect(fill = "#005b7f", color = NA),
      plot.background = element_rect(fill = "#005b7f", color = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_line(color = "white"),
      panel.grid.minor.x = element_blank(),
      axis.line.x.bottom = axis_line_x_bottom,
      axis.text.x = element_text(color = "white", size = 14),
      axis.text.y = element_text(
        color = "white",
        face = "bold",
        size = 16
      ),
      axis.title.x = element_text(
        color = "white",
        face = "bold",
        size = 20,
        margin = margin(t = 20)
      ),
      axis.title.y = element_text(
        color = "white",
        face = "bold",
        size = 22,
        margin = margin(r = 20)
      ),
      plot.margin = plot_margin
    )
  
  # ------ EXPORT OR RETURN ----------------------------------------------------
  if (!is.null(ppt_doc)) {
    ppt_doc <- export_plot_to_slide(
      ppt_doc = ppt_doc,
      plot_obj = plot_obj,
      title_text = chart_title,
      is_first = instruction$is_first
    )
  }
  
  return(
    list(
      ppt_doc = ppt_doc,
      plot_obj = plot_obj
    )
  )
}
