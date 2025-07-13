# ------ CHART FUNCTION: CIRCLE SLIDE -----------------------------------------

#' Generate Circle Chart Slide
#' 
#' Creates a PowerPoint slide with a circle-based chart showing one value
#' per category (e.g., reunion class), styled with large circle markers and
#' optional units.
#'
#' @param data A data frame with numeric values and group labels.
#' @param instruction List of slide instruction configurations.
#' @param ppt_doc Optional `read_pptx()` object to append the slide to.
#'
#' @return Updated pptx object if `ppt_doc` is given; otherwise, `NULL`.
generate_circle_slide <- function(
    data,
    instruction,
    ppt_doc
) {
  # ------ EXTRACT instruction FIELDS -------------------------------------
  unit_label <- instruction$unit %||% ""
  category_var <- instruction$category$name
  order_var <- instruction$category$order
  metric_var <- instruction$metric %||% NULL
  group_info <- instruction$focal_group
  
  # ------ FILTER FOCAL GROUP DATA ----------------------------------------
  df <- data %>%
    filter(group == group_info$name)
  
  if (!is.null(group_info$subset)) {
    df <- df %>%
      filter(.data[["reunion_class"]] %in% group_info$subset$value)
  }
  
  # ------ EXTRACT CATEGORY ORDER -----------------------------------------
  ordered_levels <- if (!is.null(order_var)) {
    df %>%
      select(
        category = all_of(category_var),
        order = all_of(order_var)
      ) %>%
      distinct() %>%
      arrange(order) %>%
      pull(category)
  } else {
    df %>%
      pull(.data[[category_var]]) %>%
      unique() %>%
      sort()
  }
  
  # ------ AGGREGATE DATA -------------------------------------------------
  df_summary <- df %>%
    group_by(.data[[category_var]]) %>%
    summarise(
      value = if (!is.null(metric_var)) {
        mean(.data[[metric_var]], na.rm = TRUE)
      } else {
        n()
      },
      .groups = "drop"
    ) %>%
    filter(
      !is.na(.data[[category_var]]),
      is.finite(value)
    ) %>%
    mutate(
      !!category_var := factor(
        .data[[category_var]],
        levels = ordered_levels,
        ordered = TRUE
      )
    ) %>%
    arrange(.data[[category_var]]) %>%
    mutate(
      x = seq_len(n()),
      y = 0,
      label_value = if (unit_label == "%") {
        paste0(round(value), unit_label)
      } else {
        paste0(round(value, 1), unit_label)
      }
    )
  
  # ------ CREATE PLOT ----------------------------------------------------
  plot_obj <- ggplot(df_summary, aes(x = x, y = y)) +
    annotate(
      "segment",
      x = min(df_summary$x) - 0.6,
      xend = max(df_summary$x) + 0.6,
      y = 0,
      yend = 0,
      color = "white",
      linewidth = 1
    ) +
    geom_point(
      shape = 21,
      fill = "white",
      color = "#01a3a2",
      stroke = 2,
      size = 40
    ) +
    geom_text(
      aes(label = label_value),
      color = "#333232",
      fontface = "bold",
      size = 9,
      nudge_y = -0.05
    ) +
    geom_richtext(
      aes(
        label = .data[[category_var]],
        y = 1
      ),
      size = 9,
      fill = NA,
      label.color = NA,
      color = "white",
      fontface = "bold"
    ) +
    coord_cartesian(
      ylim = c(-3, 3),
      xlim = c(min(df_summary$x) - 0.1, max(df_summary$x) + 0.1)
    ) +
    theme_void()
  
  # ------ EXPORT TO SLIDE ------------------------------------------------
  if (!is.null(ppt_doc)) {
    ppt_doc <- export_plot_to_slide(
      ppt_doc = ppt_doc,
      plot_obj = plot_obj,
      title_text = instruction$title %||% " ",
      is_first = instruction$is_first
    )
    return(ppt_doc)
  }
  
  return(invisible(NULL))
}
