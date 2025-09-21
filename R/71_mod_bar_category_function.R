# ------ BAR CATEGORY SLIDE ----------------------------------------------------

#' Generate category bar chart and export to PowerPoint
#'
#' Creates a bar chart showing one metric split by categories (e.g. gender or 
#' income). Supports category grouping, axis formatting, optional trend lines, 
#' and export to PowerPoint.
#'
#' @param data A data frame of pre-processed values.
#' @param instruction A list of chart options (group info, metric, category, 
#'        labels, etc.).
#' @param ppt_doc Optional `read_pptx()` object for exporting the chart.
#'
#' @return Updated PowerPoint object (`ppt_doc`) and density plot (`plot_obj`).
generate_bar_category_slide <- function(
    data,
    instruction,
    ppt_doc
) {
  # ------ EARLY VALIDATION ----------------------------------------------------
  required_cols <- c(
    instruction$metric %||% character(),
    instruction$category$name %||% character(),
    instruction$category$order %||% character()
  )

  # Add focal group subset column if present and non-null
  fg_subset <- instruction$focal_group$subset
  if (!is.null(fg_subset) &&
      !is.null(fg_subset$title) &&
      !is.na(fg_subset$title)) {
    required_cols <- c(required_cols, fg_subset$title)
  }

  # Filter out any NULL or NA column names
  required_cols <- unique(na.omit(required_cols))
  required_cols <- required_cols[!is.null(required_cols)]

  # Check for missing columns
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    message("❌ Missing column(s): ", 
            paste(missing_cols, collapse = ", "),
            ". Slide skipped."
    )
    return(NULL)
  }

  # ------ SETUP ---------------------------------------------------------------
  # Extract key instruction fields and default unit
  unit_label <- instruction$unit %||% ""
  category_var <- instruction$category$name
  order_var <- instruction$category$order
  metric_var <- instruction$metric
  group_info <- instruction$focal_group

  # ------ FILTER GROUP --------------------------------------------------------
  # Keep rows for focal group and apply optional subset filter
  df <- data %>% filter(group == group_info$name)

  if (!is.null(group_info$subset)) {
    subset_col <- group_info$subset$title
    subset_val <- group_info$subset$value
    if (!is.null(subset_col) && !is.null(subset_val)) {
      df <- df %>% filter(.data[[subset_col]] %in% subset_val)
    }
  }
  # ------ ORDER CATEGORIES ----------------------------------------------------
  # Extract and sort category levels based on provided order column
  ordered_levels <- df %>%
    select(
      category = all_of(category_var),
      order = all_of(order_var)
    ) %>%
    filter(!is.na(category), !is.na(order)) %>%
    distinct() %>%
    arrange(order) %>%
    pull(category) %>% 
    na.omit() 
  # ------ AGGREGATE METRIC VALUES ---------------------------------------------
  # Compute average metric value per category and apply factor levels
  df <- df %>%
    filter(
      !is.na(.data[[category_var]]),
      is.finite(.data[[metric_var]])
    ) %>%
    group_by(.data[[category_var]]) %>%
    summarise(
      value = mean(.data[[metric_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      !!category_var := factor(
        .data[[category_var]],
        levels = ordered_levels,
        ordered = TRUE
      )
    ) %>%
    arrange(.data[[category_var]]) %>%
    mutate(x_center = seq_len(n())) %>% 
    na.omit() 

  # ------ Y AXIS MAX ----------------------------------------------------------
  # Compute y-axis maximum for consistent scale
  y_max <- ceiling(max(df$value, na.rm = TRUE))
  # ------ HANDLE STYLED CATEGORY VAR ------------------------------------------
  # Use styled HTML version if it exists (e.g., reunion_class_html)
  if (category_var == "reunion_class") {
    df[[category_var]] <- style_ordinal_suffix(df[[category_var]])
    
    # Also apply ordered factor using styled levels
    styled_levels <- style_ordinal_suffix(ordered_levels)
    
    df[[category_var]] <- factor(
      df[[category_var]],
      levels = styled_levels,
      ordered = TRUE
    )
  }

  # ------ BUILD PLOT ----------------------------------------------------------
  # Construct ggplot2 bar chart with value labels and formatting
  plot_obj <- ggplot(df, aes(x = .data[[category_var]], y = value)) +
    geom_col(
      fill = "#70e2ff",
      width = 0.5
    ) +
    geom_text(
      data = df[df$value > 0, ],   # only rows with value > 0
      aes(
        label = if (unit_label == "") {
          sprintf("%.1f", value)
        } else {
          paste0(sprintf("%.0f", value), unit_label)
        },
        y = value / 2
      ),
      color = "black",
      size = 6.5,
      fontface = "bold"
    ) +
    scale_y_continuous(
      limits = c(0, y_max),
      breaks = if (unit_label == "%") {
        seq(0, y_max, 10)
      } else {
        waiver()
      },
      labels = function(x) paste0(x, unit_label),
      expand = c(0, 0)
    ) +
    labs(
      x = instruction$x_title %||% category_var,
      y = instruction$y_title %||% metric_var,
      title = " "
    ) +
    global_theme() +
    theme(
      axis.text.x = element_markdown(),
      plot.title = element_text(
        color = "white", face = "bold", size = 26, hjust = 0
      ),
      plot.margin = margin(30, 40, 30, 40),
      legend.position = "none"
    )

  # ------ TREND LINE (OPTIONAL) -----------------------------------------------
  # Add diagonal segment if trend_line flag is TRUE and >=2 points exist
  if (isTRUE(instruction$trend_line) && nrow(df) >= 2) {
    plot_obj <- plot_obj +
      annotate(
        "segment",
        x = df$x_center[1],
        xend = df$x_center[nrow(df)],
        y = df$value[1],
        yend = df$value[nrow(df)],
        color = "#f9f871",
        linewidth = 2
      )
  }

  # ------ EXPORT TO SLIDE -----------------------------------------------------
  # Add plot to PowerPoint if ppt_doc is provided
  if (!is.null(ppt_doc)) {
    ppt_doc <- export_plot_to_slide(
      ppt_doc = ppt_doc,
      plot_obj = plot_obj,
      title_text = instruction$title %||% " ",
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
