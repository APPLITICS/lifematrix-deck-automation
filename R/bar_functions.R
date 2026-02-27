# ------ BAR METRIC SLIDE (no placeholders) -----------------------------------

#' Generate Grouped Bar Chart and Export to PowerPoint
#'
#' Creates a grouped bar chart comparing a focal group to optional comparison
#' groups across metrics, with optional target lines. Supports unit displaying,
#' custom labels, and slide export via `officer`.
#'
#' @param data A data frame of pre-processed values.
#' @param instruction A list with plot settings.
#' @param ppt_doc Optional `read_pptx()` object to append the slide.
#'
#' @return Updated PowerPoint object if `ppt_doc` is provided, otherwise `NULL`.
generate_bar_metric_slide <- function(
    data,
    instruction,
    ppt_doc
) {
  # ------ EARLY VALIDATION ----------------------------------------------------
  bar_metrics <- instruction$bar_value %||% character()
  target_metrics <- instruction$target %||% character()
  show_target <- isTRUE(instruction$show_target)
  all_metrics <- unique(na.omit(c(bar_metrics, target_metrics)))
  unit_label <- instruction$unit %||% ""
  required_subset_cols <- character()
  
  # Add focal subset col if present
  fg_subset <- instruction$focal_group$subset
  if (!is.null(fg_subset) &&
      !is.null(fg_subset$title) &&
      !is.na(fg_subset$title)) {
    required_subset_cols <- c(required_subset_cols, fg_subset$title)
  }
  
  # Collect subset cols from comparisons and placeholders
  if (!is.null(instruction$comparison_groups) ||
      !is.null(instruction$placeholders)) {
    merged_list <- c(instruction$comparison_groups, instruction$placeholders)
    for (cg in merged_list) {
      if (!is.null(cg$subset) &&
          !is.null(cg$subset$title) &&
          !is.na(cg$subset$title)) {
        required_subset_cols <- c(required_subset_cols, cg$subset$title)
      }
    }
  }
  
  required_subset_cols <- unique(na.omit(required_subset_cols))
  
  # Missing column check
  missing_metrics <- setdiff(all_metrics, names(data))
  missing_subset_cols <- setdiff(required_subset_cols, names(data))
  all_missing <- unique(c(missing_metrics, missing_subset_cols))
  
  if (length(all_missing) > 0) {
    message(
      "❌ Missing column(s): ",
      paste(all_missing, collapse = ", "),
      ". Slide skipped."
    )
    return(NULL)
  }
  
  # ------ VARIABLE MAP --------------------------------------------------------
  vars_needed <- unique(na.omit(c(bar_metrics, target_metrics)))
  
  if (is.null(variable_map)) variable_map <- data.frame()
  if (!"variable" %in% names(variable_map)) variable_map$variable <- character(0)
  if (!"label" %in% names(variable_map)) variable_map$label <- character(0)
  
  variable_map$variable <- as.character(variable_map$variable)
  variable_map$label <- as.character(variable_map$label)
  
  existing_vars <- unique(variable_map$variable)
  missing_vars <- setdiff(vars_needed, existing_vars)
  if (length(missing_vars) > 0) {
    add_df <- data.frame(
      variable = missing_vars,
      label = missing_vars,
      stringsAsFactors = FALSE
    )
    variable_map <- rbind(variable_map, add_df)
  }
  
  bar_value_labels <- tibble(variable = bar_metrics) %>%
    left_join(variable_map, by = "variable")
  
  target_value_labels <- variable_map %>%
    filter(variable %in% target_metrics) %>%
    distinct(variable, label)
  
  # ------ GROUPING LOGIC ------------------------------------------------------
  is_simple_group <- function(instruction) {
    length(instruction$bar_value) == 1 &&
      (is.null(instruction$comparison_groups) ||
         all(sapply(
           instruction$comparison_groups,
           function(cg) is.null(cg$name) || is.na(cg$name)
         )))
  }
  
  bar_width <- if (is_simple_group(instruction)) 0.4 else 0.7
  dodge_width <- 0.8
  position_setting <- if (is_simple_group(instruction)) {
    position_identity()
  } else {
    position_dodge(width = dodge_width)
  }
  
  # ------ HELPER FUNCTIONS ----------------------------------------------------
  preprocess_group <- function(
    df_input,
    group_info,
    metric_list,
    placeholder = FALSE
  ) {
    df <- df_input %>% filter(group == group_info$name)
    
    if (!is.null(group_info$subset) &&
        !is.null(group_info$subset$value) &&
        !is.null(group_info$subset$title) &&
        (group_info$subset$title %in% names(df))) {
      df <- df %>%
        filter(.data[[group_info$subset$title]] == group_info$subset$value)
    }
    if (nrow(df) == 0) return(NULL)
    
    values <- df %>%
      summarise(across(all_of(metric_list), ~ mean(.x, na.rm = TRUE)))
    
    group_label <- group_info$name
    if (!is.null(group_info$subset) && !is.null(group_info$subset$value)) {
      group_label <- paste(group_label, group_info$subset$value)
    }
    
    tibble(
      group = group_label,
      metric = metric_list,
      value = as.numeric(values[1, ]),
      is_placeholder = placeholder
    )
  }
  
  get_x_centers <- function(df_plot, x_axis_var) {
    tmp_plot <- ggplot(
      df_plot,
      aes(
        x = .data[[x_axis_var]],
        y = .data$value,
        fill = .data$fill_group_show,
        group = interaction(group, metric)
      )
    ) +
      geom_col(width = bar_width, position = position_setting)
    
    bar_layer <- ggplot_build(tmp_plot)$data[[1]]
    df_plot %>%
      bind_cols(x_center = bar_layer$x) %>%
      select(
        group,
        metric,
        value,
        fill_group,
        fill_group_show,
        x_center
      )
  }
  
  # ------ PREPROCESS BARS -----------------------------------------------------
  # Collect focal, comparison, and placeholder bars.
  group_labels <- character(0)
  group_labels_display <- character(0)
  data_bars <- list()
  
  # Focal group
  if (!is.null(instruction$focal_group$name)) {
    fg_label <- instruction$focal_group$name
    if (!is.null(instruction$focal_group$subset) &&
        !is.null(instruction$focal_group$subset$value)) {
      fg_label <- paste(fg_label, instruction$focal_group$subset$value)
    }
    group_labels <- c(group_labels, fg_label)
    group_labels_display <- c(group_labels_display, fg_label)
    data_bars <- c(
      data_bars,
      list(
        preprocess_group(
          data,
          instruction$focal_group,
          instruction$bar_value,
          placeholder = FALSE
        )
      )
    )
  }
  
  # Comparison groups
  if (!is.null(instruction$comparison_groups)) {
    for (cg in instruction$comparison_groups) {
      if (!is.null(cg$name) && !is.na(cg$name)) {
        cg_label <- cg$name
        if (!is.null(cg$subset) && !is.null(cg$subset$value)) {
          cg_label <- paste(cg_label, cg$subset$value)
        }
        group_labels <- c(group_labels, cg_label)
        group_labels_display <- c(group_labels_display, cg_label)
        data_bars <- c(
          data_bars,
          list(
            preprocess_group(
              data,
              cg,
              instruction$bar_value,
              placeholder = FALSE
            )
          )
        )
      }
    }
  }
  
  # Placeholders
  if (!is.null(instruction$placeholders)) {
    for (ph in instruction$placeholders) {
      if (!is.null(ph$name) && !is.na(ph$name)) {
        ph_label <- ph$name
        if (!is.null(ph$subset) && !is.null(ph$subset$value)) {
          ph_label <- paste(ph_label, ph$subset$value)
        }
        group_labels <- c(group_labels, ph_label)
        data_bars <- c(
          data_bars,
          list(
            preprocess_group(
              data,
              ph,
              instruction$bar_value,
              placeholder = TRUE
            )
          )
        )
      }
    }
  }
  
  group_labels <- unique(group_labels)
  group_labels_display <- unique(group_labels_display)
  
  df_bars_all <- bind_rows(data_bars)
  if (is.null(df_bars_all) || nrow(df_bars_all) == 0) {
    return(invisible(NULL))
  }
  
  df_bars_all <- df_bars_all %>%
    left_join(bar_value_labels, by = c("metric" = "variable")) %>%
    rename(metric_label = label) %>%
    mutate(
      fill_group = group,
      fill_group_show = if_else(
        !is_placeholder,
        as.character(group),
        NA_character_
      )
    )
  # ------ TARGETS (COLLECT & MAP) ---------------------------------------------
  df_targets_all <- {
    if (length(target_metrics) == 0) {
      NULL
    } else {
      # Collect buckets for focal + comparison groups
      buckets <- list(
        preprocess_group(
          data,
          instruction$focal_group,
          target_metrics,
          placeholder = FALSE
        )
      )
      if (!is.null(instruction$comparison_groups)) {
        for (cg in instruction$comparison_groups) {
          if (!is.null(cg$name) && !is.na(cg$name)) {
            buckets[[length(buckets) + 1]] <- preprocess_group(
              data,
              cg,
              target_metrics,
              placeholder = FALSE
            )
          }
        }
      }
      
      # Drop empty buckets
      if (length(buckets) > 0L) {
        keep <- !vapply(
          buckets,
          function(x) is.null(x) || nrow(as.data.frame(x)) == 0L,
          logical(1)
        )
        buckets <- buckets[keep]
      }
      
      if (length(buckets) == 0L) {
        NULL
      } else {
        # Bind results and map to bar metrics
        out <- do.call(
          rbind,
          lapply(
            buckets,
            function(x) as.data.frame(x, stringsAsFactors = FALSE)
          )
        )
        if (is.null(out) || nrow(out) == 0L) {
          NULL
        } else {
          target_map <- data.frame(
            metric = instruction$bar_value,
            target = target_metrics,
            stringsAsFactors = FALSE
          )
          names(out)[names(out) == "metric"] <- "target"
          
          merged <- merge(
            out,
            target_map,
            by = "target",
            all.x = TRUE,
            sort = FALSE
          )
          merged <- merged[!is.na(merged$metric), , drop = FALSE]
          
          if (nrow(merged) == 0L) {
            NULL
          } else {
            merged$value <- as.integer(sprintf("%.0f", merged$value))
            merged[c("group", "metric", "value")]
          }
        }
      }
    }
  }
  
  # ------ AXIS SETUP ----------------------------------------------------------
  x_axis_var <- if (length(instruction$bar_value) == 1) {
    "group"
  } else {
    "metric_label"
  }
  
  x_axis_levels <- if (x_axis_var == "group") {
    group_labels
  } else {
    bar_value_labels$label
  }
  
  df_bars_all <- df_bars_all %>%
    mutate(value = as.integer(sprintf("%.0f", value)))
  
  y_candidates <- df_bars_all$value
  y_candidates <- c(y_candidates, df_targets_all$value)
  y_max <- ceiling((max(y_candidates, na.rm = TRUE)) / 10) * 10
  
  # Filter only display groups
  df_bars <- df_bars_all %>% filter(!is_placeholder)
  if (nrow(df_bars) == 0) return(invisible(NULL))
  
  df_bars[[x_axis_var]] <- factor(df_bars[[x_axis_var]], levels = x_axis_levels)
  df_bars$group <- factor(df_bars$group, levels = group_labels)
  df_bars$fill_group <- factor(df_bars$fill_group, levels = group_labels_display)
  df_bars$fill_group_show <- factor(
    df_bars$fill_group_show,
    levels = group_labels_display
  )
  
  # ------ PLACEHOLDER RENDERING -----------------------------------------------
  ph_groups <- setdiff(group_labels, group_labels_display)
  if (length(ph_groups) > 0) {
    ph_df <- expand.grid(
      group = ph_groups,
      metric = instruction$bar_value,
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        value = 0,
        is_placeholder = TRUE,
        metric_label = bar_value_labels$label[
          match(metric, bar_value_labels$variable)
        ],
        fill_group = group,
        fill_group_show = NA_character_
      )
    
    ph_df$group <- factor(ph_df$group, levels = group_labels)
    ph_df$fill_group <- factor(ph_df$fill_group, levels = group_labels_display)
    ph_df$fill_group_show <- factor(
      ph_df$fill_group_show,
      levels = group_labels_display
    )
    
    ph_df[[x_axis_var]] <- if (x_axis_var == "group") {
      factor(ph_df$group, levels = group_labels)
    } else {
      factor(ph_df$metric_label, levels = bar_value_labels$label)
    }
    
    df_bars_render <- bind_rows(df_bars, ph_df)
  } else {
    df_bars_render <- df_bars
  }
  
  df_bars_render[[x_axis_var]] <- factor(
    df_bars_render[[x_axis_var]],
    levels = x_axis_levels
  )
  
  # ------ LEGEND & COLORS -----------------------------------------------------
  non_zero_groups <- df_bars %>%
    filter(value > 0) %>%
    pull(fill_group_show) %>%
    unique()
  
  hide_legend_elements <- (length(non_zero_groups) <= 1 || x_axis_var == "group")
  legend_colors <- get_color_palette(group_labels_display)
  
  # ------ BUILD PLOT ----------------------------------------------------------
  breaks_x <- if (x_axis_var == "group") {
    group_labels_display
  } else {
    x_axis_levels
  }
  labels_x <- breaks_x
  
  plot_obj <- ggplot(
    df_bars_render,
    aes(
      x = .data[[x_axis_var]],
      y = value,
      fill = fill_group_show,
      group = interaction(group, metric)
    )
  ) +
    geom_col(width = bar_width, position = position_setting) +
    scale_y_continuous(
      expand = c(0, 0),
      labels = function(x) paste0(x, unit_label)
    ) +
    scale_x_discrete(
      drop = FALSE,
      limits = x_axis_levels,
      breaks = breaks_x,
      labels = labels_x
    ) +
    scale_fill_manual(
      values = legend_colors,
      na.translate = FALSE,
      drop = FALSE
    ) +
    labs(
      x = instruction$x_title,
      y = if (!is.null(instruction$y_title)) {
        if (unit_label != "" &&
            !str_detect(
              str_trim(instruction$y_title),
              fixed(unit_label)
            )) {
          paste0(instruction$y_title, " (", unit_label, ")")
        } else {
          instruction$y_title
        }
      } else {
        NULL
      },
      title = " ",
      fill = NULL
    ) +
    global_theme() +
    theme(
      plot.title = element_text(
        color = "white",
        face = "bold",
        size = 26,
        hjust = 0
      ),
      plot.margin = margin(t = 0, r = 40, b = 0, l = 40),
      legend.position = "bottom",
      legend.text = if (hide_legend_elements) {
        element_blank()
      } else {
        element_text(color = "white", size = 16, face = "bold")
      },
      legend.title = element_blank(),
      legend.spacing.y = unit(10, "pt")
    ) +
    guides(
      fill = guide_legend(
        override.aes = if (hide_legend_elements) {
          list(fill = NA, color = NA)
        } else {
          list()
        },
        title = NULL,
        label.theme = if (hide_legend_elements) {
          element_blank()
        } else {
          element_text()
        }
      )
    ) +
    coord_cartesian(ylim = c(0, y_max), clip = "off")
  
  # ------ VALUE LABELS --------------------------------------------------------
  df_labels <- get_x_centers(df_bars_render, x_axis_var) %>%
    filter(!is.na(fill_group_show), value > 0)
  
  plot_obj <- plot_obj +
    geom_text(
      data = df_labels,
      aes(
        x = x_center,
        y = value / 2,
        label = paste0(value, unit_label)
      ),
      inherit.aes = FALSE,
      color = "black",
      size = 6.5,
      fontface = "bold",
      vjust = 0.5,
      hjust = 0.5
    )
  
  # ------ TARGET LINES --------------------------------------------------------
  if (!is.null(target_metrics) && show_target) {
    df_targets_draw <- df_targets_all %>%
      left_join(
        df_labels %>% select(group, metric, x_center),
        by = c("group", "metric")
      ) %>%
      filter(!is.na(x_center))
    
    n_bar_slots <- df_bars_render %>%
      group_by(.data[[x_axis_var]]) %>%
      summarise(n = n(), .groups = "drop") %>%
      pull(n) %>%
      max()
    
    offset <- bar_width / (n_bar_slots * 2)
    
    plot_obj <- plot_obj +
      geom_segment(
        data = df_targets_draw,
        aes(
          x = x_center - offset,
          xend = x_center + offset,
          y = value,
          yend = value
        ),
        color = "#f9f871",
        linetype = "dashed",
        linewidth = 1.5,
        inherit.aes = FALSE
      ) +
      geom_text(
        data = df_targets_draw,
        aes(
          x = x_center,
          y = value + 5,
          label = paste0(value, unit_label)
        ),
        color = "#f9f871",
        size = 6.5,
        fontface = "bold",
        inherit.aes = FALSE
      )
  }
  
  # ------ EXPORT TO POWERPOINT ------------------------------------------------
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
#' @return Updated pptx object if `ppt_doc` is provided; otherwise, `NULL`.
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
    return(ppt_doc)
  }
  
  return(invisible(NULL))
}

# ------ HORIZONTAL BAR SLIDE -------------------------------------------------

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
#' @return Updated pptx object if exporting, otherwise the ggplot object.
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
  
  # ------ FILTER TO FOCAL GROUP IF NEEDED ------------------------------------
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
  # ------ PREPARE DATA FUNCTION --------------------------------------------
  prep_data <- function(ids, type_label) {
    data %>%
      select(all_of(ids)) %>%
      summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
      pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
      left_join(variable_map, by = "variable") %>%
      mutate(type = type_label) %>%
      select(label, value, type)
  }
  
  # ------ GENERATE DATA ----------------------------------------------------
  df_metric <- prep_data(metric_ids, x_titles[[1]])
  df_subj <- if (has_subj) prep_data(subj_ids, x_titles[[2]]) else NULL
  
  # ------ COMBINE & ORDER --------------------------------------------------
  label_levels <- bind_rows(df_metric, df_subj) %>%
    arrange(value) %>%
    pull(label) %>%
    unique()
  
  df_combined <- bind_rows(df_metric, df_subj) %>%
    mutate(
      label = factor(label, levels = label_levels),
      type = factor(type, levels = x_titles)
    )
  # ------ VISUAL SETTINGS --------------------------------------------------
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
      if (max(rng) <= 3) seq(0, ceiling(rng[2]), 1) else breaks_extended(n = 4)(x)
    }
    x_labels_fun <- function(x) {
      if (max(x, na.rm = TRUE) <= 3) as.character(x) else label_number(accuracy = 1)(x)
    }
  } else {
    facet_layer <- NULL
    plot_margin <- margin(20, 80, 10, 30)
    x_axis_title <- x_titles[[1]]
    axis_line_x_bottom <- element_line(color = "white", linewidth = 1)
    x_breaks_fun <- function(x) breaks_extended(n = 4)(x)
    x_labels_fun <- label_number(accuracy = 1)
  }
  
  # ------ BUILD PLOT -------------------------------------------------------
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
      axis.text.y = element_text(color = "white", face = "bold", size = 16),
      axis.title.x = element_text(color = "white", face = "bold", size = 20, margin = margin(t = 20)),
      axis.title.y = element_text(color = "white", face = "bold", size = 22, margin = margin(r = 20)),
      plot.margin = plot_margin
    )
  
  # ------ EXPORT OR RETURN -------------------------------------------------
  if (!is.null(ppt_doc)) {
    return(export_plot_to_slide(
      ppt_doc = ppt_doc,
      plot_obj = plot_obj,
      title_text = chart_title,
      is_first = instruction$is_first
    ))
  }
  
  return(plot_obj)
}
