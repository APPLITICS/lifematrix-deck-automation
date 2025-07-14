# ------ BAR METRIC SLIDE ------------------------------------------------------
#' Generate Grouped Bar Chart and Export to PowerPoint
#'
#' Creates a grouped bar chart comparing a focal group to optional comparison
#' groups across metrics, with optional target lines and placeholder logic.
#' Supports unit displaying, custom labels, and slide export via `officer`.
#'
#' @param data A data frame of pre-processed values.
#' @param instruction A list with plot settings:
#'   - `bar_value`: Metric(s) for bar heights
#'   - `target`: (Optional) Metric(s) for target lines
#'   - `focal_group`, `comparison_groups`: Group definitions
#'   - `unit`, `title`, `x_title`, `y_title`, `is_first`: Display options
#' @param ppt_doc Optional `read_pptx()` object to append the slide.
#'
#' @return Updated PowerPoint object if `ppt_doc` is provided, otherwise `NULL`.
# ------ BAR METRIC SLIDE ------------------------------------------------------

#' Generate Grouped Bar Chart and Export to PowerPoint
#'
#' Creates a grouped bar chart comparing a focal group to optional comparison
#' groups across metrics, with optional target lines and placeholder logic.
#' Supports unit displaying, custom labels, and slide export via `officer`.
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
  # ------ BAR WIDTH & POSITION SETUP ------------------------------------------
  # Compute appropriate bar width and position settings based on groups
  bar_width <- if (
    length(instruction$bar_value) == 1 &&
    (
      is.null(instruction$comparison_groups) ||
      all(sapply(
        instruction$comparison_groups,
        function(cg) is.null(cg$name) || is.na(cg$name)
      ))
    )
  ) {
    0.4
  } else {
    0.7
  }
  dodge_width <- 0.8
  unit_label <- if (!is.null(instruction$unit)) instruction$unit else ""
  
  position_setting <- if (
    length(instruction$bar_value) == 1 &&
    (
      is.null(instruction$comparison_groups) ||
      all(sapply(
        instruction$comparison_groups,
        function(cg) is.null(cg$name) || is.na(cg$name)
      ))
    )
  ) {
    position_identity()
  } else {
    position_dodge(width = dodge_width)
  }
  # ------ METRIC VALIDATION  --------------------------------------------------
  all_metrics <- unique(c(
    instruction$bar_value,
    instruction$target %||% character()
  ))
  missing_metrics <- setdiff(all_metrics, names(data))
  if (length(missing_metrics) > 0) {
    message(sprintf(
      "Missing metric(s): %s",
      paste(missing_metrics, collapse = ", ")
    ))
    return(invisible(NULL))
  }
  # ------ HELPERS ------------------------------------------------------------
  # Clean metric names by removing common suffixes (e.g. "joy_min" => "Joy")
  clean_metric_labels <- function(labels) {
    words_list <- strsplit(
      str_to_title(gsub("_", " ", labels)),
      " "
    )
    suffixes <- sapply(words_list, function(x) tail(x, 1))
    suffix_table <- table(suffixes)
    common_suffix <- names(suffix_table)[which.max(suffix_table)]
    count_common <- max(suffix_table)
    if (count_common >= 2) {
      cleaned <- mapply(function(words) {
        if (tail(words, 1) == common_suffix && length(words) > 1) {
          paste(head(words, -1), collapse = " ")
        } else {
          paste(words, collapse = " ")
        }
      }, words_list, USE.NAMES = FALSE)
    } else {
      cleaned <- sapply(words_list, paste, collapse = " ")
    }
    
    return(cleaned)
  }
  # Generate a placeholder for missing group-metric combinations
  generate_placeholder <- function(group_label, metric_list) {
    tibble(group = group_label, metric = metric_list, value = 0)
  }
  # Compute average metric values for a given group and optional subset filter
  preprocess_group <- function(df_input, group_info, metric_list) {
    df <- df_input %>% filter(group == group_info$name)
    
    if (
      !is.null(group_info$subset) &&
      !is.null(group_info$subset$value) &&
      group_info$subset$title %in% names(df)
    ) {
      df <- df %>%
        filter(.data[[group_info$subset$title]] ==
                 group_info$subset$value)
    }
    
    if (nrow(df) == 0) return(NULL)
    
    values <- df %>%
      summarise(across(all_of(metric_list), ~ mean(.x, na.rm = TRUE)))
    
    group_label <- group_info$name
    if (!is.null(group_info$subset) &&
        !is.null(group_info$subset$value)) {
      group_label <- paste(group_label, group_info$subset$value)
    }
    
    tibble(
      group = group_label,
      metric = metric_list,
      value = as.numeric(values[1, ])
    )
  }
  # Calculate the center x position of each bar for label and line alignment
  get_x_centers <- function(df_plot, x_axis_var) {
    tmp_plot <- ggplot(
      df_plot,
      aes(
        x = .data[[x_axis_var]],
        y = .data[["value"]],
        fill = .data[["fill_group_show"]],
        group = interaction(group, metric)
      )
    ) +
      geom_col(width = bar_width, position = position_setting)
    
    bar_layer <- ggplot_build(tmp_plot)$data[[1]]
    
    df_plot %>%
      bind_cols(x_center = bar_layer$x) %>%
      select(group, metric, value, fill_group, fill_group_show, x_center)
  }
  # Generate a unique invisible label using space characters for placeholders
  generate_placeholder_label <- function(index) {
    paste(rep(" ", index), collapse = "")
  }
  
  # ------ PREPROCESS BARS -----------------------------------------------------
  # Aggregate values for focal and comparison groups
  group_labels <- character(0)
  data_bars <- list()
  placeholder_count <- 0
  
  fg_label <- instruction$focal_group$name
  if (!is.null(instruction$focal_group$subset) &&
      !is.null(instruction$focal_group$subset$value)) {
    fg_label <- paste(fg_label, instruction$focal_group$subset$value)
  }
  
  group_labels <- c(group_labels, fg_label)
  data_bars <- c(
    data_bars,
    list(preprocess_group(
      data,
      instruction$focal_group,
      instruction$bar_value
    ))
  )
  
  if (!is.null(instruction$comparison_groups)) {
    for (cg in instruction$comparison_groups) {
      if (is.null(cg$name) || is.na(cg$name)) {
        placeholder_count <- placeholder_count + 1
        placeholder_label <- generate_placeholder_label(
          placeholder_count
        )
        group_labels <- c(group_labels, placeholder_label)
        data_bars <- c(
          data_bars,
          list(generate_placeholder(
            placeholder_label,
            instruction$bar_value
          ))
        )
      } else {
        cg_label <- cg$name
        if (!is.null(cg$subset) &&
            !is.null(cg$subset$value)) {
          cg_label <- paste(cg_label, cg$subset$value)
        }
        
        group_labels <- c(group_labels, cg_label)
        data_bars <- c(
          data_bars,
          list(preprocess_group(data, cg, instruction$bar_value))
        )
      }
    }
  }
  
  group_labels <- unique(group_labels)
  df_bars <- bind_rows(data_bars)
  if (nrow(df_bars) == 0) return(invisible(NULL))
  # ------ FORMAT DATA FOR PLOTTING --------------------------------------------
  # Prepare factor levels and x-axis configuration
  metric_labels <- clean_metric_labels(instruction$bar_value)
  df_bars <- df_bars %>%
    mutate(
      metric = clean_metric_labels(metric),
      fill_group = group,
      fill_group_show = ifelse(
        group %in% group_labels & str_trim(group) != "",
        as.character(group),
        NA
      )
    )
  
  x_axis_var <- if (length(instruction$bar_value) == 1) "group" else "metric"
  x_axis_levels <- if (x_axis_var == "group") group_labels else metric_labels
  
  df_bars[[x_axis_var]] <- factor(df_bars[[x_axis_var]],
                                  levels = x_axis_levels
  )
  df_bars$group <- factor(df_bars$group, levels = group_labels)
  df_bars$fill_group <- factor(df_bars$fill_group, levels = group_labels)
  df_bars$fill_group_show <- factor(
    df_bars$fill_group_show,
    levels = setdiff(group_labels, " ")
  )
  
  non_zero_groups <- df_bars %>%
    filter(group != " ", value > 0) %>%
    pull(fill_group_show) %>%
    unique()
  
  hide_legend_elements <- (length(non_zero_groups) <= 1 ||
                             x_axis_var == "group")
  
  legend_colors <- get_color_palette(setdiff(group_labels, " "))
  
  y_max <- ceiling((max(df_bars$value, na.rm = TRUE) + 10) / 10) * 10
  # ------ BUILD BAR PLOT -------------------------------------------------------
  # Create the base ggplot object with bars
  plot_obj <- ggplot(
    df_bars,
    aes(
      x = .data[[x_axis_var]],
      y = .data[["value"]],
      fill = .data[["fill_group_show"]],
      group = interaction(group, metric)
    )
  ) +
    geom_col(width = bar_width, position = position_setting) +
    scale_y_continuous(
      limits = c(0, y_max),
      expand = c(0, 0),
      labels = function(x) paste0(x, unit_label)
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
            !str_detect(instruction$y_title, unit_label)) {
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
        color = "white", face = "bold", size = 26, hjust = 0
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
    )
  
  # ------ LABELS -------------------------------------------------------------
  # Add text labels inside bars with actual values
  df_labels <- get_x_centers(df_bars, x_axis_var) %>%
    filter(value > 0, group != " ")
  
  plot_obj <- plot_obj +
    geom_text(
      data = df_labels,
      aes(x = x_center, y = value / 2,
          label = paste0(round(value), unit_label)),
      inherit.aes = FALSE,
      color = "black",
      size = 6.5,
      fontface = "bold"
    )
  
  # ------ TARGET LINES -------------------------------------------------------
  # Draw horizontal target lines and annotate with value
  if (!is.null(instruction$target)) {
    data_targets <- list(
      preprocess_group(data, instruction$focal_group, instruction$target)
    )
    if (!is.null(instruction$comparison_groups)) {
      for (cg in instruction$comparison_groups) {
        if (!is.null(cg$name) && !is.na(cg$name)) {
          data_targets <- c(
            data_targets,
            list(preprocess_group(data, cg, instruction$target))
          )
        } else {
          placeholder_count <- placeholder_count + 1
          placeholder_label <- generate_placeholder_label(
            placeholder_count
          )
          data_targets <- c(
            data_targets,
            list(generate_placeholder(
              placeholder_label,
              instruction$target
            ))
          )
        }
      }
    }
    
    df_targets <- bind_rows(data_targets) %>%
      filter(value > 0, !is.na(value)) %>%
      mutate(
        target_label = clean_metric_labels(metric),
        group = factor(group, levels = group_labels)
      )
    
    bar_target_map <- tibble(
      bar_label = clean_metric_labels(instruction$bar_value),
      target_label = clean_metric_labels(instruction$target)
    )
    
    df_target <- df_targets %>%
      merge(bar_target_map, by = "target_label")
    
    if (x_axis_var == "group") {
      df_target <- df_targets %>%
        left_join(
          df_labels %>% select(group, x_center) %>% distinct(),
          by = "group"
        )
    } else {
      df_target <- df_target %>%
        left_join(
          df_labels %>% select(group, metric, x_center) %>% distinct(),
          by = c("group", "bar_label" = "metric")
        )
    }
    
    n_bar_slots <- df_bars %>%
      group_by(.data[[x_axis_var]]) %>%
      summarise(n = n(), .groups = "drop") %>%
      pull(n) %>%
      max()
    
    offset <- bar_width / (n_bar_slots * 2)
    
    if (nrow(df_target) > 0) {
      plot_obj <- plot_obj +
        geom_segment(
          data = df_target,
          aes(
            x = x_center - offset,
            xend = x_center + offset,
            y = value,
            yend = value
          ),
          color = "#f9f871",
          linetype = "11",
          linewidth = 1.5,
          inherit.aes = FALSE
        ) +
        geom_text(
          data = df_target,
          aes(x = x_center, y = value + 5,
              label = paste0(round(value), unit_label)),
          color = "#f9f871",
          size = 6.5,
          fontface = "bold",
          inherit.aes = FALSE
        )
    }
  }
  
  # ------ EXPORT -------------------------------------------------------------
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
  # ------ SETUP --------------------------------------------------------------
  # Extract key instruction fields and default unit
  unit_label <- instruction$unit %||% ""
  category_var <- instruction$category$name
  order_var <- instruction$category$order
  metric_var <- instruction$metric
  group_info <- instruction$focal_group
  
  # ------ FILTER GROUP -------------------------------------------------------
  # Keep rows for focal group and apply optional subset filter
  df <- data %>% filter(group == group_info$name)
  
  if (!is.null(group_info$subset)) {
    subset_col <- group_info$subset$title
    subset_val <- group_info$subset$value
    if (!is.null(subset_col) && !is.null(subset_val)) {
      df <- df %>% filter(.data[[subset_col]] == subset_val)
    }
  }
  
  # ------ ORDER CATEGORIES ---------------------------------------------------
  # Extract and sort category levels based on provided order column
  ordered_levels <- df %>%
    select(
      category = all_of(category_var),
      order = all_of(order_var)
    ) %>%
    distinct() %>%
    arrange(order) %>%
    pull(category)
  
  # ------ AGGREGATE METRIC VALUES --------------------------------------------
  # Compute average metric value per category and apply factor levels
  df <- df %>%
    group_by(.data[[category_var]]) %>%
    summarise(
      value = mean(.data[[metric_var]], na.rm = TRUE),
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
    mutate(x_center = seq_len(n()))
  
  # ------ Y AXIS MAX ----------------------------------------------------------
  # Compute y-axis maximum for consistent scale
  y_max <- ceiling(max(df$value, na.rm = TRUE))
  
  # ------ BUILD PLOT ----------------------------------------------------------
  # Construct ggplot2 bar chart with value labels and formatting
  plot_obj <- ggplot(df, aes(x = .data[[category_var]], y = value)) +
    geom_col(
      fill = "#70e2ff",
      width = 0.5
    ) +
    geom_text(
      aes(
        label = if (unit_label == "") {
          round(value, 1)
        } else {
          paste0(round(value, 0), unit_label)
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
#' @param data A data frame with numeric values for hours and subjective scores.
#' @param instruction List of slide instruction configurations.
#' @param ppt_doc Optional PowerPoint object from `read_pptx()`.
#'
#' @return Updated pptx object if `ppt_doc` is provided; otherwise, `NULL`.
generate_horizontal_bar_slide <- function(
    data,
    instruction,
    ppt_doc
) {

  
  # ------ EXTRACT INSTRUCTION SETTINGS ---------------------------------------
  selected_labels <- instruction$metric
  subjective <- instruction$subjective_value %||% FALSE
  x_titles <- instruction$x_title
  
  # ------ DYNAMIC VARIABLE MAPPING -------------------------------------------
  label_variable_pairs <- variable_map %>%
    filter(label %in% selected_labels) %>%
    group_by(label) %>%
    summarise(variables = list(variable), .groups = "drop") %>%
    filter(lengths(variables) == 2) %>%
    deframe()
  
  explicit_map <- rbindlist(
    lapply(names(label_variable_pairs), function(lbl) {
      data.table(
        label = lbl,
        variable = label_variable_pairs[[lbl]],
        type = c("hours", "subjective")
      )
    })
  )
  
  selected_map <- explicit_map[label %in% selected_labels]
  
  # ------ PREPARE HOURS DATA -------------------------------------------------
  hours_map <- selected_map[type == "hours"]
  hours_df <- data %>%
    select(all_of(hours_map$variable)) %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "value"
    ) %>%
    left_join(hours_map, by = "variable") %>%
    mutate(type = x_titles[[1]]) %>%
    select(label, value, type)
  
  # ------ PREPARE SUBJECTIVE DATA --------------------------------------------
  if (subjective) {
    subjective_map <- selected_map[type == "subjective"]
    subjective_df <- data %>%
      select(all_of(subjective_map$variable)) %>%
      summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
      pivot_longer(
        cols = everything(),
        names_to = "variable",
        values_to = "value"
      ) %>%
      left_join(subjective_map, by = "variable") %>%
      mutate(type = x_titles[[2]]) %>%
      select(label, value, type)
    
    activity_summary <- bind_rows(hours_df, subjective_df) %>%
      mutate(label = fct_reorder2(label, type, value))
  } else {
    activity_summary <- hours_df %>%
      mutate(label = fct_reorder(label, value))
  }
  
  # ------ DEFINE CONDITIONAL PLOT STYLE --------------------------------------
  if (subjective) {
    plot_margin <- margin(20, 50, 20, 50)
    axis_line_x <- element_blank()
  } else {
    plot_margin <- margin(20, 80, 20, 30)
    axis_line_x <- element_line(color = "white", linewidth = 1)
  }
  
  # ------ BUILD PLOT ---------------------------------------------------------
  plot_obj <- ggplot(
    activity_summary,
    aes(x = value, y = label)
  ) +
    geom_col(fill = "#84d8f6", width = 0.7) +
    {
      if (subjective) facet_grid(. ~ type, scales = "free_x", switch = "x")
    } +
    scale_x_continuous(
      breaks = function(x) {
        rng <- range(x, na.rm = TRUE)
        if (rng[2] <= 3) {
          seq(0, ceiling(rng[2] * 2) / 2, by = 0.5)
        } else {
          breaks_extended(n = 4)(x)
        }
      },
      labels = function(x) {
        if (max(x, na.rm = TRUE) <= 3) {
          label_number(accuracy = 0.5)(x)
        } else {
          label_number(accuracy = 1)(x)
        }
      },
      expand = c(0, 0)
    ) +
    labs(
      x = if (subjective) NULL else x_titles[[1]],
      y = instruction$y_title
    ) +
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
      axis.line.x = axis_line_x,
      axis.text.x = element_text(color = "white", size = 14),
      axis.text.y = element_text(color = "white", face = "bold", size = 16),
      axis.title.x = element_text(
        color = "white", face = "bold", size = 16,
        margin = margin(t = if (subjective) 10 else 40)
      ),
      axis.title.y = element_text(
        color = "white", face = "bold", size = 22,
        margin = margin(r = 20)
      ),
      plot.margin = plot_margin
    )
  
  # ------ OPTIONAL POWERPOINT EXPORT -----------------------------------------
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
