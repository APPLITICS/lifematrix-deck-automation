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
#' @return Updated PowerPoint object (`ppt_doc`) and density plot (`plot_obj`).
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
  }
  
  return(
    list(
      ppt_doc = ppt_doc,
      plot_obj = plot_obj
    )
  )
}
