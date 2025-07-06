# ------ BAR METRIC SLIDE ------------------------------------------------------
#' Generate grouped bar chart and export to PowerPoint
#'
#' Creates a grouped bar chart comparing a focal group and optional comparisons
#' across one or more metrics. Supports scaling, placeholder logic, targets,
#' and PowerPoint export via `officer` and `rvg`.
#'
#' @param data A data frame with aggregated values.
#' @param instruction A list with plot settings (group info, metrics, unit, labels, etc.).
#' @param ppt_doc Optional `read_pptx()` object for export.
#'
#' @return Updated pptx object if `ppt_doc` is given; otherwise, `NULL`.
generate_bar_metric_slide <- function(
    data,
    instruction,
    ppt_doc
) {
  # ------ UNIT SETUP ---------------------------------------------------------
  current_unit <- get_unit_helper(instruction$unit)
  
  # ------ PLACEHOLDER GENERATOR ----------------------------------------------
  data_placeholder <- function(group_label, metric_list) {
    extract_suffix <- function(colname) sub("^value_", "", colname)
    suffixes <- unique(na.omit(c(
      extract_suffix(instruction$bar_value),
      extract_suffix(instruction$target)
    )))
    expand_grid(
      metric = unique(gsub("__.*$", "", metric_list)),
      suffix = suffixes
    ) %>%
      mutate(
        value = 0,
        group = ifelse(is.na(group_label), " ", group_label)
      ) %>%
      pivot_wider(
        names_from   = suffix,
        values_from  = value,
        names_prefix = "value_"
      ) %>%
      relocate(group, metric)
  }
  
  # ------ PREPROCESS GROUP ---------------------------------------------------
  preprocess_group <- function(
    df_input,
    group_name_input,
    subset_input   = NULL,
    display_label  = NULL
  ) {
    df_sub <- df_input %>% filter(group == group_name_input)
    
    if (!is.null(subset_input) &&
        !is.null(subset_input$value) &&
        subset_input$title %in% names(df_sub)) {
      df_sub <- df_sub %>%
        filter(.data[[subset_input$title]] == subset_input$value)
    }
    
    base_metrics   <- unique(gsub("__.*$", "", instruction$metric))
    pattern        <- paste(base_metrics, collapse = "|")
    cols_available <- grep(pattern, names(df_sub), value = TRUE)
    
    if (nrow(df_sub) == 0 || length(cols_available) == 0) {
      label_clean <- display_label %||% group_name_input
      return(data_placeholder(label_clean, instruction$metric))
    }
    
    df_agg <- df_sub %>%
      summarise(across(all_of(cols_available), ~ mean(.x, na.rm = TRUE)))
    
    df_long <- df_agg %>%
      pivot_longer(
        cols      = everything(),
        names_to  = "full_name",
        values_to = "raw_value"
      ) %>%
      mutate(
        metric = stringr::str_extract(full_name, paste0("(", paste(base_metrics, collapse = "|"), ")")),
        suffix = stringr::str_replace(full_name, metric, "") %>%
          stringr::str_remove("^__") %>%
          replace_na("real") %>%
          na_if("") %>%
          replace_na("real")
      ) %>%
      select(metric, suffix, raw_value)
    
    df_long <- pivot_wider(
      df_long,
      names_from   = suffix,
      values_from  = raw_value,
      names_prefix = "value_"
    )
    
    df_long %>%
      mutate(group = display_label %||% group_name_input) %>%
      relocate(group, metric) %>%
      mutate(across(starts_with("value_"), current_unit$scale))
  }
  
  # ------ HELPER FUNCTIONS ---------------------------------------------------
  get_y_max <- function(df_plot) {
    y_vals <- df_plot[[instruction$bar_value]]
    if (!is.null(instruction$target) && instruction$target %in% names(df_plot)) {
      y_vals <- c(y_vals, df_plot[[instruction$target]])
    }
    y_max_raw <- max(y_vals, na.rm = TRUE)
    if (instruction$unit == "%") {
      margin <- if (is.null(instruction$target)) 0 else 10
      return(ceiling((y_max_raw + margin) / 10) * 10)
    } else {
      return(ceiling(y_max_raw + 1))
    }
  }
  
  get_x_centers <- function(df_plot, x_axis_var) {
    tmp_plot <- ggplot(df_plot, aes_string(
      x     = x_axis_var,
      y     = instruction$bar_value,
      fill  = "fill_group",
      group = "interaction(group, metric)"
    )) +
      geom_col(position = position_dodge(width = 0.8), width = 0.6)
    ggplot_build(tmp_plot)$data[[1]]$x
  }
  
  # ------ PREPROCESS DATA ----------------------------------------------------
  x_axis_var <- if (length(instruction$metric) == 1) "group" else "metric"
  
  data_focal <- preprocess_group(
    df_input         = data,
    group_name_input = instruction$focal_group$name,
    subset_input     = instruction$focal_group$subset,
    display_label    = instruction$focal_group$name
  )
  
  data_comparisons  <- NULL
  comparison_labels <- NULL
  
  if (!is.null(instruction$comparison_groups)) {
    raw_labels <- sapply(instruction$comparison_groups, function(cg) {
      if (!is.null(cg$name)) {
        if (!is.null(cg$subset)) paste(cg$name, cg$subset$value) else cg$name
      } else {
        " "
      }
    })
    
    label_counts <- ave(seq_along(raw_labels), raw_labels, FUN = seq_along)
    comparison_labels <- ifelse(
      table(raw_labels)[raw_labels] > 1,
      paste0(raw_labels, " (", label_counts, ")"),
      raw_labels
    )
    
    data_comparisons <- bind_rows(
      Map(function(group_cfg, label_out) {
        label_clean <- ifelse(is.na(label_out), " ", label_out)
        preprocess_group(
          df_input         = data,
          group_name_input = group_cfg$name,
          subset_input     = group_cfg$subset,
          display_label    = label_clean
        )
      }, instruction$comparison_groups, comparison_labels)
    )
  }
  
  # ------ FORMAT AND CLEAN ---------------------------------------------------
  df <- bind_rows(data_focal, data_comparisons)
  df$metric <- tools::toTitleCase(gsub("_", " ", df$metric))
  
  if (x_axis_var == "metric") {
    df$metric <- factor(df$metric, levels = tools::toTitleCase(gsub("_", " ", instruction$metric)))
  } else {
    df$group <- factor(df$group, levels = unique(df$group))
  }
  
  df$group      <- factor(df$group, levels = unique(df$group))
  df$fill_group <- df$group
  
  non_zero_groups <- df %>%
    filter(.data[[instruction$bar_value]] > 0, !is.na(fill_group)) %>%
    distinct(fill_group) %>%
    pull(fill_group)
  
  legend_order <- df %>%
    filter(.data[[instruction$bar_value]] > 0, !is.na(fill_group), fill_group != " ") %>%
    distinct(fill_group) %>%
    pull(fill_group)
  
  palette_base <- c("#70e2ff", "#97e37e", "#2dc595", "#5d8c90", "#cccccc")
  colors       <- setNames(palette_base[seq_along(legend_order)], legend_order)
  
  hide_legend_elements <- (length(non_zero_groups) <= 1 || x_axis_var == "group")
  legend_position      <- "bottom"
  
  # ------ BUILD PLOT ---------------------------------------------------------
  y_max       <- get_y_max(df)
  df$x_center <- get_x_centers(df, x_axis_var)
  
  plot_obj <- ggplot(df, aes_string(
    x     = x_axis_var,
    y     = instruction$bar_value,
    fill  = "fill_group",
    group = "interaction(group, metric)"
  )) +
    geom_col(
      width    = 0.7,
      position = position_dodge(width = 0.8)
    ) +
    geom_text(
      aes_string(
        label = current_unit$label(instruction$bar_value),
        y     = paste0(instruction$bar_value, " / 2")
      ),
      position = position_dodge(width = 0.8),
      color    = "black",
      size     = 6.5,
      fontface = "bold"
    ) +
    scale_y_continuous(
      limits = c(0, y_max),
      breaks = if (instruction$unit == "%") seq(0, y_max, 20) else waiver(),
      labels = if (instruction$unit == "%") function(x) paste0(x, "%") else waiver(),
      expand = c(0, 0)
    ) +
    scale_fill_manual(
      values = colors,
      breaks = legend_order
    ) +
    labs(
      x     = instruction$x_title,
      y     = instruction$y_title,
      title = " ",
      fill  = NULL
    ) +
    global_theme() +
    theme(
      plot.title      = element_text(color = "white", face = "bold", size = 26, hjust = 0),
      plot.margin     = margin(t = 0, r = 40, b = 0, l = 40),
      legend.position = legend_position,
      legend.text     = if (hide_legend_elements) element_blank() else element_text(
        color = "white",
        size  = 16,
        face  = "bold"
      ),
      legend.title     = element_blank(),
      legend.spacing.y = unit(10, "pt")
    ) +
    guides(
      fill = guide_legend(
        override.aes = if (hide_legend_elements)
          list(fill = NA, color = NA) else list(),
        title       = NULL,
        label.theme = if (hide_legend_elements) element_blank() else element_text()
      )
    )
  
  # ------ TARGET LINES -------------------------------------------------------
  if (!is.null(instruction$target) && instruction$target %in% names(df)) {
    df_target <- df %>%
      filter(!is.na(.data[[instruction$target]]) & .data[[instruction$target]] > 0)
    
    bar_width   <- 0.6
    n_bar_slots <- nrow(df) / length(unique(df[[x_axis_var]]))
    offset      <- bar_width / (n_bar_slots * 2)
    
    plot_obj <- plot_obj +
      geom_segment(
        data = df_target,
        aes(
          x    = x_center - offset,
          xend = x_center + offset,
          y    = .data[[instruction$target]],
          yend = .data[[instruction$target]]
        ),
        color     = "#f9f871",
        linetype  = "dashed",
        linewidth = 2
      ) +
      geom_text(
        data = df_target,
        aes(
          x     = x_center,
          y     = .data[[instruction$target]] + current_unit$offset(),
          label = !!rlang::parse_expr(current_unit$target(instruction$target))
        ),
        color    = "#f9f871",
        size     = 6.5,
        fontface = "bold"
      )
  }
  
  print(plot_obj)
  
  # ------ EXPORT -------------------------------------------------------------
  if (!is.null(ppt_doc)) {
    ppt_doc <- export_plot_to_slide(
      ppt_doc    = ppt_doc,
      plot_obj   = plot_obj,
      title_text = instruction$title %||% " ",
      is_first   = instruction$is_first
    )
    return(ppt_doc)
  }
  
  return(invisible(NULL))
}

# ------ BAR CATEGORY SLIDE ----------------------------------------------------

#' Generate category bar chart and export to PowerPoint
#'
#' Creates a bar chart showing one metric split by categories (e.g. gender or income).
#' Supports category grouping, axis formatting, optional trend lines, and export to
#' PowerPoint or printing only.
#'
#' @param data A data frame of preprocessed values.
#' @param instruction A list of chart options (group info, metric, category, labels, etc.).
#' @param ppt_doc Optional `read_pptx()` object for exporting the chart.
#'
#' @return Updated pptx object if `ppt_doc` is provided; otherwise, `NULL`.
generate_bar_category_slide <- function(
    data,
    instruction,
    ppt_doc
) {
  # ------ UNIT SETUP ---------------------------------------------------------
  current_unit <- get_unit_helper(instruction$unit)
  
  # ------ PREPROCESS ---------------------------------------------------------
  preprocess_focal_group <- function(df_input, group_info) {
    df_filtered <- df_input
    if (!is.null(group_info$name)) {
      df_filtered <- df_filtered %>% filter(group == group_info$name)
    }
    if (!is.null(group_info$subset) &&
        !is.null(group_info$subset$value) &&
        group_info$subset$title %in% names(df_filtered)) {
      df_filtered <- df_filtered %>%
        filter(.data[[group_info$subset$title]] == group_info$subset$value)
    }
    df_filtered
  }
  
  # ------ APPLY GROUPING IF NEEDED ------------------------------------------
  metric_var     <- instruction$metric
  category_input <- instruction$category
  category_var   <- category_input$name
  
  data_focal <- preprocess_focal_group(data, instruction$focal_group)
  
  if (!is.null(category_input$grouping)) {
    data_focal   <- apply_category_grouping(data_focal, category_var, category_input$grouping)
    category_var <- "category_grouped"
  }
  
  # ------ AGGREGATE ----------------------------------------------------------
  df <- data_focal %>%
    group_by(.data[[category_var]]) %>%
    summarise(
      !!metric_var := mean(.data[[metric_var]], na.rm = TRUE),
      .groups      = "drop"
    )
  
  df[[metric_var]] <- current_unit$scale(df[[metric_var]])
  
  ordered_levels        <- sort_category_labels(as.character(df[[category_var]]))
  df[[category_var]]    <- factor(df[[category_var]], levels = ordered_levels, ordered = TRUE)
  df                    <- df[order(df[[category_var]]), ]
  df$x_center           <- seq_along(df[[category_var]])
  df$x_position         <- as.numeric(df[[category_var]])
  bar_width             <- 0.5
  y_max                 <- max(df[[metric_var]], na.rm = TRUE)
  y_max                 <- if (instruction$unit == "%") ceiling(y_max / 10) * 10 else ceiling(y_max)
  
  # ------ PLOT ---------------------------------------------------------------
  plot_obj <- ggplot(df, aes(x = .data[[category_var]], y = .data[[metric_var]])) +
    geom_col(
      fill  = "#70e2ff",
      width = bar_width
    ) +
    geom_text(
      aes(
        label = !!rlang::parse_expr(current_unit$label(metric_var)),
        y     = .data[[metric_var]] / 2
      ),
      color    = "black",
      size     = 6.5,
      fontface = "bold"
    ) +
    scale_y_continuous(
      limits = c(0, y_max),
      breaks = if (instruction$unit == "%") seq(0, y_max, 10) else waiver(),
      labels = if (instruction$unit == "%") function(x) paste0(x, "%") else waiver(),
      expand = c(0, 0)
    ) +
    scale_x_discrete(labels = format_x_labels_as_ordinal) +
    labs(
      x     = instruction$x_title %||% category_var,
      y     = instruction$y_title %||% NULL,
      title = " ",
      fill  = NULL
    ) +
    (if (is.null(instruction$y_title)) theme_minimal_yless() else global_theme()) +
    theme(
      plot.title      = element_text(color = "white", face = "bold", size = 26, hjust = 0),
      plot.margin     = margin(t = 30, r = 40, b = 30, l = 40),
      legend.position = "none"
    )
  
  # ------ TREND LINE ---------------------------------------------------------
  if (isTRUE(instruction$trend_line) && nrow(df) >= 2) {
    plot_obj <- plot_obj + geom_segment(
      aes(
        x    = df$x_center[1],
        y    = df[[metric_var]][1],
        xend = df$x_center[nrow(df)],
        yend = df[[metric_var]][nrow(df)]
      ),
      inherit.aes = FALSE,
      color       = "#f9f871",
      linewidth   = 2
    )
  }
  
  # ------ EXPORT -------------------------------------------------------------
  if (!is.null(ppt_doc)) {
    ppt_doc <- export_plot_to_slide(
      ppt_doc    = ppt_doc,
      plot_obj   = plot_obj,
      title_text = instruction$title %||% " ",
      is_first   = instruction$is_first
    )
    return(ppt_doc)
  }
  
  return(invisible(NULL))
}
