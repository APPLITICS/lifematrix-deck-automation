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

generate_bar_metric_slide <- function(
    data,
    instruction,
    ppt_doc
) {
  bar_width   <- 0.7
  dodge_width <- 0.8
  unit_label  <- if (!is.null(instruction$unit)) instruction$unit else ""
  
  # ------ METRIC CHECK -------------------------------------------------------
  all_metrics <- unique(c(instruction$bar_value, instruction$target %||% character()))
  missing_metrics <- setdiff(all_metrics, names(data))
  if (length(missing_metrics) > 0) {
    message(sprintf("Missing metric(s): %s", paste(missing_metrics, collapse = ", ")))
    return(invisible(NULL))
  }
  
  # ------ HELPERS ------------------------------------------------------------
  clean_metric_labels <- function(labels) {
    words_list <- strsplit(stringr::str_to_title(gsub("_", " ", labels)), " ")
    suffixes <- sapply(words_list, function(x) tail(x, 1))
    suffix_table <- table(suffixes)
    common_suffix <- names(suffix_table)[which.max(suffix_table)]
    count_common  <- max(suffix_table)
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
  
  generate_placeholder <- function(group_label, metric_list) {
    tibble::tibble(group = group_label, metric = metric_list, value = 0)
  }
  
  preprocess_group <- function(df_input, group_info, metric_list) {
    df <- df_input %>% filter(group == group_info$name)
    
    if (!is.null(group_info$subset) &&
        !is.null(group_info$subset$value) &&
        group_info$subset$title %in% names(df)) {
      df <- df %>% filter(.data[[group_info$subset$title]] == group_info$subset$value)
    }
    
    if (nrow(df) == 0) return(NULL)
    
    values <- df %>% summarise(across(all_of(metric_list), ~ mean(.x, na.rm = TRUE)))
    group_label <- group_info$name
    if (!is.null(group_info$subset) && !is.null(group_info$subset$value)) {
      group_label <- paste(group_label, group_info$subset$value)
    }
    
    tibble::tibble(
      group  = group_label,
      metric = metric_list,
      value  = as.numeric(values[1, ])
    )
  }
  
  get_x_centers <- function(df_plot, x_axis_var) {
    tmp_plot <- ggplot(df_plot, aes_string(
      x = x_axis_var, y = "value",
      fill = "fill_group_show",
      group = "interaction(group, metric)"
    )) +
      geom_col(width = bar_width, position = position_dodge(width = dodge_width))
    bar_layer <- ggplot_build(tmp_plot)$data[[1]]
    df_plot %>%
      bind_cols(x_center = bar_layer$x) %>%
      select(group, metric, value, fill_group, fill_group_show, x_center)
  }
  
  generate_placeholder_label <- function(index) {
    paste(rep(" ", index), collapse = "")
  }
  
  # ------ PREPROCESS BARS ----------------------------------------------------
  group_labels <- character(0)
  data_bars <- list()
  placeholder_count <- 0
  
  fg_label <- instruction$focal_group$name
  if (!is.null(instruction$focal_group$subset) && !is.null(instruction$focal_group$subset$value)) {
    fg_label <- paste(fg_label, instruction$focal_group$subset$value)
  }
  
  group_labels <- c(group_labels, fg_label)
  data_bars <- c(data_bars, list(preprocess_group(data, instruction$focal_group, instruction$bar_value)))
  
  if (!is.null(instruction$comparison_groups)) {
    for (cg in instruction$comparison_groups) {
      if (is.null(cg$name) || is.na(cg$name)) {
        placeholder_count <- placeholder_count + 1
        placeholder_label <- generate_placeholder_label(placeholder_count)
        group_labels <- c(group_labels, placeholder_label)
        data_bars <- c(data_bars, list(generate_placeholder(placeholder_label, instruction$bar_value)))
      } else {
        cg_label <- cg$name
        if (!is.null(cg$subset) && !is.null(cg$subset$value)) {
          cg_label <- paste(cg_label, cg$subset$value)
        }
        group_labels <- c(group_labels, cg_label)
        data_bars <- c(data_bars, list(preprocess_group(data, cg, instruction$bar_value)))
      }
    }
  }
  
  group_labels <- unique(group_labels)
  df_bars <- bind_rows(data_bars)
  if (nrow(df_bars) == 0) return(invisible(NULL))
  
  metric_labels <- clean_metric_labels(instruction$bar_value)
  df_bars <- df_bars %>%
    mutate(
      metric = clean_metric_labels(metric),
      fill_group = group,
      fill_group_show = ifelse(group %in% group_labels & str_trim(group) != "", as.character(group), NA)
    )
  
  x_axis_var <- if (length(instruction$bar_value) == 1) "group" else "metric"
  x_axis_levels <- if (x_axis_var == "group") group_labels else metric_labels
  
  df_bars[[x_axis_var]] <- factor(df_bars[[x_axis_var]], levels = x_axis_levels)
  df_bars$group <- factor(df_bars$group, levels = group_labels)
  df_bars$fill_group <- factor(df_bars$fill_group, levels = group_labels)
  df_bars$fill_group_show <- factor(df_bars$fill_group_show, levels = setdiff(group_labels, " "))
  
  non_zero_groups <- df_bars %>%
    filter(group != " ", value > 0) %>%
    pull(fill_group_show) %>%
    unique()
  
  hide_legend_elements <- (length(non_zero_groups) <= 1 || x_axis_var == "group")
  legend_colors <- get_color_palette(setdiff(group_labels, " "))
  
  y_max <- ceiling((max(df_bars$value, na.rm = TRUE) + 10) / 10) * 10
  plot_obj <- ggplot(df_bars, aes_string(
    x = x_axis_var, y = "value",
    fill = "fill_group_show",
    group = "interaction(group, metric)"
  )) +
    geom_col(width = bar_width, position = position_dodge(width = dodge_width)) +
    scale_y_continuous(
      limits = c(0, y_max),
      expand = c(0, 0),
      labels = function(x) paste0(x, unit_label)
    ) +
    scale_fill_manual(values = legend_colors, na.translate = FALSE, drop = FALSE) +
    labs(
      x = instruction$x_title,
      y = if (!is.null(instruction$y_title)) {
        if (unit_label != "" && !str_detect(instruction$y_title, unit_label)) {
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
      plot.title = element_text(color = "white", face = "bold", size = 26, hjust = 0),
      plot.margin = margin(t = 0, r = 40, b = 0, l = 40),
      legend.position = "bottom",
      legend.text = if (hide_legend_elements) element_blank() else element_text(color = "white", size = 16, face = "bold"),
      legend.title = element_blank(),
      legend.spacing.y = unit(10, "pt")
    ) +
    guides(
      fill = guide_legend(
        override.aes = if (hide_legend_elements) list(fill = NA, color = NA) else list(),
        title = NULL,
        label.theme = if (hide_legend_elements) element_blank() else element_text()
      )
    )
  
  # ------ LABELS -------------------------------------------------------------
  df_labels <- get_x_centers(df_bars, x_axis_var) %>%
    filter(value > 0, group != " ")
  
  plot_obj <- plot_obj +
    geom_text(
      data = df_labels,
      aes(x = x_center, y = value / 2, label = paste0(round(value), unit_label)),
      inherit.aes = FALSE,
      color = "black",
      size = 6.5,
      fontface = "bold"
    )
  
  # ------ TARGET LINES -------------------------------------------------------
  if (!is.null(instruction$target)) {
    data_targets <- list(preprocess_group(data, instruction$focal_group, instruction$target))
    if (!is.null(instruction$comparison_groups)) {
      for (cg in instruction$comparison_groups) {
        if (!is.null(cg$name) && !is.na(cg$name)) {
          data_targets <- c(data_targets, list(preprocess_group(data, cg, instruction$target)))
        } else {
          placeholder_count <- placeholder_count + 1
          placeholder_label <- generate_placeholder_label(placeholder_count)
          data_targets <- c(data_targets, list(generate_placeholder(placeholder_label, instruction$target)))
        }
      }
    }
    
    df_targets <- bind_rows(data_targets) %>%
      filter(value > 0, !is.na(value)) %>%
      mutate(
        target_label = clean_metric_labels(metric),
        group = factor(group, levels = group_labels)
      )
    
    bar_target_map <- tibble::tibble(
      bar_label    = clean_metric_labels(instruction$bar_value),
      target_label = clean_metric_labels(instruction$target)
    )
    
    df_target <- df_targets %>%
      merge(bar_target_map, by = "target_label")
    
    if (x_axis_var == "group") {
      df_target <- df_targets %>%
        left_join(df_labels %>% select(group, x_center) %>% distinct(), by = "group")
    } else {
      df_target <- df_target %>%
        left_join(df_labels %>% select(group, metric, x_center) %>% distinct(),
                  by = c("group", "bar_label" = "metric"))
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
          aes(x = x_center - offset, xend = x_center + offset, y = value, yend = value),
          color = "#f9f871",
          linetype = "11",
          linewidth = 1.5,
          inherit.aes = FALSE
        ) +
        geom_text(
          data = df_target,
          aes(x = x_center, y = value + 5, label = paste0(round(value), unit_label)),
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
#' @param data A data frame of pre-processed values.
#' @param instruction A list of chart options (group info, metric, category, labels, etc.).
#' @param ppt_doc Optional `read_pptx()` object for exporting the chart.
#'
#' @return Updated pptx object if `ppt_doc` is provided; otherwise, `NULL`.
generate_bar_category_slide <- function(
    data,
    instruction,
    ppt_doc
) {
  
  # --- Setup -----------------------------------------------------------------
  unit_label   <- instruction$unit %||% ""
  category_var <- instruction$category$name
  order_var    <- instruction$category$order
  metric_var   <- instruction$metric
  group_info   <- instruction$focal_group
  # --- Filter focal group + optional subset ----------------------------------
  df <- data %>% filter(group == group_info$name)
  
  if (!is.null(group_info$subset)) {
    subset_col <- group_info$subset$title
    subset_val <- group_info$subset$value
    if (!is.null(subset_col) && !is.null(subset_val)) {
      df <- df %>% filter(.data[[subset_col]] == subset_val)
    }
  }
  
  # --- Extract and apply category order -------------------------------------
  ordered_levels <- df %>%
    select(category = all_of(category_var), order = all_of(order_var)) %>%
    distinct() %>%
    arrange(order) %>%
    pull(category)
  
  # --- Aggregate and sort ---------------------------------------------------
  df <- df %>%
    group_by(.data[[category_var]]) %>%
    summarise(value = mean(.data[[metric_var]], na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(.data[[category_var]]), is.finite(value)) %>%
    mutate(
      !!category_var := factor(.data[[category_var]], levels = ordered_levels, ordered = TRUE)
    ) %>%
    arrange(.data[[category_var]]) %>%
    mutate(x_center = seq_len(n()))
  
  y_max <- ceiling(max(df$value, na.rm = TRUE))
  
  # --- Build plot -----------------------------------------------------------
  plot_obj <- ggplot(df, aes(x = .data[[category_var]], y = value)) +
    geom_col(fill = "#70e2ff", width = 0.5) +
    geom_text(
      aes(
        label = if (unit_label == "") round(value, 1) else paste0(round(value, 0), unit_label),
        y     = value / 2
      ),
      color = "black", size = 6.5, fontface = "bold"
    ) +
    scale_y_continuous(
      limits = c(0, y_max),
      breaks = if (unit_label == "%") seq(0, y_max, 10) else waiver(),
      labels = function(x) paste0(x, unit_label),
      expand = c(0, 0)
    ) +
    labs(
      x     = instruction$x_title %||% category_var,
      y     = instruction$y_title %||% metric_var,
      title = " "
    ) +
    global_theme() +
    theme(
      plot.title      = element_text(color = "white", face = "bold", size = 26, hjust = 0),
      plot.margin     = margin(30, 40, 30, 40),
      legend.position = "none"
    )
  
  # --- Trend line if requested ----------------------------------------------
  if (isTRUE(instruction$trend_line) && nrow(df) >= 2) {
    plot_obj <- plot_obj + annotate(
      "segment",
      x     = df$x_center[1],
      xend  = df$x_center[nrow(df)],
      y     = df$value[1],
      yend  = df$value[nrow(df)],
      color = "#f9f871",
      linewidth = 2
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
}

