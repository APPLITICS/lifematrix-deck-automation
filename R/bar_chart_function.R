#' Draws a customized bar chart for focal and comparison groups based on provided
#' instructions.
#'
#' This function supports grouped bar charts using ggplot2, and includes:
#' - Preparation of summarized input data from raw values (e.g., by group, metric)
#' - Custom x-axis modes that allow switching between displaying group, metric,
#'   or category on the x-axis
#' - Placeholder bar reservation for missing comparison groups to preserve layout
#' - Optional dashed target lines and trend lines
#' - Automatic color assignment and label formatting
#'
#' @param df   Data frame containing the input data.
#' @param inst List containing the chart configuration, including:
#'   metric(s), group definitions, titles, units, and options for target/trend lines.

draw_bar_chart <- function(df, inst) {
  # ------ HELPERS -------------------------------------------------------------
  
  #' Sorts category labels, ensuring "Less" appears first, "More" last, and
  #' other categories follow numeric order if applicable
  sort_category_labels <- function(v) {
    nums <- suppressWarnings(as.numeric(sub("^([0-9]+).*", "\\1", v)))
    custom_order <- ifelse(
      grepl("^\\s*(Less|less)", v), 0,
      ifelse(grepl("^\\s*(More|more)", v), Inf, nums)
    )
    v[order(custom_order, na.last = TRUE)]
  }
  #' Generates a dummy null data to simulate placeholders space in the graph
  df_placeholder <- function(
    label,
    x_axis_var,
    inst,
    df
  ) {
    categories <- if (x_axis_var == "metric") {
      inst$metric
    } else {
      unique(df[[x_axis_var]])
    }
    
    data.frame(
      group            = if (!is.null(label)) label else " ",
      category         = categories,
      metric_value     = 0,
      munimums_value   = 0,
      stringsAsFactors = FALSE
    )
  }
  #' Maps consistent fill colors to groups for visual distinction
  color_map <- function(df_plot) {
    unique_fill_groups   <- unique(df_plot$fill_group)
    group_colors_palette <- c("#70e2ff", "#a4d65e", "#2dc595", "#5d8c90")
    
    setNames(
      group_colors_palette[seq_along(unique_fill_groups)],
      unique_fill_groups
    )
  }
  #' Gets bar center x-positions for label/text alignment and target line placement
  get_x_positions <- function(
    df_plot,
    inst
  ) {
    tmp <- ggplot(df_plot, aes(
      x     = category,
      y     = .data[[inst$value_column]],
      group = group_category_id
    )) +
      geom_col(
        position = position_dodge(width = 0.8),
        width    = 0.5
      )
    
    ggplot_build(tmp)$data[[1]]$x
  }
  #' Determines the y-axis maximum based on data and unit type
  get_y_max <- function(
    df_plot,
    inst,
    scale_factor
  ) {
    y_vals <- c(
      df_plot[[inst$value_column]],
      if (!is.null(inst$target)) df_plot[[inst$target]] else numeric()
    ) * scale_factor
    
    y_max_raw <- max(y_vals, na.rm = TRUE)
    
    if (inst$unit == "%") {
      margin <- if (is.null(inst$target)) 0 else 10
      ceiling((y_max_raw + margin) / 10) * 10
    } else {
      ceiling(y_max_raw + 1)
    }
  }
  
  # ------ X AXIS VARIABLE SELECTION -------------------------------------------
  #' Dynamically assign the x-axis variable based on input mode
  x_axis_var <- if (length(inst$metric) == 1 && is.null(inst$category)) {
    "group"
  } else if (length(inst$metric) == 1) {
    inst$category
  } else {
    "metric"
  }
  
  #' Set scale factor (10 for %, otherwise 1)
  scale_factor <- if (inst$unit == "%") 10 else 1
  
  # ------ DATA PREPROCESSING FUNCTION -----------------------------------------
  #' Extracts, filters, and summarizes data for a group
  preprocess_group <- function(
    group_name,
    subset = NULL,
    label = NULL
  ) {
    place_holder <- is.na(group_name)
    
    df_sub <- if (!place_holder) {
      df %>%
        filter(group == group_name) %>%
        {
          if (!is.null(subset) &&
              !is.null(subset$title) &&
              !is.null(subset$value)) {
            filter(., .data[[subset$title]] == subset$value)
          } else {
            .
          }
        } %>%
        filter(metric %in% inst$metric) %>%
        mutate(category = case_when(
          x_axis_var == "metric" ~ metric,
          x_axis_var == "group"  ~ group,
          TRUE                   ~ as.character(.data[[x_axis_var]])
        )) %>%
        group_by(group, category) %>%
        summarise(
          metric_value    = mean(metric_value, na.rm = TRUE),
          munimums_value  = mean(munimums_value, na.rm = TRUE),
          .groups         = "drop"
        )
    } else {
      df_placeholder(label, x_axis_var, inst, df)
    }
    
    if (!is.null(label)) df_sub$group <- label
    df_sub
  }
  
  # ------ FOCAL GROUP ---------------------------------------------------------
  #' Process focal group data
  df_focal <- preprocess_group(
    inst$focal_group$name,
    inst$focal_group$subset,
    label = inst$focal_group$name
  )
  
  # ------ COMPARISON GROUPS ---------------------------------------------------
  #' Process comparison groups (can include multiple subgroups)
  if (!is.null(inst$comparison_groups)) {
    raw_labels <- sapply(inst$comparison_groups, function(cg) {
      if (!is.null(cg$name) && !is.na(cg$name)) {
        if (!is.null(cg$subset)) {
          paste(cg$name, cg$subset$value)
        } else {
          cg$name
        }
      } else {
        " "
      }
    })
    #' Label disambiguation for repeated groups (adds "(1)", "(2)", etc.)
    label_counts <- ave(seq_along(raw_labels), raw_labels, FUN = seq_along)
    
    comparison_labels <- ifelse(
      table(raw_labels)[raw_labels] > 1,
      paste0(raw_labels, " (", label_counts, ")"),
      raw_labels
    )
    
    df_comparisons <- bind_rows(Map(
      function(cg, label) {
        preprocess_group(cg$name, cg$subset, label)
      },
      inst$comparison_groups,
      comparison_labels
    ))
  } else {
    comparison_labels <- character(0)
    df_comparisons    <- NULL
  }
  
  # ------ MERGE & FORMAT DATA -------------------------------------------------
  #' Combine focal and comparison group data
  df_plot <- bind_rows(df_focal, df_comparisons)
  #' Set group factor levels and x-axis categories
  group_labels   <- c(inst$focal_group$name, comparison_labels)
  df_plot$group  <- factor(df_plot$group, levels = group_labels)
  
  if (x_axis_var == "group") {
    df_plot$category <- factor(df_plot$group, levels = group_labels)
  } else {
    df_plot$category <- factor(
      df_plot$category,
      levels = if (!is.null(inst$category_order)) {
        inst$category_order
      } else if (x_axis_var == "metric") {
        inst$metric
      } else {
        sort_category_labels(unique(df_plot$category))
      }
    )
  }
  #' Create ID and fill vars for plotting
  df_plot$group_category_id <- interaction(
    df_plot$group,
    df_plot$category,
    drop = TRUE
  )
  
  df_plot$fill_group <- gsub(" \\(\\d+\\)$", "", df_plot$group)
  
  # ------ COLOR AND LEGEND FILTERING ------------------------------------------
  colors         <- color_map(df_plot)
  df_plot$x_center <- get_x_positions(df_plot, inst)
  y_max          <- get_y_max(df_plot, inst, scale_factor)
  #' Only show non-zero fill groups in legend
  non_zero_groups <- df_plot %>%
    filter(metric_value > 0, !is.na(fill_group)) %>%
    distinct(fill_group) %>%
    pull(fill_group)
  #' If placeholder group has value, include it too
  has_real_comparison <- df_plot %>%
    filter(fill_group == " ", metric_value > 0) %>%
    nrow() > 0
  
  if (has_real_comparison) {
    non_zero_groups <- c(non_zero_groups, " ")
  }
  
  legend_position <- if (
    x_axis_var == "group" || length(non_zero_groups) <= 1
  ) {
    "none"
  } else {
    "bottom"
  }
  #' Filter palette to visible groups
  colors <- colors[names(colors) %in% non_zero_groups]
  
  # ------ BASE PLOT -----------------------------------------------------------
  
  dodge_pos <- position_dodge(width = 0.8)
  bar_width <- 0.5
  
  p <- ggplot(df_plot, aes(
    x     = category,
    y     = .data[[inst$value_column]] * scale_factor,
    fill  = fill_group,
    group = group_category_id
  )) +
    geom_col(position = dodge_pos, width = bar_width) +
    geom_text(
      aes(
        label = ifelse(
          is.na(.data[[inst$value_column]]) |
            .data[[inst$value_column]] == 0,
          "",
          if (inst$unit == "%") {
            paste0(round(.data[[inst$value_column]] * 10), "%")
          } else {
            round(.data[[inst$value_column]], 1)
          }
        ),
        y = .data[[inst$value_column]] * scale_factor / 2
      ),
      position = dodge_pos,
      color    = "black",
      size     = 5,
      fontface = "bold"
    )
  
  # ------ TARGET LINES --------------------------------------------------------
  
  #' Add dashed yellow target line and value label above bar
  if (!is.null(inst$target)) {
    n_bars_per_group <- length(unique(df_plot$category))
    
    target_data <- df_plot %>%
      filter(!is.na(.data[[inst$target]]) & .data[[inst$target]] > 0)
    
    p <- p +
      geom_segment(
        data = target_data,
        aes(
          x    = x_center - bar_width / (nrow(df_plot) / n_bars_per_group * 2),
          xend = x_center + bar_width / (nrow(df_plot) / n_bars_per_group * 2),
          y    = .data[[inst$target]] * scale_factor,
          yend = .data[[inst$target]] * scale_factor
        ),
        color     = "yellow",
        linetype  = "dashed",
        linewidth = 1
      ) +
      geom_text(
        data = target_data,
        aes(
          x     = x_center,
          y     = .data[[inst$target]] * scale_factor +
            if (inst$unit == "%") 6 else 0.5,
          label = if (inst$unit == "%") {
            paste0(round(.data[[inst$target]] * 10), "%")
          } else {
            round(.data[[inst$target]], 1)
          }
        ),
        color    = "yellow",
        size     = 5,
        fontface = "bold"
      )
  }
  
  # ------ TREND LINE ----------------------------------------------------------
  
  #' Adds diagonal trend line between first and last bars (focal group only)
  if (isTRUE(inst$trend_line)) {
    df_trend <- df_plot %>%
      filter(group == inst$focal_group$name) %>%
      arrange(category)
    
    if (nrow(df_trend) >= 2) {
      p <- p +
        geom_segment(
          data = NULL,
          aes(
            x    = df_trend$x_center[1],
            y    = df_trend[[inst$value_column]][1] * scale_factor,
            xend = df_trend$x_center[nrow(df_trend)],
            yend = df_trend[[inst$value_column]][nrow(df_trend)] * scale_factor
          ),
          inherit.aes = FALSE,
          color       = "yellow",
          linewidth   = 1.6
        )
    }
  }
  
  # ------ FINAL STYLING -------------------------------------------------------
  
  p +
    scale_y_continuous(
      limits = c(0, y_max),
      breaks = if (inst$unit == "%") seq(0, y_max, 20) else waiver(),
      labels = if (inst$unit == "%") function(x) paste0(x, "%") else waiver(),
      expand = c(0, 0)
    ) +
    scale_fill_manual(
      values = colors,
      breaks = names(colors)
    ) +
    labs(
      x     = inst$x_title,
      y     = inst$y_title,
      title = " ",
      fill  = NULL
    ) +
    global_theme() +
    theme(
      legend.position = legend_position,
      legend.text     = element_text(
        color = "white",
        size  = 12,
        face  = "bold"
      ),
      legend.title = element_blank()
    )
}
