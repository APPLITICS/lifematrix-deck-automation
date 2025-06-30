#' Draws a customized bar chart for focal and comparison groups based on provided
#' instructionructions.
#'
#' This function supports grouped bar charts using ggplot2, and includes:
#' - Preparation of summarized input data from raw values (e.g., by group, metric)
#' - Custom x-axis modes that allow switching between displaying group, metric,
#'   or category on the x-axis
#' - Placeholder bar reservation for missing comparison groups to preserve layout
#' - Optional dashed target lines and trend lines
#' - Automatic color assignment and label formatting
#'
#' @param data   Data frame containing the input data.
#' @param instruction List containing the chart configuration, including:
#'   metric(s), group definitions, titles, units, and options for target/trend lines.

draw_bar_chart <- function(data, instruction) {
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
  data_placeholder <- function(
    label,
    x_axis_var,
    instruction,
    data
  ) {
    categories <- if (x_axis_var == "metric") {
      instruction$metric
    } else {
      unique(data[[x_axis_var]])
    }
    
    data.frame(
      group            = if (!is.null(label)) label else " ",
      category         = categories,
      metric_value     = 0,
      minimums_value   = 0,
      stringsAsFactors = FALSE
    )
  }
  #' Maps consistent fill colors to groups for visual distinction
  color_map <- function(data_plot) {
    unique_fill_groups   <- unique(data_plot$fill_group)
    group_colors_palette <- c("#70e2ff", "#a4d65e", "#2dc595", "#5d8c90")
    
    setNames(
      group_colors_palette[seq_along(unique_fill_groups)],
      unique_fill_groups
    )
  }
  #' Gets bar center x-positions for label/text alignment and target line placement
  get_x_positions <- function(
    data_plot,
    instruction
  ) {
    tmp <- ggplot(data_plot, aes(
      x     = category,
      y     = .data[[instruction$current_value]],
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
    data_plot,
    instruction,
    scale_factor
  ) {
    y_vals <- c(
      data_plot[[instruction$current_value]],
      if (!is.null(instruction$target)) data_plot[[instruction$target]] else numeric()
    ) * scale_factor
    
    y_max_raw <- max(y_vals, na.rm = TRUE)
    
    if (instruction$unit == "%") {
      margin <- if (is.null(instruction$target)) 0 else 10
      ceiling((y_max_raw + margin) / 10) * 10
    } else {
      ceiling(y_max_raw + 1)
    }
  }
  
  # ------ X AXIS VARIABLE SELECTION -------------------------------------------
  #' Dynamically assign the x-axis variable based on input mode
  x_axis_var <- if (length(instruction$metric) == 1 && is.null(instruction$category)) {
    "group"
  } else if (length(instruction$metric) == 1) {
    instruction$category
  } else {
    "metric"
  }
  
  #' Set scale factor (10 for %, otherwise 1)
  scale_factor <- if (instruction$unit == "%") 10 else 1
  
  # ------ DATA PREPROCESSING FUNCTION -----------------------------------------
  #' Extracts, filters, and summarizes data for a group
  preprocess_group <- function(
    group_name,
    subset = NULL,
    label = NULL
  ) {
    place_holder <- is.na(group_name)
    
    data_sub <- if (!place_holder) {
      data %>%
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
        filter(metric %in% instruction$metric) %>%
        mutate(category = case_when(
          x_axis_var == "metric" ~ metric,
          x_axis_var == "group"  ~ group,
          TRUE                   ~ as.character(.data[[x_axis_var]])
        )) %>%
        group_by(group, category) %>%
        summarise(
          metric_value    = mean(metric_value, na.rm = TRUE),
          minimums_value  = mean(minimums_value, na.rm = TRUE),
          .groups         = "drop"
        )
    } else {
      data_placeholder(label, x_axis_var, instruction, data)
    }
    
    if (!is.null(label)) data_sub$group <- label
    data_sub
  }
  
  # ------ FOCAL GROUP ---------------------------------------------------------
  #' Process focal group data
  data_focal <- preprocess_group(
    instruction$focal_group$name,
    instruction$focal_group$subset,
    label = instruction$focal_group$name
  )
  
  # ------ COMPARISON GROUPS ---------------------------------------------------
  #' Process comparison groups (can include multiple subgroups)
  if (!is.null(instruction$comparison_groups)) {
    raw_labels <- sapply(instruction$comparison_groups, function(cg) {
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
    
    data_comparisons <- bind_rows(Map(
      function(cg, label) {
        preprocess_group(cg$name, cg$subset, label)
      },
      instruction$comparison_groups,
      comparison_labels
    ))
  } else {
    comparison_labels <- character(0)
    data_comparisons    <- NULL
  }
  
  # ------ MERGE & FORMAT DATA -------------------------------------------------
  #' Combine focal and comparison group data
  data_plot <- bind_rows(data_focal, data_comparisons)
  #' Set group factor levels and x-axis categories
  group_labels   <- c(instruction$focal_group$name, comparison_labels)
  data_plot$group  <- factor(data_plot$group, levels = group_labels)
  
  if (x_axis_var == "group") {
    data_plot$category <- factor(data_plot$group, levels = group_labels)
  } else {
    data_plot$category <- factor(
      data_plot$category,
      levels = if (!is.null(instruction$category_order)) {
        instruction$category_order
      } else if (x_axis_var == "metric") {
        instruction$metric
      } else {
        sort_category_labels(unique(data_plot$category))
      }
    )
  }
  #' Create ID and fill vars for plotting
  data_plot$group_category_id <- interaction(
    data_plot$group,
    data_plot$category,
    drop = TRUE
  )
  
  data_plot$fill_group <- gsub(" \\(\\d+\\)$", "", data_plot$group)
  
  # ------ COLOR AND LEGEND FILTERING ------------------------------------------
  colors         <- color_map(data_plot)
  data_plot$x_center <- get_x_positions(data_plot, instruction)
  y_max          <- get_y_max(data_plot, instruction, scale_factor)
  #' Only show non-zero fill groups in legend
  non_zero_groups <- data_plot %>%
    filter(metric_value > 0, !is.na(fill_group)) %>%
    distinct(fill_group) %>%
    pull(fill_group)
  #' If placeholder group has value, include it too
  has_real_comparison <- data_plot %>%
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
  
  p <- ggplot(data_plot, aes(
    x     = category,
    y     = .data[[instruction$current_value]] * scale_factor,
    fill  = fill_group,
    group = group_category_id
  )) +
    geom_col(position = dodge_pos, width = bar_width) +
    geom_text(
      aes(
        label = ifelse(
          is.na(.data[[instruction$current_value]]) |
            .data[[instruction$current_value]] == 0,
          "",
          if (instruction$unit == "%") {
            paste0(round(.data[[instruction$current_value]] * 10), "%")
          } else {
            round(.data[[instruction$current_value]], 1)
          }
        ),
        y = .data[[instruction$current_value]] * scale_factor / 2
      ),
      position = dodge_pos,
      color    = "black",
      size     = 5,
      fontface = "bold"
    )
  
  # ------ TARGET LINES --------------------------------------------------------
  
  #' Add dashed yellow target line and value label above bar
  if (!is.null(instruction$target)) {
    n_bars_per_group <- length(unique(data_plot$category))
    
    target_data <- data_plot %>%
      filter(!is.na(.data[[instruction$target]]) & .data[[instruction$target]] > 0)
    
    p <- p +
      geom_segment(
        data = target_data,
        aes(
          x    = x_center - bar_width / (nrow(data_plot) / n_bars_per_group * 2),
          xend = x_center + bar_width / (nrow(data_plot) / n_bars_per_group * 2),
          y    = .data[[instruction$target]] * scale_factor,
          yend = .data[[instruction$target]] * scale_factor
        ),
        color     = "yellow",
        linetype  = "dashed",
        linewidth = 1
      ) +
      geom_text(
        data = target_data,
        aes(
          x     = x_center,
          y     = .data[[instruction$target]] * scale_factor +
            if (instruction$unit == "%") 6 else 0.5,
          label = if (instruction$unit == "%") {
            paste0(round(.data[[instruction$target]] * 10), "%")
          } else {
            round(.data[[instruction$target]], 1)
          }
        ),
        color    = "yellow",
        size     = 5,
        fontface = "bold"
      )
  }
  
  # ------ TREND LINE ----------------------------------------------------------
  
  #' Adds diagonal trend line between first and last bars (focal group only)
  if (isTRUE(instruction$trend_line)) {
    data_trend <- data_plot %>%
      filter(group == instruction$focal_group$name) %>%
      arrange(category)
    
    if (nrow(data_trend) >= 2) {
      p <- p +
        geom_segment(
          data = NULL,
          aes(
            x    = data_trend$x_center[1],
            y    = data_trend[[instruction$current_value]][1] * scale_factor,
            xend = data_trend$x_center[nrow(data_trend)],
            yend = data_trend[[instruction$current_value]][nrow(data_trend)] * scale_factor
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
      breaks = if (instruction$unit == "%") seq(0, y_max, 20) else waiver(),
      labels = if (instruction$unit == "%") function(x) paste0(x, "%") else waiver(),
      expand = c(0, 0)
    ) +
    scale_fill_manual(
      values = colors,
      breaks = names(colors)
    ) +
    labs(
      x     = instruction$x_title,
      y     = instruction$y_title,
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
