#' Generate Donut Slide
#'
#' This function creates a donut-style chart slide for a focal group,
#' optionally comparing multiple groups to the focal group. If `ppt_doc` is 
#' provided, the function appends the slide to the PowerPoint object.
#' 
#' The function supports:
#' - placeholder groups to preserve layout
#' - vertical white separators between groups
#' - optional ordering of categories
#'
#' @param data Data frame with numeric values for hours and subjective scores.
#' @param instruction A list with plot settings.
#' @param ppt_doc Optional PowerPoint object
#'
#' @return Updated pptx object if exporting, otherwise the ggplot object.

generate_donut_slide <- function(
    data,
    instruction,
    ppt_doc
) {
  # ------ EXTRACT INSTRUCTION SETTINGS ---------------------------------------
  category_var <- instruction$category$name
  order_var <- instruction$category$order %||% NULL
  chart_title <- instruction$title %||% " "
  focal_group <- instruction$focal_group
  focal_name <- focal_group$name
  fg_subset_col <- focal_group$subset$title %||% NULL
  fg_subset_val <- focal_group$subset$value %||% NULL
  comp_groups <- instruction$comparison_groups %||% list()
  has_comparisons <- length(comp_groups) > 0
  
  # ------ EARLY VALIDATION ----------------------------------------------------
  # Collect all required column names
  required_cols <- c(category_var)
  
  if (!is.null(order_var)) {
    required_cols <- c(required_cols, order_var)
  }
  
  if (!is.null(fg_subset_col)) {
    required_cols <- c(required_cols, fg_subset_col)
  }
  
  for (cg in comp_groups) {
    if (is.list(cg) && !is.null(cg$subset) && !is.null(cg$subset$title)) {
      required_cols <- c(required_cols, cg$subset$title)
    }
  }
  
  # Determine which required columns are missing
  missing_cols <- setdiff(unique(required_cols), names(data))
  if (length(missing_cols) > 0) {
    message("❌ Missing column(s): ", paste(missing_cols, collapse = ", "), ". Slide skipped.")
    return(NULL)
  }
  
  # ------ COLOR PALETTE -----------------------------------------------------
  # Define color palette to assign to donut slices
  palette_colors <- c(
    "#fe62c1", "#652cd2", "#9f55e6", "#03cdfe",
    "#1aa2ff", "#d47edd", "#7492dd"
  )
  
  # ------ FILTER FOCAL GROUP ------------------------------------------------
  # Filter and label focal group (and optional subset)
  data_focal <- data %>% filter(group == focal_name)
  
  if (!is.null(fg_subset_col) && fg_subset_col %in% names(data)) {
    data_focal <- data_focal %>%
      filter(.data[[fg_subset_col]] %in% fg_subset_val)
    data_focal$group <- if (length(fg_subset_val) == 1) {
      paste(focal_name, fg_subset_val)
    } else {
      focal_name
    }
  }
  
  data_focal <- data_focal %>%
    filter(!is.na(.data[[category_var]])) %>%
    mutate(
      group_label = group %||% focal_name,
      is_placeholder = FALSE
    )
  
  # ------ CATEGORY LEVELS ---------------------------------------------------
  # Store all possible category values for placeholder compatibility
  category_levels <- unique(na.omit(data[[category_var]]))
  
  # ------ FILTER COMPARISON GROUPS ------------------------------------------
  # Filter and prepare comparison groups (or create placeholders)
  comparison_data_list <- lapply(seq_along(comp_groups), function(i) {
    cg <- comp_groups[[i]]
    
    if (!is.list(cg) || is.null(cg$name) || is.na(cg$name)) {
      dummy <- data.frame(
        group = paste("Group", i),
        group_label = paste("Group", i),
        count = 0,
        is_placeholder = TRUE,
        stringsAsFactors = FALSE
      )
      dummy[[category_var]] <- factor(category_levels[1], levels = category_levels)
      return(dummy)
    }
    
    cg_name <- cg$name
    subset_col <- cg$subset$title %||% NULL
    subset_val <- cg$subset$value %||% NULL
    
    data_comp <- data %>% filter(group == cg_name)
    
    if (!is.null(subset_col) && subset_col %in% names(data)) {
      data_comp <- data_comp %>%
        filter(.data[[subset_col]] %in% subset_val)
      data_comp$group <- if (length(subset_val) == 1) {
        paste(cg_name, subset_val)
      } else {
        cg_name
      }
    }
    
    data_comp <- data_comp %>% filter(!is.na(.data[[category_var]]))
    
    if (nrow(data_comp) == 0) {
      dummy <- data.frame(
        group = paste("Group", i),
        group_label = paste("Group", i),
        count = 0,
        is_placeholder = TRUE,
        stringsAsFactors = FALSE
      )
      dummy[[category_var]] <- factor(category_levels[1], levels = category_levels)
      return(dummy)
    }
    
    data_comp %>%
      mutate(
        group_label = group %||% cg_name,
        is_placeholder = FALSE
      )
  })
  
  # ------ COMBINE DATA ------------------------------------------------------
  # Merge focal and comparison group data
  combined_data <- bind_rows(data_focal, comparison_data_list)
  
  if (!is.null(order_var)) {
    level_order <- data %>%
      select(all_of(category_var), all_of(order_var)) %>%
      filter(!is.na(.data[[order_var]])) %>%
      distinct() %>%
      arrange(.data[[order_var]]) %>%
      pull(.data[[category_var]]) %>%
      unique()
    combined_data[[category_var]] <- factor(combined_data[[category_var]], levels = level_order)
  } else {
    combined_data[[category_var]] <- factor(combined_data[[category_var]])
  }
  
  combined_data$group_label <- factor(
    combined_data$group_label,
    levels = unique(combined_data$group_label)
  )
  
  # ------ AGGREGATE FOR PLOTTING --------------------------------------------
  # Aggregate data to compute counts and percentages per group/category
  summary_df <- combined_data %>%
    count(group_label, .data[[category_var]], is_placeholder, name = "count") %>%
    group_by(group_label) %>%
    mutate(
      percent = count / sum(count),
      percent_label = paste0(percent(percent, accuracy = 1))
    ) %>%
    ungroup()
  
  palette_final <- palette_colors[
    seq_len(min(length(levels(summary_df[[category_var]])), length(palette_colors)))
  ]
  names(palette_final) <- levels(summary_df[[category_var]])
  
  # ------ SINGLE DONUT CHART FUNCTION -----------------------------------------
  # Function to generate an individual donut chart
  plot_single_donut <- function(df, group_title, show_legend = TRUE) {
    ggplot(df, aes(x = 2, y = percent, fill = .data[[category_var]])) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      xlim(0.5, 2.5) +
      geom_text(
        aes(label = percent_label),
        position = position_stack(vjust = 0.5),
        size = 6,
        color = "white",
        fontface = "bold"
      ) +
      scale_fill_manual(values = palette_final) +
      ggtitle(group_title) +
      theme_void() +
      theme(
        plot.title = element_text(size = 24, face = "bold", color = "white", hjust = 0.5, margin = margin(b = 12)),
        legend.position = if (show_legend) "bottom" else "none",
        legend.text = element_text(color = "white", size = 16, face = "bold", margin = margin(l = 6, t = 5)),
        legend.key.size = unit(1.2, "lines"),
        legend.title = element_blank(),
        legend.box = "horizontal",
        legend.box.just = "center",
        legend.spacing.x = unit(0.8, "cm"),
        legend.margin = margin(t = 5, b = 0)
      ) +
      guides(fill = guide_legend(nrow = 1, byrow = TRUE, label.position = "right"))
  }
  
  # ------ VERTICAL SEPARATOR FUNCTION ---------------------------------------
  # Function to create a vertical white line separator between donuts
  separator_plot <- function() {
    ggplot() +
      geom_segment(aes(x = 0.5, xend = 0.5, y = 0, yend = 1), color = "white", linewidth = 2) +
      xlim(0, 1) + ylim(0, 1) +
      theme_void()
  }
  
  # ------ BUILD DONUT + SEPARATOR COMPONENTS ----------------------------------
  # Loop through each group and build donut/separator elements
  components <- list()
  group_levels <- levels(summary_df$group_label)
  
  for (i in seq_along(group_levels)) {
    group_data <- summary_df %>%
      filter(group_label == group_levels[i]) %>%
      arrange(desc(.data[[category_var]])) %>%
      mutate(
        ymax = cumsum(percent),
        ymin = c(0, head(ymax, -1)),
        mid = (ymin + ymax) / 2
      )
    
    is_placeholder <- unique(group_data$is_placeholder)
    
    if (isTRUE(is_placeholder)) {
      components[[length(components) + 1]] <- ggplot() + theme_void()
    } else {
      show_title <- has_comparisons
      components[[length(components) + 1]] <- plot_single_donut(
        group_data,
        group_title = if (show_title) group_levels[i] else "",
        show_legend = FALSE
      )
    }
    
    if (i < length(group_levels)) {
      components[[length(components) + 1]] <- separator_plot()
    }
  }
  
  # ------ LEGEND (BOTTOM ROW) -----------------------------------------------
  # Add a shared legend (only for multi-group layout) from the first real group
  first_valid_group <- summary_df %>%
    filter(!is_placeholder) %>%
    distinct(group_label) %>%
    pull(group_label) %>%
    .[1]
  
  legend_plot <- plot_single_donut(
    summary_df %>% filter(group_label == first_valid_group),
    group_title = first_valid_group,
    show_legend = TRUE
  )
  
  legend <- plot_grid(get_legend(legend_plot))
  
  # ------ COMBINE FINAL PLOT ------------------------------------------------
  # Combine donut components and legend using patchwork layout
  plot_obj <- wrap_plots(
    components,
    nrow = 1,
    widths = rep(c(1, 0.01), length.out = length(components))[1:length(components)]
  ) /
    legend +
    plot_layout(heights = c(1, 0.08)) +
    plot_annotation(
      title = " ",
      theme = theme(plot.title = element_text(size = 20, face = "bold", color = "white", hjust = 0.5))
    ) & theme(
      plot.background = element_rect(fill = "#00587A", color = NA),
      panel.background = element_rect(fill = "#00587A", color = NA)
    )
  
  # ------ EXPORT TO POWERPOINT (OPTIONAL) -----------------------------------
  # Export the plot to a slide if ppt_doc is provided
  if (!is.null(ppt_doc)) {
    ppt_doc <- export_plot_to_slide(
      ppt_doc = ppt_doc,
      plot_obj = plot_obj,
      title_text = chart_title,
      is_first = instruction$is_first
    )
    return(ppt_doc)
  }
  
  return(invisible(NULL))
}
