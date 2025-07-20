# ------ BAR STACKED SLIDE -------------------------------------------------
#' Generate Stacked Bar Chart Slide
#'
#' Creates a grouped or classic stacked bar chart with optional average labels
#' and top/bottom labeling logic. Handles multiple x or y categories, subset 
#' filtering, and flexible theming. Optimized for PowerPoint export.
#'
#' @param data A preprocessed dataframe (e.g., survey or summary data).
#' @param instruction A list of plotting instructions (categories, filters, etc.).
#' @param ppt_doc A PowerPoint object to append slide to (optional).
#' 
#' @return Updated PowerPoint object (if provided), or invisible(NULL).
generate_bar_stacked_slide <- function(
    data,
    instruction,
    ppt_doc = NULL
) {
  # ------ EXTRACT SETTINGS -------------------------------------------------
  category_x <- instruction$category_x
  category_y <- instruction$category_y
  focal_name <- instruction$focal_group$name
  focal_subset <- instruction$focal_group$subset
  metric_var <- instruction$metric %||% NULL
  unit_label <- instruction$unit %||% ""
  chart_title <- instruction$title %||% " "
  
  # ------ VALIDITY CHECK ---------------------------------------------------
  if (length(category_x) > 1 && length(category_y) > 1) {
    stop("Multiple definitions in both category_x and category_y are not supported.")
  }
  
  # ------ DETECT MULTIPLE AXES ---------------------------------------------
  multi_y <- length(category_y) > 1
  multi_x <- length(category_x) > 1
  
  # ------ ADJUST VISUAL PARAMETERS -----------------------------------------
  bar_width <- if (multi_y || multi_x) 0.35 else 0.55
  x_expand <- if (multi_y || multi_x) c(0.3, 0.3) else c(0.1, 0.1)
  plot_margin <- if (multi_y || multi_x) margin(0, 20, 0, 20) else margin(0, 100, 0, 100)
  
  # ------ FILTER TO FOCAL GROUP --------------------------------------------
  df <- data %>%
    filter(group == focal_name)
  
  if (!is.null(focal_subset)) {
    subset_col <- focal_subset$title
    subset_val <- focal_subset$value
    if (!is.null(subset_col) && !is.null(subset_val)) {
      df <- df %>% filter(.data[[subset_col]] %in% subset_val)
    }
  }
  
  # ------ EXTRACT BASE X VARIABLE ------------------------------------------
  base_x_name <- category_x[[1]]$name
  base_x_order <- category_x[[1]]$order
  
  lvl_x <- if (!is.null(base_x_order) && all(c(base_x_name, base_x_order) %in% names(data))) {
    df %>%
      select(x_val = all_of(base_x_name), x_order = all_of(base_x_order)) %>%
      filter(!is.na(x_val), !is.na(x_order)) %>%
      distinct() %>%
      arrange(x_order) %>%
      pull(x_val)
  } else {
    unique(df[[base_x_name]])
  }
  
  df <- df %>%
    rename(x = all_of(base_x_name)) %>%
    filter(x %in% lvl_x) %>%
    mutate(x = factor(x, levels = lvl_x))
  
  # ------ MULTIPLE SUBSET BARS PER X ---------------------------------------
  subset_tabs <- list()
  label_df <- NULL
  
  if (multi_x) {
    for (i in seq_along(category_x)) {
      cx <- category_x[[i]]
      subset_df <- df
      
      if (!is.null(cx$subset)) {
        sub_col <- cx$subset$title
        sub_val <- cx$subset$value
        subset_df <- subset_df %>% filter(.data[[sub_col]] == sub_val)
        bar_label <- sub_val
      } else {
        bar_label <- "All"
      }
      
      cat_y <- category_y[[1]]
      cat_y_name <- cat_y$name
      cat_y_order <- cat_y$order
      cat_y_value <- cat_y$value %||% NULL
      
      lvl_y <- if (!is.null(cat_y_order) && all(c(cat_y_name, cat_y_order) %in% names(data))) {
        df %>%
          select(y_val = all_of(cat_y_name), y_order = all_of(cat_y_order)) %>%
          filter(!is.na(y_val), !is.na(y_order)) %>%
          distinct() %>%
          arrange(y_order) %>%
          pull(y_val)
      } else {
        df %>%
          pull(cat_y_name) %>%
          unique() %>%
          na.omit()
      }
      
      tab <- subset_df %>%
        filter(.data[[cat_y_name]] %in% lvl_y) %>%
        rename(y = all_of(cat_y_name)) %>%
        mutate(
          y = factor(y, levels = lvl_y),
          bar_position = bar_label
        ) %>%
        count(x, bar_position, y) %>%
        group_by(x, bar_position) %>%
        mutate(prop = n / sum(n)) %>%
        ungroup()
      
      if (!is.null(cat_y_value)) {
        tab <- tab %>% filter(y %in% cat_y_value)
      }
      
      subset_tabs[[i]] <- tab
      
      if (!is.null(metric_var) && metric_var %in% names(subset_df)) {
        subset_avg <- subset_df %>%
          group_by(x) %>%
          summarise(value = mean(.data[[metric_var]], na.rm = TRUE), .groups = "drop") %>%
          mutate(
            bar_position = bar_label,
            label = round(value, 1),
            y_pos = 1.13
          )
        label_df <- bind_rows(label_df, subset_avg)
      }
    }
    
    combined_data <- bind_rows(subset_tabs) %>%
      mutate(x = factor(x, levels = lvl_x))
    
    x_map <- tibble(x = factor(lvl_x, levels = lvl_x), x_num = seq_along(lvl_x))
    n_pos <- length(unique(combined_data$bar_position))
    offsets <- seq(-0.22, 0.22, length.out = n_pos)
    offset_map <- tibble(
      bar_position = unique(combined_data$bar_position),
      offset = offsets
    )
    
    combined_data <- combined_data %>%
      left_join(x_map, by = "x") %>%
      left_join(offset_map, by = "bar_position") %>%
      mutate(x_offset = x_num + offset)
    
    if (!is.null(label_df)) {
      label_df <- label_df %>%
        left_join(x_map, by = "x") %>%
        left_join(offset_map, by = "bar_position") %>%
        mutate(x_offset = x_num + offset)
    }
    
    top_bar_labels <- combined_data %>%
      distinct(x, x_num, bar_position, x_offset) %>%
      mutate(label = bar_position)
    
    bottom_group_labels <- x_map %>% mutate(label = as.character(x))
    
  } else {
    all_y_names <- purrr::map_chr(category_y, "name")
    lvl_y_global <- df %>%
      select(any_of(all_y_names)) %>%
      pivot_longer(cols = everything(), values_to = "val", names_to = NULL) %>%
      filter(!is.na(val)) %>%
      count(val) %>%
      arrange(desc(n)) %>%
      pull(val) %>%
      as.character()
    
    all_tabs <- list()
    for (i in seq_along(category_y)) {
      cat_y_info <- category_y[[i]]
      cat_y_name <- cat_y_info$name
      order_y <- cat_y_info$order
      value_y <- cat_y_info$value %||% NULL
      
      label_match <- variable_map$label[variable_map$variable == cat_y_name]
      bar_label <- if (length(label_match) > 0) label_match else paste0("Bar_", i)
      
      lvl_y <- if (!is.null(order_y) && all(c(cat_y_name, order_y) %in% names(data))) {
        df %>%
          select(y_val = all_of(cat_y_name), y_order = all_of(order_y)) %>%
          filter(!is.na(y_val), !is.na(y_order)) %>%
          distinct() %>%
          arrange(y_order) %>%
          pull(y_val)
      } else {
        intersect(lvl_y_global, unique(na.omit(df[[cat_y_name]])))
      }
      
      tab <- df %>%
        filter(.data[[cat_y_name]] %in% lvl_y) %>%
        rename(y = all_of(cat_y_name)) %>%
        mutate(
          y = factor(y, levels = lvl_y),
          bar_position = bar_label
        ) %>%
        count(x, bar_position, y) %>%
        group_by(x, bar_position) %>%
        mutate(prop = n / sum(n)) %>%
        ungroup()
      
      if (!is.null(value_y)) {
        tab <- tab %>% filter(y %in% value_y)
      }
      
      all_tabs[[i]] <- tab
    }
    
    combined_data <- bind_rows(all_tabs) %>%
      mutate(x = factor(x, levels = lvl_x))
    
    x_map <- tibble(x = factor(lvl_x, levels = lvl_x), x_num = seq_along(lvl_x))
    n_pos <- length(unique(combined_data$bar_position))
    offsets <- seq(-0.22, 0.22, length.out = n_pos)
    offset_map <- tibble(
      bar_position = unique(combined_data$bar_position),
      offset = if (multi_y) offsets else rep(0, n_pos)
    )
    
    combined_data <- combined_data %>%
      left_join(x_map, by = "x") %>%
      left_join(offset_map, by = "bar_position") %>%
      mutate(x_offset = x_num + offset)
    
    top_bar_labels <- if (multi_y) {
      combined_data %>%
        distinct(x, x_num, bar_position, x_offset) %>%
        mutate(label = bar_position)
    } else NULL
    
    bottom_group_labels <- x_map %>% mutate(label = as.character(x))
    
    if (!is.null(metric_var) && metric_var %in% names(df)) {
      label_df <- df %>%
        select(x, all_of(metric_var)) %>%
        group_by(x) %>%
        summarise(value = mean(.data[[metric_var]], na.rm = TRUE), .groups = "drop") %>%
        mutate(
          label = round(value, 1),
          y_pos = 1.13
        ) %>%
        left_join(x_map, by = "x")
    }
  }
  
  # ------ FINAL PLOT BUILDING ----------------------------------------------
  y_limit_top  <- if (!is.null(label_df)) max(label_df$y_pos, 1.1) else 1.1
  fill_levels  <- unique(combined_data$y)
  pal          <- get_color_palette(fill_levels)
  
  plot_obj <- ggplot(combined_data) +
    geom_bar(
      aes(x = x_offset, y = prop, fill = y, group = interaction(x, bar_position)),
      stat = "identity",
      width = bar_width
    ) +
    geom_text(
      data = combined_data %>% filter(prop >= 0.03),
      aes(x = x_offset, y = prop, label = scales::percent(prop, accuracy = 1)),
      stat = "identity",
      position = position_stack(vjust = 0.5),
      color = "black",
      fontface = "bold",
      size = 5
    ) +
    {
      if (!is.null(top_bar_labels)) {
        geom_text(
          data = top_bar_labels,
          aes(x = x_offset, y = 1.05, label = label),
          inherit.aes = FALSE,
          size = 4,
          fontface = "bold",
          color = "white"
        )
      }
    } +
    {
      if (!is.null(label_df)) {
        list(
          geom_text(
            data = label_df,
            aes(
              x = .data[[if ("x_offset" %in% names(label_df)) "x_offset" else "x_num"]],
              y = y_pos,
              label = label
            ),
            inherit.aes = FALSE,
            color = "yellow",
            fontface = "bold",
            size = 7
          ),
          annotate(
            "text",
            x = 0.55,
            y = label_df$y_pos[1],
            label = paste0("Avg ", unit_label, ":"),
            hjust = 1,
            color = "yellow",
            fontface = "bold",
            size = 7
          )
        )
      }
    } +
    geom_hline(yintercept = 0, color = "white", linewidth = 1.5) +
    geom_text(
      data = bottom_group_labels,
      aes(x = x_num, y = -0.07, label = label),
      inherit.aes = FALSE,
      size = 7,
      fontface = "bold",
      color = "white"
    ) +
    scale_x_continuous(
      breaks = x_map$x_num,
      labels = rep("", length(x_map$x_num)),
      expand = expansion(add = x_expand)
    ) +
    scale_y_continuous(
      limits = c(-0.1, y_limit_top),
      breaks = NULL,
      labels = NULL,
      expand = expansion(mult = c(0, 0.2))
    ) +
    scale_fill_manual(values = pal, drop = FALSE) +
    labs(x = NULL, y = NULL, fill = NULL) +
    global_theme() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = plot_margin,
      legend.position = if (length(fill_levels) <= 1) "none" else "bottom",
      legend.direction = "horizontal",
      legend.text = element_text(color = "white", size = 14, face = "bold"),
      legend.key.height = unit(12, "pt"),
      legend.key.width = unit(12, "pt"),
      legend.title = element_blank()
    ) +
    coord_cartesian(clip = "off")
  
  # ------ EXPORT TO SLIDE (IF NEEDED) --------------------------------------
  if (!is.null(ppt_doc)) {
    ppt_doc <- export_plot_to_slide(
      ppt_doc = ppt_doc,
      plot_obj = plot_obj,
      title_text = chart_title,
      is_first = instruction$is_first
    )
    return(ppt_doc)
  }
  
  return(invisible(NULL))}

