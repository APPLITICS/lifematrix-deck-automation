# ------ BAR STACKED VERTICAL SLIDE -------------------------------------------------
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
#' @return Updated PowerPoint object (`ppt_doc`) and density plot (`plot_obj`).

generate_stacked_vertical_slide <- function(
    data,
    instruction,
    ppt_doc = NULL
) {
  # ------ SPACING PARAMETERS --------------------------------------------------
  intra_group_offset <- 0.2
  inter_group_expand <- 0.35
  
  # ------ EXTRACT SETTINGS ----------------------------------------------------
  category_x <- instruction$category_x
  category_y <- instruction$category_y
  focal_name <- instruction$focal_group$name
  focal_subset <- instruction$focal_group$subset
  metric_var <- instruction$metric %||% NULL
  unit_label <- instruction$unit %||% ""
  chart_title <- instruction$title %||% " "
  
  # ------ EARLY VALIDATION ----------------------------------------------------
  category_x_names <- unlist(lapply(
    category_x,
    function(x) c(x$name, x$order %||% NULL, x$subset$title %||% NULL)
  ))
  category_y_names <- unlist(lapply(
    category_y,
    function(y) c(y$name, y$order %||% NULL)
  ))
  subset_col <- focal_subset$title %||% NULL
  
  required_cols <- unique(na.omit(c(
    category_x_names, category_y_names, subset_col, metric_var
  )))
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    message(
      "❌ Missing column(s): ",
      paste(missing_cols, collapse = ", "),
      ". Slide skipped."
    )
    return(NULL)
  }
  if (length(category_x) > 1 && length(category_y) > 1) {
    stop("Multiple definitions in both category_x and category_y are not supported.")
  }

  # ------ ENSURE VARIABLE MAP HAS NEEDED VARS ---------------------------------
  needed_vars <- unique(na.omit(purrr::map_chr(category_y, "name")))
  if (!exists("variable_map")) {
    variable_map <- tibble(variable = character(), label = character())
  }
  missing_vars <- setdiff(needed_vars, variable_map$variable)
  if (length(missing_vars) > 0) {
    variable_map <- bind_rows(
      variable_map,
      tibble(variable = missing_vars, label = missing_vars)
    ) %>% distinct(variable, .keep_all = TRUE)
  }

  # ------ DETECT MULTI-AXIS CASES ---------------------------------------------
  multi_y <- length(category_y) > 1
  multi_x <- length(category_x) > 1

  # ------ VISUAL PARAMETERS ---------------------------------------------------
  bar_width <- if (multi_y || multi_x) 0.35 else 0.5
  x_expand <- if (multi_y || multi_x) {
    rep(inter_group_expand, 2)
  } else {
    c(0.1, 0.1)
  }
  plot_margin <- if (multi_y || multi_x) {
    margin(0, 20, 10, 20)
  } else {
    margin(0, 100, 10, 100)
  }

  # ------ FILTER TO FOCAL GROUP -----------------------------------------------
  df <- data %>% filter(group == focal_name)
  if (!is.null(focal_subset)) {
    subset_col <- focal_subset$title
    subset_val <- focal_subset$value
    if (!is.null(subset_col) && !is.null(subset_val)) {
      df <- df %>% filter(.data[[subset_col]] %in% subset_val)
    }
  }

  # ------ DROP NAs IN KEY VARS ------------------------------------------------
  key_vars <- c(
    category_x[[1]]$name,
    purrr::map_chr(category_y, "name"),
    metric_var
  )
  key_vars <- intersect(key_vars, names(df))
  df <- df %>% filter(if_all(all_of(key_vars), ~ !is.na(.)))
  
  # ------ EXTRACT BASE X ------------------------------------------------------
  base_x_name <- category_x[[1]]$name
  base_x_order <- category_x[[1]]$order
  base_x_value <- category_x[[1]]$value %||% NULL
  
  if (!is.null(base_x_value)) {
    df <- df %>% filter(.data[[base_x_name]] %in% base_x_value)
  }
  lvl_x <- if (!is.null(base_x_order) &&
               all(c(base_x_name, base_x_order) %in% names(df))) {
    df %>%
      select(
        x_val = all_of(base_x_name),
        x_order = all_of(base_x_order)
      ) %>%
      filter(!is.na(x_val), !is.na(x_order)) %>%
      distinct() %>%
      arrange(x_order) %>%
      pull(x_val)
  } else {
    unique(df[[base_x_name]])
  }
  
  # ------ PREPARE X + SUBSET BARS ---------------------------------------------
  df <- df %>% rename(x = all_of(base_x_name))
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
      cy_name <- cat_y$name
      cy_order <- cat_y$order
      cy_value <- cat_y$value %||% NULL
      
      lvl_y <- if (!is.null(cy_order) &&
                   all(c(cy_name, cy_order) %in% names(df))) {
        df %>%
          select(
            y_val = all_of(cy_name),
            y_order = all_of(cy_order)
          ) %>%
          filter(!is.na(y_val), !is.na(y_order)) %>%
          distinct() %>%
          arrange(y_order) %>%
          pull(y_val)
      } else {
        df %>%
          pull(all_of(cy_name)) %>%
          unique() %>%
          na.omit()
      }
      
      totals <- subset_df %>%
        count(x, bar_position = bar_label, name = "total_n")
      
      tab <- subset_df %>%
        { if (!is.null(cy_value)) {
          filter(., .data[[cy_name]] %in% cy_value)
        } else . } %>%
        filter(.data[[cy_name]] %in% lvl_y) %>%
        rename(y = all_of(cy_name)) %>%
        mutate(
          y = factor(y, levels = lvl_y),
          bar_position = bar_label
        ) %>%
        count(x, bar_position, y, name = "n") %>%
        left_join(totals, by = c("x", "bar_position")) %>%
        mutate(prop = n / total_n) %>%
        select(-n, -total_n)
      
      subset_tabs[[i]] <- tab
    }
    
    combined_data <- bind_rows(subset_tabs) %>%
      mutate(x = factor(x, levels = lvl_x))
    
    x_map <- tibble(
      x = factor(lvl_x, levels = lvl_x),
      x_num = seq_along(lvl_x)
    )
    n_pos <- length(unique(combined_data$bar_position))
    offsets <- seq(-intra_group_offset, intra_group_offset, length.out = n_pos)
    offset_map <- tibble(
      bar_position = unique(combined_data$bar_position),
      offset = offsets
    )
    combined_data <- combined_data %>%
      left_join(x_map, by = "x") %>%
      left_join(offset_map, by = "bar_position") %>%
      mutate(x_offset = x_num + offset)
    
    top_bar_labels <- combined_data %>%
      distinct(x, x_num, bar_position, x_offset) %>%
      mutate(label = bar_position)
    
    bottom_group_labels <- x_map %>%
      mutate(label = style_ordinal_suffix(as.character(x)))
    
  } else {
    all_tabs <- list()
    for (i in seq_along(category_y)) {
      cy <- category_y[[i]]
      cy_name <- cy$name
      order_y <- cy$order
      value_y <- cy$value %||% NULL
      
      label_match <- variable_map$label[
        variable_map$variable == cy_name
      ]
      bar_label <- if (length(label_match) > 0) {
        label_match
      } else {
        paste0("Bar_", i)
      }
      
      lvl_y <- if (!is.null(order_y) &&
                   all(c(cy_name, order_y) %in% names(df))) {
        df %>%
          select(
            y_val = all_of(cy_name),
            y_order = all_of(order_y)
          ) %>%
          filter(!is.na(y_val), !is.na(y_order)) %>%
          distinct() %>%
          arrange(y_order) %>%
          pull(y_val)
      } else {
        df %>%
          pull(all_of(cy_name)) %>%
          unique() %>%
          na.omit()
      }
      
      totals <- df %>%
        count(x, bar_position = bar_label, name = "total_n")
      
      tab <- df %>%
        { if (!is.null(value_y)) {
          filter(., .data[[cy_name]] %in% value_y)
        } else . } %>%
        filter(.data[[cy_name]] %in% lvl_y) %>%
        rename(y = all_of(cy_name)) %>%
        mutate(
          y = factor(y, levels = lvl_y),
          bar_position = bar_label
        ) %>%
        count(x, bar_position, y, name = "n") %>%
        left_join(totals, by = c("x", "bar_position")) %>%
        mutate(prop = n / total_n) %>%
        select(-n, -total_n)
      
      all_tabs[[i]] <- tab
    }
    
    combined_data <- bind_rows(all_tabs) %>%
      mutate(x = factor(x, levels = lvl_x))
    
    x_map <- tibble(
      x = factor(lvl_x, levels = lvl_x),
      x_num = seq_along(lvl_x)
    )
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
    } else {
      NULL
    }
    
    bottom_group_labels <- x_map %>%
      mutate(label = style_ordinal_suffix(as.character(x)))
  }
  
  # ------ RESCALE BY VISIBLE ONLY ---------------------------------------------
  bar_totals <- combined_data %>%
    group_by(x, bar_position) %>%
    summarise(
      visible_total = sum(prop, na.rm = TRUE),
      .groups = "drop"
    )
  max_visible <- max(bar_totals$visible_total, na.rm = TRUE)
  scale_factor <- if (is.finite(max_visible) && max_visible > 0) 1 / max_visible else 1
  
  combined_data <- combined_data %>%
    left_join(bar_totals, by = c("x", "bar_position")) %>%
    mutate(prop_rescaled = prop * scale_factor) %>%
    select(-visible_total)
  
  # ------ LABEL Y POSITIONS ---------------------------------------------------
  max_bar_height <- combined_data %>%
    group_by(x, bar_position) %>%
    summarise(
      total = sum(prop_rescaled, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pull(total) %>%
    max(na.rm = TRUE)
  
  subset_label_y <- max_bar_height + 0.025
  avg_value_label_y <- subset_label_y + 0.05
  
  # ------ OPTIONAL: NUMERIC AVERAGE LABELS ------------------------------------
  if (!is.null(metric_var) && metric_var %in% names(df)) {
    label_df <- df %>%
      select(x, all_of(metric_var)) %>%
      group_by(x) %>%
      summarise(
        value = mean(.data[[metric_var]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(label = sprintf("%.1f", value)) %>%
      left_join(x_map, by = "x")
  }
  
  # ------ PLOT BUILD ----------------------------------------------------------
  fill_levels <- unique(combined_data$y)
  pal <- get_color_palette(fill_levels)
  
  plot_obj <- ggplot(combined_data) +
    geom_bar(
      aes(
        x = x_offset,
        y = prop_rescaled,
        fill = y,
        group = interaction(x, bar_position)
      ),
      stat = "identity",
      width = bar_width
    ) +
    geom_text(
      data = combined_data %>% filter(prop >= 0.03),
      aes(
        x = x_offset,
        y = prop_rescaled,
        label = percent(prop, accuracy = 1)
      ),
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
          aes(
            x = x_offset,
            y = subset_label_y * 1.05,
            label = label
          ),
          inherit.aes = FALSE,
          size = 4,
          fontface = "bold",
          color = "white",
          vjust = 0
        )
      }
    } +
    {
      if (!is.null(label_df)) {
        list(
          geom_text(
            data = label_df,
            aes(
              x = .data[[if ("x_offset" %in% names(label_df)) {
                "x_offset"
              } else {
                "x_num"
              }]],
              y = avg_value_label_y * 1.1,
              label = label
            ),
            inherit.aes = FALSE,
            color = "yellow",
            fontface = "bold",
            size = 7,
            vjust = 0
          ),
          annotate(
            "text",
            x = min(x_map$x_num) - 0.4,
            y = avg_value_label_y * 1.1,
            label = paste0("Avg ", unit_label %||% "", ":"),
            hjust = 1,
            vjust = 0,
            color = "yellow",
            fontface = "bold",
            size = 7
          )
        )
      }
    } +
    geom_hline(yintercept = 0, color = "white", linewidth = 1.5) +
    geom_richtext(
      data = bottom_group_labels,
      aes(x = x_num, y = -0.07, label = label),
      fill = NA,
      label.color = NA,
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
      breaks = NULL,
      labels = NULL,
      expand = expansion(mult = c(0, 0.2))
    ) +
    scale_fill_manual(values = pal, drop = FALSE) +
    labs(x = NULL, y = NULL, fill = NULL) +
    global_theme() +
    theme(
      axis.text.x = element_markdown(),
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
      legend.text = element_text(
        color = "white",
        size = 14,
        face = "bold"
      ),
      legend.key.height = unit(12, "pt"),
      legend.key.width = unit(12, "pt"),
      legend.title = element_blank(),
      legend.box.margin = margin(t = 5)
    ) +
    coord_cartesian(clip = "off")
  
  # ------ EXPORT TO SLIDE -----------------------------------------------------
  if (!is.null(ppt_doc)) {
    ppt_doc <- export_plot_to_slide(
      ppt_doc = ppt_doc,
      plot_obj = plot_obj,
      title_text = chart_title,
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
