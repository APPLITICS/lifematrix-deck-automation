# ------ EXPORT: DENSITY SLIDE -------------------------------------------------

#' Generate a density plot and export to PowerPoint
#'
#' Creates a smoothed density chart for a focal group, with optional comparison
#' labels. Adds a vertical line for the average and auto-scales axes.
#'
#' @param data A data frame with numeric values and group labels.
#' @param instruction A list with chart options (group info, metric, etc.).
#' @param ppt_doc Optional `read_pptx()` object for export.
#'
#' @return Updated pptx object if `ppt_doc` is given; otherwise, `NULL`.
generate_density_slide <- function(
    data,
    instruction,
    ppt_doc
) {
  # ------ HELPERS -------------------------------------------------------------
  # Define custom axis step functions and comparison label drawer
  get_y_step <- function(y_max) {
    target_lines <- 5
    raw_step     <- y_max / (target_lines - 1)
    nice_steps   <- c(0.01, 0.02, 0.05, 0.1, 0.2, 0.25, 0.5,
                      1, 2, 5, 10, 20, 50, 100)
    step <- nice_steps[which.min(abs(nice_steps - raw_step))]
    return(step)
  }
  
  get_x_step <- function(x_range) {
    if (x_range <= 12) return(1)
    for (step in c(10, 5, 2)) {
      n_breaks <- x_range / step
      if (n_breaks >= 4 && n_breaks <= 8) return(step)
    }
    return(1)
  }
  
  draw_comparison_label <- function(comparison_data_list, instruction) {
    if (is.null(instruction$comparison_groups)) return(NULL)
    
    metric_col <- instruction$metric
    labels <- mapply(
      function(data_comp, cg) {
        avg <- round(mean(data_comp[[metric_col]], na.rm = TRUE), 1)
        paste0(cg$name, " ", cg$subset$value, " Avg. = ", avg)
      },
      comparison_data_list,
      instruction$comparison_groups,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )
    
    label_text <- paste(labels, collapse = "\n")
    text_grob <- textGrob(
      label = label_text,
      just  = "center",
      gp    = gpar(col = "#145c3c", fontsize = 18, fontface = "bold")
    )
    width_grob  <- grobWidth(text_grob) + unit(12, "mm")
    height_grob <- grobHeight(text_grob) + unit(8, "mm")
    rect_grob <- rectGrob(
      x      = unit(1, "npc") - unit(5, "mm"),
      y      = unit(1, "npc") - unit(5, "mm"),
      width  = width_grob,
      height = height_grob,
      just   = c("right", "top"),
      gp     = gpar(fill = "#97e27f", col = NA)
    )
    text_grob_centered <- editGrob(
      text_grob,
      x = unit(1, "npc") - unit(5, "mm") - width_grob / 2,
      y = unit(1, "npc") - unit(5, "mm") - height_grob / 2,
      just = c("center", "center")
    )
    grid.draw(grobTree(rect_grob, text_grob_centered))
  }
  
  # ------ DATA FILTERING ------------------------------------------------------
  # Filter data for focal group and validate required columns
  metric_col <- instruction$metric
  if (!(metric_col %in% names(data))) {
    message(paste0("⚠️ Metric '", metric_col, "' not found in the dataset."))
    return(ppt_doc)
  }
  
  subset_col <- instruction$focal_group$subset$title %||% NULL
  if (!is.null(subset_col) && !(subset_col %in% names(data))) {
    message(paste0("⚠️ Subset column '", subset_col, "' not found."))
    return(ppt_doc)
  }
  
  data_focal <- data %>% filter(group == instruction$focal_group$name)
  if (!is.null(subset_col)) {
    data_focal <- data_focal %>%
      filter(.data[[subset_col]] == instruction$focal_group$subset$value)
  }
  
  values_focal <- data_focal[[metric_col]]
  avg_focal    <- round(mean(values_focal, na.rm = TRUE), 1)
  
  # ------ COMPARISON DATA -----------------------------------------------------
  # Prepare filtered comparison group data
  comparison_data_list <- NULL
  if (!is.null(instruction$comparison_groups)) {
    comparison_data_list <- lapply(
      instruction$comparison_groups,
      function(cg) {
        subset_col <- cg$subset$title %||% NULL
        if (!is.null(subset_col) && !(subset_col %in% names(data))) {
          return(NULL)
        }
        data_comp <- data %>% filter(group == cg$name)
        if (!is.null(subset_col)) {
          data_comp <- data_comp %>%
            filter(.data[[subset_col]] == cg$subset$value)
        }
        data_comp
      }
    )
  }
  
  # ------ DENSITY ESTIMATION --------------------------------------------------
  # Estimate and normalize density of focal group values
  x_min <- min(values_focal, na.rm = TRUE)
  x_max <- max(values_focal, na.rm = TRUE)
  x_min <- if (x_min < 5) 0 else x_min
  
  dens <- density(
    values_focal,
    adjust = 1.2,
    from   = x_min,
    to     = x_max,
    n      = 100
  )
  data_dens <- data.frame(x = dens$x, y = dens$y * 100)
  
  y_max     <- max(data_dens$y)
  y_step    <- get_y_step(y_max)
  y_max_pad <- ceiling(y_max / y_step) * y_step
  
  x_range <- x_max - x_min
  x_step  <- get_x_step(x_range)
  x_lim   <- c(x_min - (x_min %% x_step), ceiling(x_max))
  
  # ------ PLOT ----------------------------------------------------------------
  # Build ggplot object with density curve and vertical average line
  plot_obj <- ggplot(data_dens, aes(x = x, y = y)) +
    geom_line(color = "#8ddef9", linewidth = 1.5) +
    annotate(
      "segment",
      x     = avg_focal,
      xend  = avg_focal,
      y     = 0,
      yend  = y_max_pad * 1.1,
      color = "yellow",
      linetype = "dashed",
      linewidth = 1
    ) +
    annotate(
      "text",
      x        = avg_focal - 0.5,
      y        = y_max_pad * 1.05,
      label    = paste("Avg. =", avg_focal),
      color    = "yellow",
      size     = 7,
      fontface = "bold",
      hjust    = 1
    ) +
    scale_x_continuous(
      limits = x_lim,
      breaks = seq(x_lim[1], x_lim[2], by = x_step),
      expand = c(0, 1)
    ) +
    scale_y_continuous(
      breaks = seq(0, y_max_pad, by = y_step),
      labels = label_percent(scale = 1),
      expand = c(0, 0)
    ) +
    coord_cartesian(ylim = c(0, y_max_pad * 1.1)) +
    labs(
      title = NULL,
      x     = instruction$x_title,
      y     = "Density"
    ) +
    global_theme() +
    theme(
      plot.title      = element_text(
        color = "white", face = "bold", size = 26, hjust = 0
      ),
      plot.margin     = margin(t = 30, r = 20, b = 15, l = 20),
      legend.position = "none"
    )
  
  # ------ INSERT SLIDE --------------------------------------------------------
  # Insert plot and optional comparison label into PowerPoint slide
  if (is.null(instruction$is_first) || !instruction$is_first) {
    ppt_doc <- add_slide(ppt_doc, layout = "Title and Content")
  }
  
  dims <- slide_size(ppt_doc)
  
  ppt_doc <- ppt_doc %>%
    ph_with(
      value = block_list(
        fpar(ftext(
          instruction$title %||% " ",
          fp_text(
            font.size   = 28,
            bold        = TRUE,
            font.family = "Arial",
            color       = "#323233"
          )
        ))
      ),
      location = ph_location(
        left   = 0.4,
        top    = 0.2,
        width  = dims$width - 1,
        height = 0.8
      )
    ) %>%
    ph_with(
      value = dml(ggobj = plot_obj, bg = "transparent"),
      location = ph_location(
        left   = 0,
        top    = 1.0,
        width  = dims$width,
        height = dims$height - 1.4
      )
    )
  
  if (!is.null(instruction$comparison_groups)) {
    ppt_doc <- ppt_doc %>%
      ph_with(
        value = dml(
          code = draw_comparison_label(
            comparison_data_list,
            instruction
          ),
          bg = "transparent"
        ),
        location = ph_location(
          left   = 0,
          top    = 1.0,
          width  = dims$width,
          height = dims$height
        )
      )
  }
  
  return(ppt_doc)
}
