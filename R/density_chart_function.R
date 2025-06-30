#' Draws a density chart for a focal group and optionally displays comparison averages.
#'
#' This function uses kernel density estimation to show the distribution of a 
#' selected metric for a focal group. It supports:
#' - Vertical line annotation at the focal group’s average value
#' - Comparison group average labels displayed in a corner box
#' - Smart axis step sizing
#' - Automatic padding and layout configuration
#' - Filtering and summarizing the input data to focus only on the relevant group/metric
#'
#' @param data   Data frame containing the input data.
#' @param instruction List containing the configuration for the plot, including:
#'   the metric to plot, focal group definition, subset filters, and axis titles.

draw_density_chart <- function(data,instruction) {
  # ------ HELPERS -------------------------------------------------------------
  
  #' Determines step size for y-axis breaks based on y max value
  get_y_step <- function(y_max) {
    if (y_max >= 0.8)   return(0.2)
    if (y_max >= 0.2)   return(0.1)
    if (y_max >= 0.125) return(0.025)
    if (y_max >= 0.1)   return(0.025)
    if (y_max >= 0.04)  return(0.02)
    return(0.01)
  }
  #' Determines x-axis step size based on the range of x values
  get_x_step <- function(x_range) {
    if (x_range <= 12) return(1)
    nice_int_steps <- c(10, 5, 2)
    for (step in nice_int_steps) {
      n_breaks <- x_range / step
      if (n_breaks >= 4 && n_breaks <= 8) return(step)
    }
    return(1)
  }

  # ------ COMPARISON LABELS FUNCTION ------------------------------------------
  
  #' Draws a comparison group label box in top-right with average values
  draw_comparison_label <- function(data,instruction) {
    if (is.null(instruction$comparison_groups)) return(NULL)
    
    comp_text <- lapply(instruction$comparison_groups, function(cg) {
      avg <- data %>%
        filter(
          group == cg$name,
          metric == instruction$metric,
          !!sym(cg$subset$title) == cg$subset$value
        ) %>%
        summarise(
          avg = round(mean(value), 1)
        ) %>%
        pull(avg)
      
      paste0(cg$name, " ", cg$subset$value, " Avg. = ", avg)
    }) %>%
      unlist() %>%
      paste(collapse = "\n")
    
    text_grob <- textGrob(
      label = comp_text,
      just  = "center",
      gp    = gpar(
        col       = "#16634b",
        fontsize  = 12,
        fontface  = "bold"
      )
    )
    # Calculate width and height for the rectangle
    width_grob  <- grobWidth(text_grob)  + unit(10, "mm")
    height_grob <- grobHeight(text_grob) + unit(6, "mm")
    rect_grob <- rectGrob(
      x      = unit(1, "npc") - unit(5, "mm"),
      y      = unit(1, "npc") - unit(5, "mm"),
      width  = width_grob,
      height = height_grob,
      just   = c("right", "top"),
      gp     = gpar(fill = "#97e27f", col = NA)
    )
    # Center the text within the rectangle
    text_grob_centered <- editGrob(
      text_grob,
      x    = unit(1, "npc") - unit(5, "mm") - width_grob / 2,
      y    = unit(1, "npc") - unit(5, "mm") - height_grob / 2,
      just = c("center", "center")
    )
    
    grid.draw(grobTree(rect_grob, text_grob_centered))
  }
  
  #' ------ DATA FILTERING -----------------------------------------------------
  
  # Filter data for focal group
  data_focal <- data %>%
    filter(
      group  == instruction$focal_group$name,
      metric == instruction$metric
    )
  # Apply subset filter if provided (e.g., gender = "Men")
  if (!is.null(instruction$focal_group$subset)) {
    data_focal <- data_focal %>%
      filter(
        !!sym(instruction$focal_group$subset$title) ==
          instruction$focal_group$subset$value
      )
  }
  # Compute focal group average value
  avg_focal <- round(mean(data_focal$value), 1)
  
  #' ------ DENSITY COMPUTATION ------------------------------------------------
  # Determine x-range (clipping at 0 if small)
  x_min <- min(data_focal$value)
  x_max <- max(data_focal$value)
  x_min <- if (x_min < 5) 0 else x_min
  # Estimate density for focal group
  dens <- density(
    data_focal$value,
    adjust = 1.2,
    from   = x_min,
    to     = x_max,
    n      = 2048
  )
  # Create data frame for density values
  data_dens <- data.frame(
    x = dens$x,
    y = dens$y
  )
  
  #' ------ AXIS CALCULATIONS --------------------------------------------------
  
  # Y-axis limits and breaks
  y_max_raw <- max(data_dens$y)
  y_step    <- get_y_step(y_max_raw)
  y_max_pad <- ceiling(y_max_raw / y_step) * y_step
  y_lim     <- c(0, y_max_pad)
  
  # X-axis limits and breaks
  x_range <- x_max - x_min
  x_step  <- get_x_step(x_range)
  x_start <- x_min - (x_min %% x_step)
  x_end   <- ceiling(x_max)
  x_lim   <- c(x_start, x_end)
  
  #' ------ PLOT ---------------------------------------------------------------
  
  p <- ggplot(data_dens, aes(x = x, y = y)) +
    geom_line(
      color = "#8ddef9",
      size  = 1.5
    ) +
    # Vertical line for focal group average
    geom_segment(
      aes(
        x     = avg_focal,
        xend  = avg_focal,
        y     = 0,
        yend  = y_max_pad * 1.1
      ),
      linetype = "dashed",
      color    = "yellow",
      size     = 1
    ) +
    # Avg label above dashed line
    annotate(
      "text",
      x        = avg_focal - 0.5,
      y        = y_max_pad * 1.05,
      label    = paste("Avg. =", avg_focal),
      color    = "yellow",
      size     = 4.5,
      fontface = "bold",
      hjust    = 1
    ) +
    scale_x_continuous(
      limits = x_lim,
      breaks = seq(x_lim[1], x_lim[2], by = x_step),
      expand = c(0, 1)
    ) +
    scale_y_continuous(
      breaks = seq(0, y_max_pad, y_step),
      expand = c(0, 0)
    ) +
    coord_cartesian(
      ylim = c(0, y_max_pad * 1.1)
    ) +
    labs(
      title = " ",
      x     = instruction$x_title,
      y     = instruction$y_title
    ) +
    global_theme() +
    theme(
      plot.title      = element_text(
        color = "white",
        face  = "bold",
        size  = 26,
        hjust = 0
      ),
      plot.margin     = margin(t = 30, r = 20, b = 15, l = 20),
      legend.position = "none"
    )
  
  #' ------ OUTPUT -------------------------------------------------------------
  
  # Render plot
  print(p)
  # Overlay comparison group average box
  draw_comparison_label(data, instruction)
}
