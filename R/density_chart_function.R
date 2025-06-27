# ------ MAIN FUNCTION ------------------------------------------------------
draw_density_chart <- function(
    df,
    inst
) {
  # ------ HELPERS -------------------------------------------------------
  get_y_step <- function(y_max) {
    if (y_max >= 0.8)   return(0.2)
    if (y_max >= 0.2)   return(0.1)
    if (y_max >= 0.125) return(0.025)
    if (y_max >= 0.1)   return(0.025)
    if (y_max >= 0.04)  return(0.02)
    return(0.01)
  }
  
  get_x_step <- function(x_range) {
    if (x_range <= 12) return(1)
    nice_int_steps <- c(10, 5, 2)
    for (step in nice_int_steps) {
      n_breaks <- x_range / step
      if (n_breaks >= 4 && n_breaks <= 8) return(step)
    }
    return(1)
  }
  
  draw_comparison_label <- function(df, inst) {
    if (is.null(inst$comparison_groups)) return(NULL)
    
    comp_text <- lapply(inst$comparison_groups, function(cg) {
      avg <- df %>%
        filter(
          group == cg$name,
          metric == inst$metric,
          !!sym(cg$subset$title) == cg$subset$value
        ) %>%
        summarise(
          avg = round(mean(value), 1)
        ) %>%
        pull(avg)
      
      paste0(
        cg$name, " ", cg$subset$value, " Avg. = ", avg
      )
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
    
    text_grob_centered <- editGrob(
      text_grob,
      x    = unit(1, "npc") - unit(5, "mm") - width_grob / 2,
      y    = unit(1, "npc") - unit(5, "mm") - height_grob / 2,
      just = c("center", "center")
    )
    
    grid.draw(grobTree(rect_grob, text_grob_centered))
  }
  
  # ------ DATA FILTERING ------------------------------------------------
  df_focal <- df %>%
    filter(
      group  == inst$focal_group$name,
      metric == inst$metric
    )
  
  if (!is.null(inst$focal_group$subset)) {
    df_focal <- df_focal %>%
      filter(
        !!sym(inst$focal_group$subset$title) ==
          inst$focal_group$subset$value
      )
  }
  
  avg_focal <- round(mean(df_focal$value), 1)
  
  # ------ DENSITY COMPUTATION -------------------------------------------
  x_min <- min(df_focal$value)
  x_max <- max(df_focal$value)
  x_min <- if (x_min < 5) 0 else x_min
  
  dens <- density(
    df_focal$value,
    adjust = 1.2,
    from   = x_min,
    to     = x_max,
    n      = 2048
  )
  
  df_dens <- data.frame(
    x = dens$x,
    y = dens$y
  )
  
  # ------ AXIS CALCULATIONS --------------------------------------------
  y_max_raw <- max(df_dens$y)
  y_step    <- get_y_step(y_max_raw)
  y_max_pad <- ceiling(y_max_raw / y_step) * y_step
  y_lim     <- c(0, y_max_pad)
  
  x_range <- x_max - x_min
  x_step  <- get_x_step(x_range)
  x_start <- x_min - (x_min %% x_step)
  x_end   <- ceiling(x_max)
  x_lim   <- c(x_start, x_end)
  
  # ------ PLOT ---------------------------------------------------------
  p <- ggplot(df_dens, aes(x = x, y = y)) +
    geom_line(
      color = "#8ddef9",
      size  = 1.5
    ) +
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
      x     = inst$x_title,
      y     = inst$y_title
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.background   = element_rect(
        fill   = "#015881",
        color  = NA
      ),
      plot.background    = element_rect(
        fill   = "#015881",
        color  = NA
      ),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(
        color = "white",
        size  = 0.1
      ),
      panel.grid.minor.y = element_blank(),
      axis.line.x        = element_line(
        color = "white",
        size  = 1.2
      ),
      axis.line.y        = element_line(
        color = "white",
        size  = 1.2
      ),
      axis.text          = element_text(
        color = "white",
        size  = 12
      ),
      axis.title.x       = element_text(
        color   = "white",
        size    = 14,
        face    = "bold",
        margin  = margin(t = 15)
      ),
      axis.title.y       = element_text(
        color   = "white",
        size    = 14,
        face    = "bold",
        margin  = margin(r = 15)
      ),
      plot.title         = element_text(
        color = "white",
        face  = "bold",
        size  = 26,
        hjust = 0
      ),
      plot.margin        = margin(t = 30, r = 20, b = 15, l = 20),
      legend.position    = "none"
    )
  
  # ------ OUTPUT --------------------------------------------------------
  print(p)
  draw_comparison_label(df, inst)
}
