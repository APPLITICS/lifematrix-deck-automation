# ------ DENSITY SLIDE ---------------------------------------------------------
#' Generate Density Slide
#'
#' Creates a density chart for a focal group with optional comparison overlays.
#' Adds a vertical line for the mean and auto-scales axes. Can export directly
#' to a PowerPoint slide.
#'
#' @param data A data frame containing the metric of interest and grouping
#'   variables.
#' @param instruction A list with chart options (focal group, comparison groups,
#'   metric, titles, units).
#' @param ppt_doc Optional `officer::read_pptx()` object. If provided, the slide
#'   is appended to this document.
#'
#' @return Updated PowerPoint object (`ppt_doc`) and density plot (`plot_obj`).
generate_density_slide <- function(
    data,
    instruction,
    ppt_doc
) {
  # ------ HELPERS -------------------------------------------------------------
  # Define custom axis step functions
  get_y_step <- function(y_max) {
    target_lines <- 5
    raw_step <- y_max / (target_lines - 1)
    nice_steps <- c(0.01, 0.02, 0.05, 0.1, 0.2, 0.25, 0.5,
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

  # ------ DATA FILTERING ------------------------------------------------------
  unit <- instruction$unit
  metric_col <- instruction$metric
  subset_col <- instruction$focal_group$subset$title %||% NULL

  # ------ VALIDATION: REQUIRED COLUMNS ----------------------------------------
  missing_cols <- character()

  if (!(instruction$metric %in% names(data))) {
    missing_cols <- c(missing_cols, instruction$metric)
  }

  if (!is.null(subset_col) && !(subset_col %in% names(data))) {
    missing_cols <- c(missing_cols, subset_col)
  }

  if (!is.null(instruction$comparison_groups)) {
    for (cg in instruction$comparison_groups) {
      subset_col_cg <- cg$subset$title %||% NULL
      if (!is.null(subset_col_cg) && !(subset_col_cg %in% names(data))) {
        missing_cols <- c(missing_cols, subset_col_cg)
      }
    }
  }

  if (length(missing_cols) > 0) {
    message("❌ Missing column(s): ", paste(unique(missing_cols), collapse = ", "),
            ". Slide skipped.")
    return(invisible(NULL))
  }

  # ------ FILTER FOCAL DATA ---------------------------------------------------
  data_focal <- data %>% filter(group == instruction$focal_group$name)
  if (!is.null(subset_col)) {
    data_focal <- data_focal %>%
      filter(.data[[subset_col]] == instruction$focal_group$subset$value)
  }

  values_focal <- data_focal[[metric_col]]
  avg_focal <- round(mean(values_focal, na.rm = TRUE), 1)

  # ------ COMPARISON DATA -----------------------------------------------------
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
  x_min <- min(values_focal, na.rm = TRUE)
  x_max <- max(values_focal, na.rm = TRUE)
  x_min <- if (x_min < 5) 0 else x_min
  dens <- density(
    values_focal[!is.na(values_focal)],
    adjust = 1.2,
    from = x_min,
    to = x_max,
    n = 100
  )

  data_dens <- data.frame(
    x = dens$x,
    y = if (!is.null(unit) && unit == "%") dens$y * 100 else dens$y
  )

  y_max <- max(data_dens$y)
  y_step <- get_y_step(y_max)
  y_max_pad <- ceiling(y_max / y_step) * y_step

  x_range <- x_max - x_min
  x_step <- get_x_step(x_range)
  x_lim <- c(x_min - (x_min %% x_step), ceiling(x_max))

  # ------ PLOT ----------------------------------------------------------------
  plot_obj <- ggplot(data_dens, aes(x = x, y = y)) +
    geom_line(color = "#8ddef9", linewidth = 1.5) +
    annotate(
      "segment",
      x = avg_focal, xend = avg_focal,
      y = 0, yend = y_max_pad * 1.1,
      color = "yellow", linetype = "dashed", linewidth = 1
    ) +
    annotate(
      "text",
      x = avg_focal - 0.5, y = y_max_pad * 1.05,
      label = paste("Avg. =", sprintf("%.1f", avg_focal)),
      color = "yellow", size = 7, fontface = "bold", hjust = 1
    ) +
    scale_x_continuous(
      limits = x_lim,
      breaks = seq(x_lim[1], x_lim[2], by = x_step),
      expand = c(0, 1)
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      expand = c(0, 0),
      labels = if (!is.null(unit) && unit == "%") {
        label_percent(scale = 1, accuracy = 1)
      } else {
        label_number(accuracy = 0.01)
      }
    ) +
    coord_cartesian(ylim = c(0, y_max_pad * 1.1)) +
    labs(
      title = NULL,
      x = instruction$x_title,
      y = "Density"
    ) +
    global_theme() +
    theme(
      plot.title = element_text(color = "white", face = "bold", size = 26, hjust = 0),
      plot.margin = margin(t = 90, r = 20, b = 10, l = 20),
      legend.position = "none"
    )
  
  # ------ ADD COMPARISON LABELS (TOP-RIGHT BOX OUTSIDE PLOT) --------------------
  if (!is.null(comparison_data_list) && length(comparison_data_list) > 0) {
    metric_col <- instruction$metric
    labels <- mapply(
      function(data_comp, cg) {
        avg <- round(mean(data_comp[[metric_col]], na.rm = TRUE), 1)
        paste0(cg$name, " ", cg$subset$value, " Avg. = ", sprintf("%.1f", avg))
      },
      comparison_data_list,
      instruction$comparison_groups,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )

    label_obj <- ggplot() +
      annotate(
        "label",
        x = 1, y = 1,
        label = paste(labels, collapse = "\n"),
        hjust = 1, vjust = 1,
        fill = "#97e27f", color = "#145c3c",
        fontface = "bold", size = 7,
        label.size = 0,
        label.padding = unit(0.6, "lines")
      ) +
      xlim(0, 1) + ylim(0, 1.1) +
      theme_void() +
      theme(plot.background = element_rect(fill = "transparent", color = NA))

    plot_obj <- ggdraw() +
      draw_plot(plot_obj) +
      draw_plot(
        label_obj, x = 1, y = 1,
        width = 0.35, height = 0.25,
        hjust = 1, vjust = 1
      )
  }

  # ------ INSERT SLIDE --------------------------------------------------------
  if (!is.null(ppt_doc)) {
    ppt_doc <- export_plot_to_slide(
      ppt_doc = ppt_doc,
      plot_obj = plot_obj,
      title_text = instruction$title %||% " ",
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
