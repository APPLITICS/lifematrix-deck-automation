# ------  SCATTER SLIDE -------------------------------------------------------
#' Generate Scatter Plot
#'
#' Creates a labeled scatter plot comparing average x and y metric values 
#' (e.g., hours/week vs subjective value) for multiple activities. Each point 
#' represents one activity, with coordinates computed as the mean of the 
#' respective x and y variables. Labels are automatically spaced to avoid 
#' overlap using the `ggrepel` package. 
#'
#' @param data        Data frame with numeric values for each activity
#' @param instruction A list with plot settings (metrics, group, title, etc.)
#' @param ppt_doc     Optional PowerPoint object to export into
#'
#' @return ggplot object (or pptx object if ppt_doc is provided)
#' 
generate_scatter_slide <- function(
    data,
    instruction,
    ppt_doc = NULL
) {
  # ------ EXTRACT INSTRUCTION SETTINGS -----------------------------------
  x_vars <- instruction$metric
  y_vars <- instruction$subjective_value
  focal <- instruction$focal_group
  
  # ------ EARLY VALIDATION ------------------------------------------------
  required_cols <- unique(na.omit(c(
    x_vars,
    y_vars,
    focal$subset$title %||% NULL
  )))
  
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    message("❌ Missing column(s): ", paste(missing_cols, collapse = ", "), ". Slide skipped.")
    return(NULL)
  }
  # ------ FILTER TO FOCAL GROUP ------------------------------------------
  if (!is.null(focal)) {
    group_filter <- data$group == focal$name
    if (!is.null(focal$subset)) {
      group_filter <- group_filter &
        data[[focal$subset$title]] == focal$subset$value
    }
    data <- data[group_filter, ]
  }
  
  # ------ BUILD COORDINATE MAPPING TABLE ---------------------------------
  coord_map <- tibble(
    x_var = x_vars,
    y_var = y_vars,
    label = coalesce(
      variable_map$label[match(x_vars, variable_map$variable)],
      x_vars
    )
  )
  
  # ------ COMPUTE AVERAGES FOR EACH POINT --------------------------------
  plot_df <- coord_map %>%
    rowwise() %>%
    mutate(
      x = mean(data[[x_var]], na.rm = TRUE),
      y = mean(data[[y_var]], na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(label, x, y)
  
  # ------ BUILD PLOT -----------------------------------------------------
  plot_obj <- ggplot(plot_df, aes(x = x, y = y)) +
    geom_point(color = "#84d8f6", size = 4) +
    
    geom_text_repel(
      aes(label = label),
      color = "white",
      size = 6,
      fontface = "bold",
      box.padding = 0.6,
      point.padding = 0.6,
      force = 2,
      force_pull = 0.2,
      max.overlaps = Inf,
      segment.color = NA
    ) +
    
    # ------ X-AXIS SETUP -----------------------------------------------
  scale_x_continuous(
    name = instruction$x_title %||% " ",
    breaks = function(x) {
      rng <- range(x, na.rm = TRUE)
      max_val <- ceiling(rng[2])
      step <- case_when(
        max_val <= 5  ~ 1,
        max_val <= 10 ~ 1,
        max_val <= 15 ~ 2,
        max_val <= 25 ~ 5,
        TRUE          ~ 10
      )
      seq(0, max_val + step, by = step)
    },
    limits = c(0, NA),
    expand = c(0, 0)
  ) +
    
    # ------ Y-AXIS SETUP -----------------------------------------------
  scale_y_continuous(
    name = instruction$y_title %||% " ",
    breaks = function(y) {
      rng <- range(y, na.rm = TRUE)
      ymax <- ceiling(rng[2])
      ymax <- pmax(ymax, 3)
      
      step <- case_when(
        ymax <= 3  ~ 1,
        ymax <= 6  ~ 1,
        ymax <= 10 ~ 2,
        ymax <= 20 ~ 5,
        TRUE       ~ 10
      )
      
      last_tick <- ceiling(ymax / step) * step
      seq(0, last_tick, by = step)
    },
    limits = function(y) {
      rng <- range(y, na.rm = TRUE)
      ymax <- ceiling(rng[2])
      ymax <- pmax(ymax, 3)
      
      step <- case_when(
        ymax <= 3  ~ 1,
        ymax <= 6  ~ 1,
        ymax <= 10 ~ 2,
        ymax <= 20 ~ 5,
        TRUE       ~ 10
      )
      
      last_tick <- ceiling(ymax / step) * step
      c(0, last_tick)
    },
    expand = c(0, 0)
  ) +
  
    # ------ THEME SETUP ------------------------------------------------
  theme_minimal(base_size = 16) +
    theme(
      panel.grid = element_blank(),
      axis.line.x = element_line(color = "white", linewidth = 1),
      axis.line.y = element_line(color = "white", linewidth = 1),
      axis.ticks = element_blank(),
      axis.text.x = element_text(
        color = "white",
        size = 18,
        face = "bold",
        margin = margin(t = 8)
      ),
      axis.text.y = element_text(
        color = "white",
        size = 18,
        face = "bold"
      ),
      axis.title.x = element_text(
        color = "white",
        face = "bold",
        size = 20,
        margin = margin(t = 20)
      ),
      axis.title.y = element_text(
        color = "white",
        face = "bold",
        size = 20,
        margin = margin(r = 15)
      ),
      plot.margin = margin(30, 40, 10, 40)
    )
  
  # ------ EXPORT OR RETURN -----------------------------------------------
  if (!is.null(ppt_doc)) {
    return(export_plot_to_slide(
      ppt_doc = ppt_doc,
      plot_obj = plot_obj,
      title_text = instruction$title %||% " ",
      is_first = instruction$is_first
    ))
  }
  
  return(invisible(NULL))
}
