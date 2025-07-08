# ------ INSTRUCTIONS ---------------------------------------------------------

#' Slide instructions for automated chart generation
#'
#' A structured list where each element defines how a chart should be rendered
#' in the LIFE Matrix deck—used by functions like `generate_density_slide()`
#' and `generate_bar_*_slide()`.
#'
#' Includes group setup, metric(s), unit, axis labels, and chart type.
#'
#' @format A list of slide configs with fields like `slide`, `function_name`,
#'   `metric`, `unit`, `focal_group`, `comparison_groups`, `category`, etc.

instructions <- list(
  
  # ------ Slide 3: Density without comparisons -------------------------------
  list(
    function_name = "generate_density_slide",
    metric = "life_satisfaction",
    x_title = "Life Satisfaction",
    y_title = "Density",
    title = "LIFE SATISFACTION",
    focal_group = list(name = focal_group, subset = NULL),
    comparison_groups = NULL
  ),
  
  # ------ Slide 4: Density with group comparison -----------------------------
  list(
    function_name = "generate_density_slide",
    metric = "life_satisfaction",
    x_title = "Life Satisfaction",
    y_title = "Density",
    title = "LIFE SATISFACTION",
    focal_group = list(name = focal_group, subset = NULL),
    comparison_groups = list(
      list(name = comparison_group_1, subset = NULL)
    )
  ),
  
  # ------ Slide 5: Density with gender comparisons ---------------------------
  list(
    function_name = "generate_density_slide",
    metric = "life_satisfaction",
    x_title = "Life Satisfaction",
    y_title = "Density",
    title = "LIFE SATISFACTION",
    focal_group = list(name = focal_group, subset = NULL),
    comparison_groups = list(
      list(name = comparison_group_1, subset = list(title = "gender", value = "Women")),
      list(name = comparison_group_1, subset = list(title = "gender", value = "Men"))
    )
  ),
  
  # ------ Slide 8–12: Importance of JAM (bar metric) -------------------------
  list(
    function_name = "generate_bar_metric_slide",
    unit = "%",
    x_title = NULL,
    y_title = "% High Importance",
    title = "IMPORTANCE OF JAM",
    focal_group = list(name = focal_group, subset = NULL),
    comparison_groups = NULL,
    bar_value = c("joy", "achievement", "meaningfulness"),
    target = NULL
  ),
  list(
    function_name = "generate_bar_metric_slide",
    unit = "%",
    x_title = NULL,
    y_title = "% High Importance",
    title = "IMPORTANCE OF JAM",
    focal_group = list(name = focal_group, subset = NULL),
    comparison_groups = list(
      list(name = NA, subset = NULL)
    ),
    bar_value = c("joy", "achievement", "meaningfulness"),
    target = NULL
  ),
  list(
    function_name = "generate_bar_metric_slide",
    unit = "%",
    x_title = NULL,
    y_title = "% High Importance",
    title = "IMPORTANCE OF JAM",
    focal_group = list(name = focal_group, subset = NULL),
    comparison_groups = list(
      list(name = comparison_group_1, subset = list(title = "gender", value = NULL))
    ),
    bar_value = c("joy", "achievement", "meaningfulness"),
    target = NULL
  ),
  list(
    function_name = "generate_bar_metric_slide",
    unit = "%",
    x_title = NULL,
    y_title = "% High Importance",
    title = "IMPORTANCE OF JAM",
    focal_group = list(name = focal_group, subset = NULL),
    comparison_groups = list(
      list(name = NA, subset = NULL),
      list(name = NA, subset = NULL)
    ),
    bar_value = c("joy", "achievement", "meaningfulness"),
    target = NULL
  ),
  list(
    function_name = "generate_bar_metric_slide",
    unit = "%",
    x_title = NULL,
    y_title = "% High Importance",
    title = "IMPORTANCE OF JAM",
    focal_group = list(name = focal_group, subset = NULL),
    comparison_groups = list(
      list(name = comparison_group_1, subset = list(title = "gender", value = "Women")),
      list(name = NA, subset = NULL)
    ),
    bar_value = c("joy", "achievement", "meaningfulness"),
    target = NULL
  ),
  list(
    function_name = "generate_bar_metric_slide",
    unit = "%",
    x_title = NULL,
    y_title = "% High Importance",
    title = "IMPORTANCE OF JAM",
    focal_group = list(name = focal_group, subset = NULL),
    comparison_groups = list(
      list(name = comparison_group_1, subset = list(title = "gender", value = "Women")),
      list(name = comparison_group_1, subset = list(title = "gender", value = "Men"))
    ),
    bar_value = c("joy", "achievement", "meaningfulness"),
    target = NULL
  ),
  list(
    function_name = "generate_bar_metric_slide",
    unit = "%",
    x_title = NULL,
    y_title = "% High Importance",
    title = "IMPORTANCE OF JAM",
    focal_group = list(name = focal_group, subset = NULL),
    comparison_groups = list(
      list(name = comparison_group_1, subset = list(title = "gender", value = "Women")),
      list(name = comparison_group_1, subset = list(title = "gender", value = "Men"))
    ),
    bar_value = c("joy", "achievement", "meaningfulness"),
    target = c("joy_min", "achievement_min", "meaningfulness_min")
  ),
  
  # ------ More density plots -------------------------------------------------
  list(
    function_name = "generate_density_slide",
    metric = "reading_hours",
    unit = "score",
    x_title = "Reading hours",
    y_title = "Density",
    title = "READING HOURS",
    focal_group = list(name = focal_group, subset = NULL),
    comparison_groups = NULL
  ),
  list(
    function_name = "generate_density_slide",
    metric = "reading_hours",
    unit = "score",
    x_title = "Reading hours",
    y_title = "Density",
    title = "READING HOURS",
    focal_group = list(name = focal_group, subset = NULL),
    comparison_groups = list(
      list(name = comparison_group_1, subset = list(title = "gender", value = "Women")),
      list(name = comparison_group_1, subset = list(title = "gender", value = "Men"))
    )
  ),
  list(
    function_name = "generate_density_slide",
    metric = "sleep_hours",
    unit = "score",
    x_title = "Sleep hours",
    y_title = "Density",
    title = "SLEEP HOURS",
    focal_group = list(name = focal_group, subset = NULL),
    comparison_groups = NULL
  ),
  list(
    function_name = "generate_density_slide",
    metric = "sleep_hours",
    unit = "score",
    x_title = "Sleep hours",
    y_title = "Density",
    title = "SLEEP HOURS",
    focal_group = list(name = focal_group, subset = NULL),
    comparison_groups = list(
      list(name = comparison_group_1, subset = list(title = "gender", value = "Women")),
      list(name = comparison_group_1, subset = list(title = "gender", value = "Men"))
    )
  ),
  list(
    function_name = "generate_density_slide",
    metric = "exercice_hours",
    unit = "%",
    x_title = "Exercise hours",
    y_title = "Density",
    title = "EXERCISE HOURS",
    focal_group = list(name = focal_group, subset = NULL),
    comparison_groups = NULL
  ),
  list(
    function_name = "generate_density_slide",
    metric = "exercice_hours",
    unit = "%",
    x_title = "Exercise hours",
    y_title = "Density",
    title = "EXERCISE HOURS",
    focal_group = list(name = focal_group, subset = NULL),
    comparison_groups = list(
      list(name = comparison_group_1, subset = list(title = "gender", value = "Women")),
      list(name = comparison_group_1, subset = list(title = "gender", value = "Men"))
    )
  ),
  
  # ------ Slide 62–65: Example with one bar metric ---------------------------
  list(
    function_name = "generate_bar_metric_slide",
    unit = "%",
    x_title = NULL,
    y_title = "% Getting Meaningfulness+",
    title = "PERCENT MEETING MEANINGFULNESS+ AT WORK",
    focal_group = list(name = focal_group, subset = NULL),
    comparison_groups = NULL,
    bar_value = c("meaningfulness"),
    target = NULL
  ),
  list(
    function_name = "generate_bar_metric_slide",
    unit = "%",
    x_title = NULL,
    y_title = "% Getting Meaningfulness+",
    title = "PERCENT MEETING MEANINGFULNESS+ AT WORK",
    focal_group = list(name = focal_group, subset = NULL),
    comparison_groups = list(
      list(name = comparison_group_1, subset = list(title = "gender", value = "Women")),
      list(name = NA, subset = NULL)
    ),
    bar_value = c("meaningfulness"),
    target = NULL
  ),
  list(
    function_name = "generate_bar_metric_slide",
    unit = "%",
    x_title = NULL,
    y_title = "% Getting Meaningfulness+",
    title = "PERCENT MEETING MEANINGFULNESS+ AT WORK",
    focal_group = list(name = focal_group, subset = NULL),
    comparison_groups = list(
      list(name = comparison_group_1, subset = list(title = "gender", value = "Women")),
      list(name = comparison_group_1, subset = list(title = "gender", value = "Men"))
    ),
    bar_value = c("meaningfulness"),
    target = c("meaningfulness_min")
  ),
  
  # ------ Slide 67–74: Category charts ---------------------------------------
  list(
    function_name = "generate_bar_category_slide",
    metric = "life_satisfaction",
    category = list(
      name = "income_range",
      order = "income_range_levels"
    ),
    unit = NULL,
    title = "LIFE SATISFACTION BY INCOME",
    x_title = "Income ($)",
    y_title = "Life Satisfaction",
    focal_group = list(name = focal_group, subset = NULL),
    trend_line = TRUE
  ),
  list(
    function_name = "generate_bar_category_slide",
    metric = "n_children",
    category = list(
      name = "reunion_class",
      order = "reunion_class_levels"
    ),
    unit = NULL,
    title = "KIDS - AVERAGE NUMBER BY REUNION CLASS",
    x_title = "Reunion Class",
    y_title = "Number of Children",
    focal_group = list(name = focal_group, subset = NULL),
    trend_line = FALSE
  ),
  list(
    function_name = "generate_bar_category_slide",
    metric = "life_satisfaction",
    category = list(
      name = "children_range",
      order = "children_range_levels"
    ),
    unit = NULL,
    title = "LIFE SATISFACTION BY NUMBER OF CHILDREN",
    x_title = "Number of Children",
    y_title = "Life satisfaction",
    focal_group = list(name = focal_group, subset = NULL),
    trend_line = TRUE
  ),
  list(
    function_name = "generate_bar_category_slide",
    metric = "meaningfulness",
    category = list(
      name = "reunion_class",
      order = "reunion_class_levels"
    ),
    unit = "%",
    title = "PERCENT MEETING MEANINGFULNESS+ AT WORK",
    x_title = "Reunion Class",
    y_title = "Percent Meeting Meaningfulness+",
    focal_group = list(name = focal_group, subset = NULL),
    trend_line = FALSE
  )
)
