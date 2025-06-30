#' List of structured slide instructions for chart generation.
#'
#' Each list element defines the configuration for a slide in the LIFE Matrix deck.
#' Used by chart generation functions to dynamically control rendering.

instructions <- list(
  
  # ------ Slide 3: Density with no comparison groups -----------------------
  list(
    slide             = 3,
    slide_type        = "density_chart",
    metric            = "life_satisfaction",
    unit              = NULL,
    x_title           = "Life Satisfaction",
    y_title           = "Density",
    focal_group       = list(
      name   = focal_group,
      subset = NULL
    ),
    comparison_groups = NULL
  ),
  
  # ------ Slide 5: Density with comparison groups  -------------------------
  list(
    slide             = 3,
    slide_type        = "density_chart",
    metric            = "life_satisfaction",
    unit              = NULL,
    x_title           = "Life Satisfaction",
    y_title           = "Density",
    focal_group       = list(
      name   = focal_group,
      subset = NULL
    ),
    comparison_groups = list(
      list(
        name   = comparison_group_1,
        subset = list(title = "gender", value = "Women")
      ),
      list(
        name   = comparison_group_1,
        subset = list(title = "gender", value = "Men")
      )
    )
  ),
  # ------ Slide 8: Focal only + target ------------------------------------
  list(
    slide             = 8,
    slide_type        = "bar_chart",
    metric            = c("Joy", "Achievement", "Meaningfulness"),
    category          = NULL,
    unit              = "%",
    x_title           = NULL,
    y_title           = "% High Importance",
    title             = "IMPORTANCE OF JAM",
    focal_group       = list(name = focal_group, subset = NULL),
    comparison_groups = NULL,
    value_column      = "metric_value",
    target            = NULL,
    trend_line        = FALSE
  ),
  
  # ------ Slide 12: Mix real + placeholder -------------------------------
  list(
    slide             = 12,
    slide_type        = "bar_chart",
    metric            = c("Joy", "Achievement", "Meaningfulness"),
    category          = NULL,
    unit              = "%",
    x_title           = NULL,
    y_title           = "% High Importance",
    title             = "IMPORTANCE OF JAM",
    focal_group       = list(name = focal_group, subset = NULL),
    comparison_groups = list(
      list(
        name   = comparison_group_1,
        subset = list(title = "gender", value = "Women")
      ),
      list(name = NA, subset = NULL)
    ),
    value_column      = "metric_value",
    target            = NULL,
    trend_line        = NULL
  ),
  
  # ------ Slide 13: Two gender comparisons -------------------------------
  list(
    slide             = 13,
    slide_type        = "bar_chart",
    metric            = c("Joy", "Achievement", "Meaningfulness"),
    category          = NULL,
    unit              = "%",
    x_title           = NULL,
    y_title           = "% High Importance",
    title             = "IMPORTANCE OF JAM",
    focal_group       = list(name = focal_group, subset = NULL),
    comparison_groups = list(
      list(
        name   = comparison_group_1,
        subset = list(title = "gender", value = "Women")
      ),
      list(
        name   = comparison_group_1,
        subset = list(title = "gender", value = "Men")
      )
    ),
    value_column      = "metric_value",
    target            = NULL,
    trend_line        = NULL
  ),
  
  # ------ Slide 18: With target and real groups --------------------------
  list(
    slide             = 18,
    slide_type        = "bar_chart",
    metric            = c("Joy", "Achievement", "Meaningfulness"),
    category          = NULL,
    unit              = "%",
    x_title           = NULL,
    y_title           = "% High Importance",
    title             = "IMPORTANCE OF JAM",
    focal_group       = list(name = focal_group, subset = NULL),
    comparison_groups = list(
      list(
        name   = comparison_group_1,
        subset = list(title = "gender", value = "Women")
      ),
      list(
        name   = comparison_group_1,
        subset = list(title = "gender", value = "Men")
      )
    ),
    value_column      = "metric_value",
    target            = "munimums_value",
    trend_line        = NULL
  ),
  
  # ------ Slide 65: Meaningfulness+ with trend ---------------------------
  list(
    slide             = 65,
    slide_type        = "bar_chart",
    metric            = "Joy",
    category          = NULL,
    unit              = "%",
    x_title           = NULL,
    y_title           = "% Getting Meaningfulness+",
    title             = "PERCENT GETTING MEANINGFULNESS+ AT WORK",
    focal_group       = list(name = focal_group, subset = NULL),
    comparison_groups = list(
      list(
        name   = comparison_group_1,
        subset = list(title = "gender", value = "Women")
      ),
      list(
        name   = comparison_group_1,
        subset = list(title = "gender", value = "Men")
      )
    ),
    value_column      = "metric_value",
    target            = NULL,
    trend_line        = TRUE
  ),

  list(
    slide             = 65,
    slide_type        = "bar_chart",
    metric            = "Joy",
    category          = NULL,
    unit              = "%",
    x_title           = NULL,
    y_title           = "% Getting Meaningfulness+",
    title             = "PERCENT GETTING MEANINGFULNESS+ AT WORK",
    focal_group       = list(name = focal_group, subset = NULL),
    comparison_groups = list(
      list(
        name   = comparison_group_1,
        subset = list(title = "gender", value = "Women")
      ),
      list(name = NA, subset = NULL)
    ),
    value_column      = "metric_value",
    target            = NULL,
    trend_line        = TRUE
  ),
  
  # ------ Slide 67: Category comparisons ---------------------------------
  list(
    slide             = 67,
    slide_type        = "bar_chart",
    metric            = "Life satisfaction",
    category          = "income",
    unit              = "score",
    x_title           = "Income ($)",
    y_title           = "Life Satisfaction",
    title             = "LIFE SATISFACTION AND INCOME",
    focal_group       = list(name = focal_group, subset = NULL),
    comparison_groups = NULL,
    value_column      = "metric_value",
    target            = NULL,
    trend_line        = TRUE
  ),
  
  list(
    slide             = 67,
    slide_type        = "bar_chart",
    metric            = "Life satisfaction",
    category          = "children_category",
    unit              = "score",
    x_title           = "Number of Kids",
    y_title           = "Life Satisfaction",
    title             = "Number of Kids",
    focal_group       = list(name = focal_group, subset = NULL),
    comparison_groups = NULL,
    value_column      = "metric_value",
    target            = NULL,
    trend_line        = TRUE
  ),
  
  # ------ Slide 74: Reunion class variations -----------------------------
  list(
    slide             = 74,
    slide_type        = "bar_chart",
    metric            = "Meaningfulness+",
    category          = "reunion_class",
    unit              = "%",
    x_title           = "Reunion Class",
    y_title           = "Percent Meeting Meaningfulness+",
    title             = "PERCENT MEETING MEANINGFULNESS+ AT WORK",
    focal_group       = list(name = focal_group, subset = NULL),
    comparison_groups = NULL,
    value_column      = "metric_value",
    target            = NULL,
    trend_line        = NULL
  ),
  
  list(
    slide             = 74,
    slide_type        = "bar_chart",
    metric            = NULL,
    category          = c("reunion_class", "n_children"),
    unit              = "score",
    x_title           = " ",
    y_title           = " ",
    title             = " ",
    focal_group       = list(name = focal_group, subset = NULL),
    comparison_groups = NULL,
    value_column      = NULL,
    target            = NULL,
    trend_line        = NULL
  )
)
