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
  # ------ Slide 3: Density without comparisons ------------------------------
  list(
    function_name = "generate_density_slide",
    metric = "life_satisfaction",
    x_title = "Life Satisfaction",
    y_title = "Density",
    title = "LIFE SATISFACTION",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = NULL
  ),
  
  # ------ Slide 4: Density with group comparison ----------------------------
  list(
    function_name = "generate_density_slide",
    metric = "life_satisfaction",
    x_title = "Life Satisfaction",
    y_title = "Density",
    title = "LIFE SATISFACTION",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = list(
      list(
        name = comparison_group_1,
        subset = NULL
      )
    )
  ),
  
  # ------ Slide 5: Density with gender comparisons --------------------------
  list(
    function_name = "generate_density_slide",
    metric = "life_satisfaction",
    x_title = "Life Satisfaction",
    y_title = "Density",
    title = "LIFE SATISFACTION",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = list(
      list(
        name = comparison_group_1,
        subset = list(
          title = "gender",
          value = "Women"
        )
      ),
      list(
        name = comparison_group_1,
        subset = list(
          title = "gender",
          value = "Men"
        )
      )
    )
  ),
  
  # ------ Slide 8: JAM - Focal only -----------------------------------------
  list(
    function_name = "generate_bar_metric_slide",
    unit = "%",
    x_title = NULL,
    y_title = "% High Importance",
    title = "IMPORTANCE OF JAM",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = NULL,
    bar_value = c(
      "joy",
      "achievement",
      "meaningfulness"
    ),
    target = NULL
  ),
  
  # ------ Slide 9: JAM - NA comparison --------------------------------------
  list(
    function_name = "generate_bar_metric_slide",
    unit = "%",
    x_title = NULL,
    y_title = "% High Importance",
    title = "IMPORTANCE OF JAM",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = list(
      list(
        name = NA,
        subset = NULL
      )
    ),
    bar_value = c(
      "joy",
      "achievement",
      "meaningfulness"
    ),
    target = NULL
  ),
  
  # ------ Slide 10: JAM - Gender subset without value -----------------------
  list(
    function_name = "generate_bar_metric_slide",
    unit = "%",
    x_title = NULL,
    y_title = "% High Importance",
    title = "IMPORTANCE OF JAM",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = list(
      list(
        name = comparison_group_1,
        subset = list(
          title = "gender",
          value = NULL
        )
      )
    ),
    bar_value = c(
      "joy",
      "achievement",
      "meaningfulness"
    ),
    target = NULL
  ),
  
  # ------ Slide 11: JAM - Two NA comparisons --------------------------------
  list(
    function_name = "generate_bar_metric_slide",
    unit = "%",
    x_title = NULL,
    y_title = "% High Importance",
    title = "IMPORTANCE OF JAM",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = list(
      list(
        name = NA,
        subset = NULL
      ),
      list(
        name = NA,
        subset = NULL
      )
    ),
    bar_value = c(
      "joy",
      "achievement",
      "meaningfulness"
    ),
    target = NULL
  ),
  
  # ------ Slide 12: JAM - Women + NA comparison -----------------------------
  list(
    function_name = "generate_bar_metric_slide",
    unit = "%",
    x_title = NULL,
    y_title = "% High Importance",
    title = "IMPORTANCE OF JAM",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = list(
      list(
        name = comparison_group_1,
        subset = list(
          title = "gender",
          value = "Women"
        )
      ),
      list(
        name = NA,
        subset = NULL
      )
    ),
    bar_value = c(
      "joy",
      "achievement",
      "meaningfulness"
    ),
    target = NULL
  ),
  
  # ------ Slide 13: JAM - Women + Men with targets --------------------------
  list(
    function_name = "generate_bar_metric_slide",
    unit = "%",
    x_title = NULL,
    y_title = "% High Importance",
    title = "IMPORTANCE OF JAM",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = list(
      list(
        name = comparison_group_1,
        subset = list(
          title = "gender",
          value = "Women"
        )
      ),
      list(
        name = comparison_group_1,
        subset = list(
          title = "gender",
          value = "Men"
        )
      )
    ),
    bar_value = c(
      "joy",
      "achievement",
      "meaningfulness"
    ),
    target = c(
      "joy_min",
      "achievement_min",
      "meaningfulness_min"
    )
  ),
  # ------ Slide 14: Reading hours (no comparison) -----------------------------
  list(
    function_name = "generate_density_slide",
    metric = "reading_hours",
    unit = "score",
    x_title = "Reading hours",
    y_title = "Density",
    title = "READING HOURS",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = NULL
  ),
  
  # ------ Slide 15: Reading hours by gender -----------------------------------
  list(
    function_name = "generate_density_slide",
    metric = "reading_hours",
    unit = "score",
    x_title = "Reading hours",
    y_title = "Density",
    title = "READING HOURS",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = list(
      list(
        name = comparison_group_1,
        subset = list(
          title = "gender",
          value = "Women"
        )
      ),
      list(
        name = comparison_group_1,
        subset = list(
          title = "gender",
          value = "Men"
        )
      )
    )
  ),
  
  # ------ Slide 16: Sleep hours (no comparison) -------------------------------
  list(
    function_name = "generate_density_slide",
    metric = "sleeping_hours",
    unit = "score",
    x_title = "Sleep hours",
    y_title = "Density",
    title = "SLEEP HOURS",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = NULL
  ),
  
  # ------ Slide 17: Sleep hours by gender -------------------------------------
  list(
    function_name = "generate_density_slide",
    metric = "sleeping_hours",
    unit = "score",
    x_title = "Sleep hours",
    y_title = "Density",
    title = "SLEEP HOURS",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = list(
      list(
        name = comparison_group_1,
        subset = list(
          title = "gender",
          value = "Women"
        )
      ),
      list(
        name = comparison_group_1,
        subset = list(
          title = "gender",
          value = "Men"
        )
      )
    )
  ),
  
  # ------ Slide 18: Exercise hours (no comparison) ----------------------------
  list(
    function_name = "generate_density_slide",
    metric = "exercising_hours",
    unit = "%",
    x_title = "Exercise hours",
    y_title = "Density",
    title = "EXERCISE HOURS",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = NULL
  ),
  
  # ------ Slide 19: Exercise hours by gender ----------------------------------
  list(
    function_name = "generate_density_slide",
    metric = "exercising_hours",
    unit = "%",
    x_title = "Exercise hours",
    y_title = "Density",
    title = "EXERCISE HOURS",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = list(
      list(
        name = comparison_group_1,
        subset = list(
          title = "gender",
          value = "Women"
        )
      ),
      list(
        name = comparison_group_1,
        subset = list(
          title = "gender",
          value = "Men"
        )
      )
    )
  ),
  
  # ------ Slide 62: Meaningfulness+ (focal only) ------------------------------
  list(
    function_name = "generate_bar_metric_slide",
    unit = "%",
    x_title = NULL,
    y_title = "% Getting Meaningfulness+",
    title = "PERCENT MEETING MEANINGFULNESS+ AT WORK",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = NULL,
    bar_value = c("meaningfulness"),
    target = NULL
  ),
  
  # ------ Slide 63: Meaningfulness+ (Women + NA) ------------------------------
  list(
    function_name = "generate_bar_metric_slide",
    unit = "%",
    x_title = NULL,
    y_title = "% Getting Meaningfulness+",
    title = "PERCENT MEETING MEANINGFULNESS+ AT WORK",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = list(
      list(
        name = comparison_group_1,
        subset = list(
          title = "gender",
          value = "Women"
        )
      ),
      list(
        name = NA,
        subset = NULL
      )
    ),
    bar_value = c("meaningfulness"),
    target = NULL
  ),
  
  # ------ Slide 64: Meaningfulness+ (Women vs Men) w/ target ------------------
  list(
    function_name = "generate_bar_metric_slide",
    unit = "%",
    x_title = NULL,
    y_title = "% Getting Meaningfulness+",
    title = "PERCENT MEETING MEANINGFULNESS+ AT WORK",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = list(
      list(
        name = comparison_group_1,
        subset = list(
          title = "gender",
          value = "Women"
        )
      ),
      list(
        name = comparison_group_1,
        subset = list(
          title = "gender",
          value = "Men"
        )
      )
    ),
    bar_value = c("meaningfulness"),
    target = c("meaningfulness_min")
  ),
  
  # ------ Slide 67: Life Satisfaction by Income -------------------------------
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
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    trend_line = TRUE
  ),
  
  # ------ Slide 68: Number of Children by Reunion Class -----------------------
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
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    trend_line = FALSE
  ),
  
  # ------ Slide 69: Life Satisfaction by Number of Children -------------------
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
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    trend_line = TRUE
  ),
  
  # ------ Slide 70: Meaningfulness by Reunion Class ---------------------------
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
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    trend_line = FALSE
  ),
  # ------ Slide 75: Current sample by reunion class ---------------------------
  list(
    function_name = "generate_circle_slide",
    metric = NULL,
    category = list(
      name = "reunion_class",
      order = "reunion_class_levels"
    ),
    unit = NULL,
    title = "CURRENT SAMPLE",
    focal_group = list(
      name = focal_group,
      subset = NULL
    )
  ),
  
  # ------ Slide 76: Life Satisfaction by reunion class ------------------------
  list(
    function_name = "generate_circle_slide",
    metric = "life_satisfaction",
    category = list(
      name = "reunion_class",
      order = "reunion_class_levels"
    ),
    unit = NULL,
    title = "LIFE SATISFACTION",
    focal_group = list(
      name = focal_group,
      subset = NULL
    )
  ),
  
  # ------ Slide 77: TQI by reunion class --------------------------------------
  list(
    function_name = "generate_circle_slide",
    metric = "tqi",
    category = list(
      name = "reunion_class",
      order = "reunion_class_levels"
    ),
    unit = NULL,
    title = "TQI",
    focal_group = list(
      name = focal_group,
      subset = NULL
    )
  ),
  
  # ------ Slide 78: Meaningfulness by reunion class ---------------------------
  list(
    function_name = "generate_circle_slide",
    metric = "meaningfulness_work",
    category = list(
      name = "reunion_class",
      order = "reunion_class_levels"
    ),
    unit = "%",
    title = "PERCENT MEETING MEANINGFULNESS",
    focal_group = list(
      name = focal_group,
      subset = NULL
    )
  ),
  
  # ------ Slide 79: Subset reunion classes only -------------------------------
  list(
    function_name = "generate_circle_slide",
    metric = "meaningfulness_work",
    category = list(
      name = "reunion_class",
      order = "reunion_class_levels"
    ),
    unit = "%",
    title = "PERCENT MEETING MEANINGFULNESS",
    focal_group = list(
      name = focal_group,
      subset = list(
        title = "reunion_class",
        value = c(
          "1st", "5th", "10th",
          "15th", "20th", "25th"
        )
      )
    )
  ),
  # ------ Slide 23: High value activities (1st & 10th reunion) ----------------
  list(
    function_name = "generate_tile_slide",
    metric = c(
      "catching_up_hours",
      "reading_hours",
      "care_giving_hours",
      "exercising_hours",
      "chores_with_others_hours",
      "chores_with_family_hours",
      "watching_TV_with_others_hours",
      "watching_TV_alone_hours",
      "eating_with_others_hours"
    ),
    title = "MOST COMMON HIGH VALUE ACTIVITIES",
    focal_group = list(
      name = focal_group,
      subset = list(
        title = "reunion_class",
        value = c("1st", "10th")
      )
    ),
    comparison_groups = NULL,
    preferred_value = "high",
    n_activities = 5
  ),
  
  # ------ Slide 24: High value activities (focal vs comparison) --------------
  list(
    function_name = "generate_tile_slide",
    metric = c(
      "catching_up_hours",
      "reading_hours",
      "care_giving_hours",
      "exercising_hours",
      "chores_with_others_hours",
      "chores_with_family_hours",
      "watching_TV_with_others_hours",
      "watching_TV_alone_hours",
      "eating_with_others_hours"
    ),
    title = "MOST COMMON HIGH VALUE ACTIVITIES",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = list(
      list(
        name = comparison_group_1,
        subset = NULL
      )
    ),
    preferred_value = "high",
    n_activities = 5
  ),
  
  # ------ Slide 25: High value of some reunion focal group  -------------------
  list(
    function_name = "generate_tile_slide",
    metric = c(
      "catching_up_hours",
      "reading_hours",
      "care_giving_hours",
      "exercising_hours",
      "chores_with_others_hours",
      "chores_with_family_hours",
      "watching_TV_with_others_hours",
      "watching_TV_alone_hours",
      "eating_with_others_hours"
    ),
    title = "MOST COMMON HIGH VALUE ACTIVITIES",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = list(
      list(
        name = comparison_group_1,
        subset = list(
          title = "reunion_class",
          value = c(
            "5th", "10th", "15th",
            "20th", "25th", "30th"
          )
        )
      )
    ),
    preferred_value = "high",
    n_activities = 5
  ),
  
  # ------ Slide 26: High value activities (IWF vs HBS gender groups) ----------
  list(
    function_name = "generate_tile_slide",
    metric = c(
      "catching_up_hours",
      "reading_hours",
      "care_giving_hours",
      "exercising_hours",
      "chores_with_others_hours",
      "chores_with_family_hours",
      "watching_TV_with_others_hours",
      "watching_TV_alone_hours",
      "eating_with_others_hours"
    ),
    title = "MOST COMMON HIGH VALUE ACTIVITIES",
    focal_group = list(
      name = "IWF",
      subset = NULL
    ),
    comparison_groups = list(
      list(
        name = comparison_group_1,
        subset = list(title = "gender", value = "Women")
      ),
      list(
        name = comparison_group_1,
        subset = list(title = "gender", value = "Men")
      )
    ),
    preferred_value = "high",
    n_activities = 5
  ),
  
  # ------ Slide 27: High value (3 comparison groups) -----------------------
  list(
    function_name = "generate_tile_slide",
    metric = c(
      "catching_up_hours",
      "reading_hours",
      "care_giving_hours",
      "exercising_hours",
      "chores_with_others_hours",
      "chores_with_family_hours",
      "watching_TV_with_others_hours",
      "watching_TV_alone_hours",
      "eating_with_others_hours"
    ),
    title = "MOST COMMON HIGH VALUE ACTIVITIES",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = list(
      list(name = comparison_group_2, subset = NULL),
      list(name = comparison_group_1, subset = list(title = "gender", value = "Women")),
      list(name = comparison_group_1, subset = list(title = "gender", value = "Men"))
    ),
    preferred_value = "high",
    n_activities = 5
  ),
  
  # ------ Slide 28: Low value activities (1st & 10th) ----------------------
  list(
    function_name = "generate_tile_slide",
    metric = c(
      "social_media_hours",
      "watching_TV_alone_hours",
      "eating_alone_hours",
      "chores_alone_hours",
      "commuting_alone_hours",
      "hanging_out_alone_hours"
    ),
    title = "MOST COMMON HIGH VALUE ACTIVITIES",
    focal_group = list(
      name = focal_group,
      subset = list(
        title = "reunion_class",
        value = c("1st", "10th")
      )
    ),
    comparison_groups = NULL,
    preferred_value = "low",
    n_activities = 6
  ),
  
  # ------ Slide 29: Low value (focal vs comparison) ------------------------
  list(
    function_name = "generate_tile_slide",
    metric = c(
      "social_media_hours",
      "watching_TV_alone_hours",
      "eating_alone_hours",
      "chores_alone_hours",
      "commuting_alone_hours",
      "hanging_out_alone_hours"
    ),
    title = "MOST COMMON HIGH VALUE ACTIVITIES",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = list(
      list(
        name = comparison_group_1,
        subset = NULL
      )
    ),
    preferred_value = "low",
    n_activities = 6
  ),
  
  # ------ Slide 30: Low value (reunion class filter) -----------------------
  list(
    function_name = "generate_tile_slide",
    metric = c(
      "social_media_hours",
      "watching_TV_alone_hours",
      "eating_alone_hours",
      "chores_alone_hours",
      "commuting_alone_hours",
      "hanging_out_alone_hours"
    ),
    title = "MOST COMMON HIGH VALUE ACTIVITIES",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = list(
      list(
        name = comparison_group_1,
        subset = list(
          title = "reunion_class",
          value = c(
            "5th", "10th", "15th",
            "20th", "25th", "30th"
          )
        )
      )
    ),
    preferred_value = "low",
    n_activities = 4
  ),
  
  # ------ Slide 31: Low value (IWF vs gendered comparison) -----------------
  list(
    function_name = "generate_tile_slide",
    metric = c(
      "social_media_hours",
      "watching_TV_alone_hours",
      "eating_alone_hours",
      "chores_alone_hours",
      "commuting_alone_hours",
      "hanging_out_alone_hours"
    ),
    title = "MOST COMMON HIGH VALUE ACTIVITIES",
    focal_group = list(
      name = "IWF",
      subset = NULL
    ),
    comparison_groups = list(
      list(name = comparison_group_1, subset = list(title = "gender", value = "Women")),
      list(name = comparison_group_1, subset = list(title = "gender", value = "Men"))
    ),
    preferred_value = "low",
    n_activities = 5
  ),
  
  # ------ Slide 32: Low value (focal group + 3 comparison groups) -------------
  list(
    function_name = "generate_tile_slide",
    metric = c(
      "social_media_hours",
      "watching_TV_alone_hours",
      "eating_alone_hours",
      "chores_alone_hours",
      "commuting_alone_hours",
      "hanging_out_alone_hours"
    ),
    title = "MOST COMMON HIGH VALUE ACTIVITIES",
    focal_group = list(
      name = focal_group,
      subset = NULL
    ),
    comparison_groups = list(
      list(name = comparison_group_2, subset = NULL),
      list(name = comparison_group_1, subset = list(title = "gender", value = "Women")),
      list(name = comparison_group_1, subset = list(title = "gender", value = "Men"))
    ),
    preferred_value = "low",
    n_activities = 5
  ),
  # ------ Slide 33: Activities horizontal bar for focal group ----------------
  
  list(
    function_name     = "generate_horizontal_bar_slide",
    metric            = c(
      "Volunteering",
      "Therapy",
      "Games",
      "School/learning",
      "Side Projects",
      "Job searching",
      "Napping",
      "Hobbies"
    ),
    subjective_value  = FALSE,
    title             = "DISCRETIONARY TIME – ACTIVITIES",
    x_title           = c("Avg. Hours Per Week"),
    y_title           = "Activity",
    focal_group       = list(name = focal_group, subset = NULL)
  ),
  # ------ Slide 34: Activities hours vs subjective values for focal group -----
  list(
    function_name     = "generate_horizontal_bar_slide",
    metric            = c(
      "Volunteering",
      "Therapy",
      "Games",
      "School/learning",
      "Side Projects",
      "Job searching",
      "Napping",
      "Hobbies"
    ),
    subjective_value  = TRUE,
    title             = "DISCRETIONARY TIME – ACTIVITIES",
    x_title           = c("Avg. Hours Per Week", "Avg. Subjective Value"),
    y_title           = "Activity",
    focal_group       = list(name = focal_group, subset = NULL)
  )
  
)

