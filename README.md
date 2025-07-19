# Automated Slide Deck Generation from Survey Data (Phase 3)

## Overview

This README documents the Phase 3 slide generation pipeline of the LIFE Matrix Deck Automation project. It uses **structured instructions** and **preprocessed survey data** to generate full PowerPoint slides in R via a **reusable, instruction-driven pipeline**, ensuring consistency with the reference deck and Phase 2 architecture.

## Directory Structure
```
R/
├── bar_functions.R            # Bar chart slide generation functions
├── density_functions.R        # Density chart slide generation functions
|── ...
├── helpers.R                  # Shared utility functions
├── run_pipeline.R             # Core function to build all slides
├── instructions.R             # Structured list of slide instructions

data/
├── simulated_pipeline_input.csv  # Preprocessed survey data for pipeline

inputs/
├── template.pptx              # PowerPoint template for slide layout
├── mapping.csv                # Mapping of survey metrics to slide displayed labels

output/
├── generated_slides.pptx      # Output PowerPoint file with generated slides

provided_reference_deck.pptx   # Reference slide deck for validation
main.R                         # Script to load data and run the pipeline
```
---
## Data Format 

- The `simulated_pipeline_input.csv` file contains detailed survey data and is the main data source for the slide generation pipeline. Each row represents a participant, and each column provides a specific metric or category.
- The `mapping_file.csv` file provides a mapping of survey data columns, whether metrics or categories, to their corresponding display labels used in the slides. This ensures a direct and consistent translation of internal variable names to presentation-friendly labels.

### Preprocessing Requirements

Any metric that appears as a categorical variable in charts (i.e., used on the x-axis) must be **pre-grouped in the data input**.
Examples:

- `children_range` is a preprocessed grouping of the numeric metric `n_children`
- `income_range` is a preprocessed grouping of the numeric metric `income`

These grouped columns must be present in the `simulated_pipeline_input.csv`.

### Category Ordering

To ensure proper ordering of grouped categories, the data input must include a dedicated column specifying the display order.

Example:


| children_range | children_range_levels |
|----------------|------------------------|
| 0              | 1                      |
| 1              | 2                      |
| 2              | 3                      |
| 3+             | 4                      |

Where column `children_range_levels` defines the order in which categories appear on the x-axis of bar charts.

This ordering column is referenced in the instructions list as:

```r
category = list(
  name  = "children_range",
  order = "children_range_levels"
)
```
Both the grouped values and their numeric ordering must already be present in the input file. The pipeline does not derive or sort them.

---
## Global Pipeline Structure

The slide generation pipeline is driven by a single function:

```r
run_pipeline(
  data,
  instructions,
  ppt_template_path = "inputs/template.pptx",
  ppt_output_path = "outputs/generated_slides.pptx"
)
```

This function automates the creation of PowerPoint slides from preprocessed survey data, based on a structured instruction list.

### Input Parameters

- **`data`**: A preprocessed survey data containing cleaned values and derived metrics.
- **`instructions`**: A parameterized list defining each slide, including chart type, metrics, labels, and subgroup logic. One level of slicing is supported (except for slide 70).
- **`ppt_template_path`**: PowerPoint template path for layout and styling. *Default: "inputs/template.pptx"*
- **`ppt_output_path`**: Output path for the final PowerPoint file. *Default: "outputs/generated\_slides.pptx"*

### Core Logic

- Iterates through each instruction.
- Dynamically calls the appropriate chart function.
- Builds a slide with the correct layout and title.
- Saves the result to `ppt_output_path`.

---
## Slide Generation Overview

Slides are generated using dedicated functions specified in the `function_name` field of each instruction. The pipeline dynamically dispatches these functions to build complete slides.

---

### A. Density Graphs

**Function:**

```r
generate_density_slide(
  data,
  instruction,
  ppt_doc
)
```

**Used In:** Slides 3–5, 29–52, 57–58

#### Key Features

- Plots a density chart for a given metric.
- Shows focal group average (dashed line).
- Adds comparison group averages as annotated green boxes.

#### Example Instruction: Slide 3

```r
list(
  function_name = "generate_density_slide",
  metric        = "life_satisfaction",
  x_title       = "Life Satisfaction",
  y_title       = "Density",
  title         = "LIFE SATISFACTION",
  focal_group   = list(
    name   = focal_group,
    subset = NULL
  ),
  comparison_groups = NULL
)
```
#### Example Instruction: Slide 5 (With Comparisons)

```r
list(
  function_name = "generate_density_slide",
  metric        = "life_satisfaction",
  x_title       = "Life Satisfaction",
  y_title       = "Density",
  title         = "LIFE SATISFACTION",
  focal_group   = list(
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
)
```
### B. Bar Charts

Bar charts are handled using two functions depending on whether the x-axis represents **metrics** or **categories**.

#### 1. Metric-Based X-Axis

**Function:**

```r
generate_bar_metric_slide(
  data,
  instruction,
  ppt_doc
)
```

**Used In:** Slides 8–12, 62–65

Based on the provided PowerPoint file, this function includes:
- Compares multiple metrics across groups.
- X-axis = metric names.
- Optional: target lines, placeholder groups.

#### Example: Slide 12

```r
list(
  function_name = "generate_bar_metric_slide",
  bar_value     = c("joy", "achievement", "meaningfulness"),
  target        = NULL,
  unit          = "%",
  title         = "IMPORTANCE OF JAM",
  y_title       = "% High Importance",
  focal_group   = list(
    name   = focal_group,
    subset = NULL
  ),
  comparison_groups = list(
    list(
      name   = comparison_group_1,
      subset = list(title = "gender", value = "Women")
    ),
    list(
      name   = NA,
      subset = NULL
    )
  )
)
```

#### Example: Slide 14 with Targets

```r
list(
  function_name = "generate_bar_metric_slide",
  bar_value     = c("joy", "achievement", "meaningfulness"),
  target        = c("joy_min", "achievement_min", "meaningfulness_min"),
  unit          = "%",
  title         = "IMPORTANCE OF JAM",
  y_title       = "% High Importance",
  focal_group   = list(
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
)
```
#### 2. Category-Based X-Axis

**Function:**

```r
generate_bar_category_slide(
  data,
  instruction,
  ppt_doc
)
```

**Used In:** Slides 67, 72, 73

Based on the provided PowerPoint file, this function includes:
- Plots a bar chart with categories on the x-axis.
- X-axis = category values (e.g., `gender`, `income_range`, `reunion_class`).
- One metric for **the focal group**, **no comparison groups**, split across categories.
- Does not support comparison groups
- Supports trend lines.

#### Example: Slide 67 with Income Grouping + Trend

```r
list(
  function_name = "generate_bar_category_slide",
  metric        = "life_satisfaction",
  category      = list(
    name  = "income_range",
    order = "income_range_levels"
  ),
  unit          = NULL,
  title         = "LIFE SATISFACTION BY INCOME",
  x_title       = "Income ($)",
  y_title       = "Life Satisfaction",
  focal_group   = list(
    name   = focal_group,
    subset = NULL
  ),
  trend_line    = TRUE
)
```

#### Example: Slide 72 without Y-Axis

```r
list(
  function_name = "generate_bar_category_slide",
  metric        = "n_children",
  category      = list(
    name  = "reunion_class",
    order = "reunion_class_levels"
  ),
  unit          = NULL,
  title         = "KIDS - AVERAGE NUMBER BY REUNION CLASS",
  x_title       = "Reunion Class",
  y_title       = "Number of Children",
  focal_group   = list(
    name   = focal_group,
    subset = NULL
  ),
  trend_line    = FALSE
)
```

#### Example: Slide 73 with Grouped Numeric Category

```r
list(
  function_name = "generate_bar_category_slide",
  metric        = "life_satisfaction",
  category      = list(
    name  = "children_range",
    order = "children_range_levels"
  ),
  unit          = NULL,
  title         = "LIFE SATISFACTION BY NUMBER OF CHILDREN",
  x_title       = "Number of Children",
  y_title       = "Life satisfaction",
  focal_group   = list(
    name   = focal_group,
    subset = NULL
  ),
  trend_line    = TRUE
)
```


### C. Circle Graphs

**Function:**

```r
generate_circle_slide(
  data,
  instructions,
  ppt_doc
)
```

**Used In:** Slides 75–78

#### Key Features

- Displays one value per category for the focal group.
- If `metric` is provided, shows the summary value (e.g., mean) of that metric per category.
- If `metric = NULL`, shows the **count of participant** per category for the focal group.
- Supports subsetting focal group data by category or other variables.

#### Example: Slide 75 (Counts per Category)

```r
list(
  function_name = "generate_circle_slide",
  metric        = NULL,
  category      = list(
    name  = "reunion_class",
    order = "reunion_class_levels"
  ),
  unit          = NULL,
  title         = "CURRENT SAMPLE",
  focal_group   = list(
    name   = focal_group,
    subset = NULL
  )
)
```

#### Example: Slide 76 (Metric-Based Summary)

```r
list(
  function_name = "generate_circle_slide",
  metric        = "life_satisfaction",
  category      = list(
    name  = "reunion_class",
    order = "reunion_class_levels"
  ),
  unit          = NULL,
  title         = "LIFE SATISFACTION",
  focal_group   = list(
    name   = focal_group,
    subset = NULL
  )
)
```

#### Example: Slide 78 (Filtered Categories)

```r
list(
  function_name = "generate_circle_slide",
  metric        = "meaningfulness_work",
  category      = list(
    name  = "reunion_class",
    order = "reunion_class_levels"
  ),
  unit          = "%",
  title         = "PERCENT MEETING MEANINGFULNESS",
  focal_group   = list(
    name   = focal_group,
    subset = list(
      title = "reunion_class",
      value = c("1st", "5th", "10th", "15th", "20th", "25th")
    )
  )
)
```
### D. Tile Charts

**Function:**

```r
generate_tile_slide(
    data,
    instruction,
    ppt_doc
)
```

**Used In:** Slides 24–32

#### Key Features

- **Focal or comparison view**: Show top activities for one group or compare up to 4 groups side by side.
- **Top activity selection**: Automatically selects and ranks the top `n_activities` based on metric values.
- **Color by preference**: Changes tile color based on whether higher or lower values are preferred.
- **Group filtering**: Supports filtering groups by variables like gender or reunion class.

#### Example 1: Focal vs Comparison (High Value Activities)

```r
list(
    function_name = "generate_tile_slide",
    metric        = c(
        "catching_up_hours", "reading_hours", "care_giving_hours",
        "exercising_hours", "chores_with_others_hours",
        "chores_with_family_hours", "watching_TV_with_others_hours",
        "watching_TV_alone_hours", "eating_with_others_hours"
    ),
    title         = "MOST COMMON HIGH VALUE ACTIVITIES",
    focal_group   = list(
        name   = focal_group,
        subset = NULL
    ),
    comparison_groups = list(
        list(
            name   = comparison_group_1,
            subset = NULL
        )
    ),
    preferred_value = "high",
    n_activities    = 5
)
```
#### Example 2: Filtered Comparison by Category

```r
list(
    function_name = "generate_tile_slide",
    metric        = c(
        "catching_up_hours", "reading_hours", "care_giving_hours",
        "exercising_hours", "chores_with_others_hours",
        "chores_with_family_hours", "watching_TV_with_others_hours",
        "watching_TV_alone_hours", "eating_with_others_hours"
    ),
    title         = "MOST COMMON HIGH VALUE ACTIVITIES",
    focal_group   = list(
        name   = focal_group,
        subset = NULL
    ),
    comparison_groups = list(
        list(
            name   = comparison_group_1,
            subset = list(
                title = "reunion_class",
                value = c("5th", "10th", "15th", "20th", "25th", "30th")
            )
        )
    ),
    preferred_value = "high",
    n_activities    = 5
)
```
#### Example 3: Focal + 3 Comparison Groups (Low Value Activities)

```r
list(
    function_name = "generate_tile_slide",
    metric        = c(
        "social_media_hours", "watching_TV_alone_hours", "eating_alone_hours",
        "chores_alone_hours", "commuting_alone_hours", "hanging_out_alone_hours"
    ),
    title         = "MOST COMMON HIGH VALUE ACTIVITIES",
    focal_group   = list(
        name   = focal_group,
        subset = NULL
    ),
    comparison_groups = list(
        list(name = comparison_group_2, subset = NULL),
        list(name = comparison_group_1, subset = list(title = "gender", value = "Women")),
        list(name = comparison_group_1, subset = list(title = "gender", value = "Men"))
    ),
    preferred_value = "low",
    n_activities    = 5
)
```
### E. Horizontal Bar Charts

**Function:**

```r
generate_horizontal_bar_slide(
    data,
    instruction,
    ppt_doc
)
```

**Used In:** Slides 53–54

#### Key Features

- **Focal group only**: This function displays data for a single group without including any comparisons to others. It is designed for clear and focused group-specific insights.
- **Supports hours and subjective values**: Set `subjective_value` to `TRUE` to include both average weekly hours and corresponding subjective values on separate x-axes, or `FALSE` to show only the hour values.

#### Example 1: Hours Only (Focal Group)

```r
list(
    function_name    = "generate_horizontal_bar_slide",
    metric           = c(
        "Volunteering",
        "Therapy",
        "Games",
        "School/learning",
        "Side Projects",
        "Job searching",
        "Napping",
        "Hobbies"
    ),
    subjective_value = FALSE,
    title            = "DISCRETIONARY TIME – ACTIVITIES",
    x_title          = c("Avg. Hours Per Week"),
    y_title          = "Activity",
    focal_group      = list(
        name   = focal_group,
        subset = NULL
    )
)
```

#### Example 2: Hours + Subjective Value (Focal Group)

```r
list(
    function_name    = "generate_horizontal_bar_slide",
    metric           = c(
        "Volunteering",
        "Therapy",
        "Games",
        "School/learning",
        "Side Projects",
        "Job searching",
        "Napping",
        "Hobbies"
    ),
    subjective_value = TRUE,
    title            = "DISCRETIONARY TIME – ACTIVITIES",
    x_title          = c("Avg. Hours Per Week", "Avg. Subjective Value"),
    y_title          = "Activity",
    focal_group      = list(
        name   = focal_group,
        subset = NULL
    )
)
```
### F. Donut Charts

**Function:**

```r
generate_donut_slide(
    data,
    instruction,
    ppt_doc
)
```

**Used In:** Slides 81–83

#### Key Features

- Supports focal and comparison groups to display category distributions across one or multiple segments.
- Handles empty or undefined comparison groups by inserting placeholders to preserve layout structure.
- Accepts optional category ordering through the `order` field.

#### Example 1: Focal Group Only

```r
list(
    function_name  = "generate_donut_slide",
    category       = list(
        name   = "jam_type_distribution",
        order  = NULL
    ),
    title          = "JAM TYPE DISTRIBUTION",
    focal_group    = list(
        name    = focal_group,
        subset  = NULL
    ),
    comparison_groups = NULL
)
```

#### Example 2: Focal Group + Placeholder

```r
list(
    function_name  = "generate_donut_slide",
    category       = list(
        name   = "jam_type_distribution",
        order  = NULL
    ),
    title          = "JAM TYPE DISTRIBUTION",
    focal_group    = list(
        name    = focal_group,
        subset  = NULL
    ),
    comparison_groups = list(
        list(
            name    = NA,
            subset  = NULL
        )
    )
)
```

#### Example 3: Focal Group + Comparison Group with Subsets

```r
list(
    function_name  = "generate_donut_slide",
    category       = list(
        name   = "jam_type_distribution",
        order  = NULL
    ),
    title          = "JAM TYPE DISTRIBUTION",
    focal_group    = list(
        name    = focal_group,
        subset  = NULL
    ),
    comparison_groups = list(
        list(
            name    = comparison_group_1,
            subset  = list(
                title  = "gender",
                value  = "Women"
            )
        ),
        list(
            name    = comparison_group_1,
            subset  = list(
                title  = "gender",
                value  = "Men"
            )
        )
    )
)
```
### F. Line Charts

**Function:**

```r
generate_line_slide(
    data,
    instruction,
    ppt_doc
)
```

**Used In:** Slides 84–87

#### Key Features

- Supports one or multiple **metrics** plotted as separate lines across a categorical x-axis (e.g., income levels).
- Filters data by **focal group**, with optional **subset filtering** (e.g., Women only).
- Accepts optional **category ordering** via `instruction$category$order`.

#### Example 1: Perceived vs Behavioral Achievement

```r
list(
    function_name  = "generate_line_slide",
    metric         = c(
        "achievement_behavioral",
        "achievement_perceived"
    ),
    category       = list(
        name   = "income_range",
        order  = "income_range_levels"
    ),
    title          = "PERCEIVED VS BEHAVIORAL ACHIEVEMENT",
    y_title        = "Achievement Rating",
    focal_group    = list(
        name    = focal_group,
        subset  = list(
            title  = "gender",
            value  = "Women"
        )
    )
)
```
#### Example 2: JAM Gap Distribution Across Income Levels

```r
list(
    function_name  = "generate_line_slide",
    metric         = c(
        "joy_gap",
        "achievement_gap",
        "meaningfulness_gap"
    ),
    category       = list(
        name   = "income_range",
        order  = "income_range_levels"
    ),
    unit           = "%",
    title          = "GAP IN PERCEIVED VS BEHAVIORAL JAM",
    y_title        = "Gap",
    focal_group    = list(
        name    = focal_group,
        subset  = NULL
    )
)
```
---
## More Examples & Testing

- Use `main.R` to test the pipeline on `instructions.R`.
- The `instructions.R` file provides real-case slides for direct mapping to `automation.pptx`.
- Examples include focal-only, subgroup comparisons, placeholders, trend lines, and targets.