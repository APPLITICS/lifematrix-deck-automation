# Automated Slide Deck Generation from Survey Data (Phase 3)

## Overview

This README documents the Phase 3 slide generation pipeline of the LIFE Matrix Deck Automation project. It uses **structured instructions** and **preprocessed survey data** to generate full PowerPoint slides in R via a **reusable, instruction-driven pipeline**, ensuring consistency with the reference deck and Phase 2 architecture.

## Directory Structure
```
R/
├── bar_functions.R            # Bar chart slide generation functions
├── density_functions.R        # Density chart slide generation functions
├── helpers.R                  # Shared utility functions
├── run_pipeline.R             # Core function to build all slides
├── instructions.R             # Structured list of slide instructions

data/
├── simulated_pipeline_input.csv  # Preprocessed survey data for pipeline

provided_reference_deck.pptx   # Reference slide deck for validation
main.R                         # Script to load data and run the pipeline
```
---
## Data Format 

The `simulated_pipeline_input.csv` file contains detailed survey data and is the main data source for the slide generation pipeline. Each row represents a single user input, and each column provides a specific metric or category.

### Column Naming Convention

Metric columns must follow the format:

```
<metric_name>__<measurement_type>
```

Use a double underscore (`__`) to separate the base metric from the measurement. This enables the pipeline to automatically extract and align the correct data for plotting.

### Examples

| Column Name              | Description                              |
| ------------------------ | ---------------------------------------- |
| `joy__high_importance`   | % of respondents rating joy as important |
| `joy__min`               | Minimum target threshold for joy         |
| `life_satisfaction__min` | Minimum benchmark for satisfaction       |

---
## Global Pipeline Structure

The slide generation pipeline is driven by a single function:

```r
run_pipeline(
  data,
  instructions,
  ppt_template_path = "inputs/template.pptx",
  ppt_output_path   = "outputs/generated_slides.pptx"
)
```

This function automates the creation of PowerPoint slides from preprocessed survey data, based on a structured instruction list.

### Input Parameters

- Preprocessed survey data containing cleaned values and derived metrics.
- A parameterized list defining each slide, including chart type, metrics, labels, and subgroup logic. One level of slicing is supported (except for slide 70).
- PowerPoint template path for layout and styling. *Default: "inputs/template.pptx"*
- Output path for the final PowerPoint file. *Default: "outputs/generated\_slides.pptx"*

### Core Logic

- Iterates through each instruction.
- Dynamically calls the appropriate chart function.
- Builds a slide with the correct layout and title.
- Saves the result to `ppt_output_path`.

### Example Usage

```r
run_pipeline(
  data         = pipeline_data,
  instructions = instructions,
  ppt_template_path = "inputs/template.pptx",
  ppt_output_path   = "outputs/generated_slides.pptx"
)
```
---
## Slide Generation Overview

Slides are generated using dedicated functions specified in the `function_name` field of each instruction. The pipeline dynamically dispatches these functions to build complete slides.

---

### A. Density Graphs

**Function:**

```r
generate_density_slide(data, instruction, ppt_doc)
```

**Used In:** Slides 3–5, 29–52, 57–58

#### Key Features

- Plots a density chart for a given metric.
- Shows focal group average (dashed line).
- Adds comparison group averages as annotated green boxes.

#### Example Instruction: Slide 3

```r
list(
  slide             = 3,
  function_name     = "generate_density_slide",
  metric            = "life_satisfaction",
  unit              = "score",
  x_title           = "Life Satisfaction",
  y_title           = "Density",
  title             = "LIFE SATISFACTION",
  focal_group       = list(name = focal_group, subset = NULL),
  comparison_groups = NULL
)
```
#### Example Instruction: Slide 5 (With Comparisons)

```r
list(
  slide             = 5,
  function_name     = "generate_density_slide",
  metric            = "life_satisfaction",
  unit              = "score",
  x_title           = "Life Satisfaction",
  y_title           = "Density",
  title             = "LIFE SATISFACTION",
  focal_group       = list(name = focal_group, subset = NULL),
  comparison_groups = list(
    list(name = comparison_group_1, subset = list(title = "gender", value = "Women")),
    list(name = comparison_group_1, subset = list(title = "gender", value = "Men"))
  )
)
```
### B. Bar Charts

Bar charts are handled using two functions depending on whether the x-axis represents **metrics** or **categories**.

#### 1. Metric-Based X-Axis

**Function:**

```r
generate_bar_metric_slide(data, instruction, ppt_doc)
```

**Used In:** Slides 8–12, 62–65

- Compares multiple metrics across groups.
- X-axis = metric names.
- Optional: target lines, placeholder groups.

**Example: Slide 12**

```r
list(
  slide             = 12,
  function_name     = "generate_bar_metric_slide",
  metric            = c("joy", "achievement", "meaningfulness"),
  unit              = "%",
  title             = "IMPORTANCE OF JAM",
  y_title           = "% High Importance",
  focal_group       = list(name = focal_group, subset = NULL),
  comparison_groups = list(
    list(name = comparison_group_1, subset = list(title = "gender", value = "Women")),
    list(name = NA, subset = NULL)
  ),
  bar_value         = "value_high_importance",
  target            = NULL
)
```

**Example: Slide 14 with Targets**

```r
list(
  slide             = 14,
  function_name     = "generate_bar_metric_slide",
  metric            = c("joy", "achievement", "meaningfulness"),
  unit              = "%",
  title             = "IMPORTANCE OF JAM",
  y_title           = "% High Importance",
  focal_group       = list(name = focal_group, subset = NULL),
  comparison_groups = list(
    list(name = comparison_group_1, subset = list(title = "gender", value = "Women")),
    list(name = comparison_group_1, subset = list(title = "gender", value = "Men"))
  ),
  bar_value         = "value_high_importance",
  target            = "value_min"
)
```

#### 2. Category-Based X-Axis

**Function:**

```r
generate_bar_category_slide(data, instruction, ppt_doc)
```

**Used In:** Slides 67, 72, 73

- X-axis = category values (e.g., gender, income, reunion class).
- One metric split across categories.
- Supports trend lines and flexible grouping.

**Example: Slide 67 with Income Grouping + Trend**

```r
list(
  slide     = 67,
  function_name = "generate_bar_category_slide",
  metric    = "life_satisfaction",
  category  = list(
    name     = "income",
    grouping = c("Less than 150K", "150K – 250K", "250K – 500K", "500K - 1M", "More than 1M")
  ),
  unit        = "measure",
  title       = "LIFE SATISFACTION BY INCOME",
  x_title     = "Income ($)",
  y_title     = "Life Satisfaction",
  focal_group = list(name = focal_group, subset = NULL),
  trend_line  = TRUE
)
```

**Example: Slide 72 without Y-Axis**

```r
list(
  slide     = 72,
  function_name = "generate_bar_category_slide",
  metric    = "n_children",
  category  = list(name = "reunion_class", grouping = NULL),
  unit        = "measure",
  title       = "KIDS - AVERAGE NUMBER BY REUNION CLASS",
  x_title     = "Number of Children",
  y_title     = NULL,
  focal_group = list(name = focal_group, subset = NULL),
  trend_line  = FALSE
)
```

**Example: Slide 73 with Grouped Numeric Category**

```r
list(
  slide     = 73,
  function_name = "generate_bar_category_slide",
  metric    = "life_satisfaction",
  category  = list(
    name = "n_children",
    grouping = c("3+")
  ),
  unit        = "measure",
  title       = "LIFE SATISFACTION BY NUMBER OF CHILDREN",
  x_title     = "Number of Children",
  y_title     = "Life Satisfaction",
  focal_group = list(name = focal_group, subset = NULL),
  trend_line  = TRUE
)
```
---
## More Examples & Testing

- Use `main.R` to test the pipeline on `instructions.R`.
- The `instructions.R` file provides real-case slides for direct mapping to `automation.pptx`.
- Examples include focal-only, subgroup comparisons, placeholders, trend lines, and targets.

