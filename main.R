# ------ LIBRARIES ----------------------------------------------------------
library(data.table)
library(dplyr)
library(ggplot2)

# ------ LOAD FUNCTIONS -----------------------------------------------------
invisible(
  lapply(
    list.files(
      "R",
      pattern     = "\\.R$",
      full.names  = TRUE
    ),
    source
  )
)

# ------ LOAD DATA ----------------------------------------------------------
df <- fread("simulated_data/density_data.csv")

# ------ INSTRUCTION --------------------------------------------------------
instructions <- list(
  list(
    slide             = 3,
    slide_type        = "density_chart",
    metric            = "life_satisfaction",
    unit              = NULL,
    x_title           = "Life Satisfaction",
    y_title           = "Density",
    focal_group       = list(
      name   = "Xilio",
      subset = NULL
    ),
    comparison_groups = list(
      list(
        name   = "HBS",
        subset = list(title = "gender", value = "Women")
      ),
      list(
        name   = "HBS",
        subset = list(title = "gender", value = "Men")
      )
    )
  )
)

# ------ DRAW CHART ---------------------------------------------------------
draw_density_chart(
  df   = df,
  inst = instructions[[1]]
)
