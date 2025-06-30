# ------ LIBRARIES -------------------------------------------------------------
library(data.table)
library(dplyr)
library(ggplot2)
library(grid)

# ------ LOAD FUNCTIONS --------------------------------------------------------
invisible(
  lapply(
    list.files(
      "R",
      pattern    = "\\.R$",
      full.names = TRUE
    ),
    source
  )
)

# ------ LOAD DATA -------------------------------------------------------------
df_density <- fread("simulated_data/density_data.csv")
df_bar     <- fread("simulated_data/bar_data.csv")

focal_group        <- "Xilio"
comparison_group_1 <- "HBS"
comparison_group_2 <- "IWF"

# ------ GENERATE CHARTS -------------------------------------------------------

## Slide 3: Density chart with gender comparison -------------------------------
draw_density_chart(
  df   = df_density,
  inst = instructions[[1]]
)

## Slide 5: Density chart with gender comparison -------------------------------
draw_density_chart(
  df   = df_density,
  inst = instructions[[2]]
)

## Slide 8: Focal group only (no comparison)  ----------------------------------
draw_bar_chart(
  df   = df_bar,
  inst = instructions[[3]]
)

## Slide 12: One real + one placeholder comparison group (no subset) -----------
draw_bar_chart(
  df   = df_bar,
  inst = instructions[[4]]
)

## Slide 13: Two real comparison groups with gender subsets (no target line) ---
draw_bar_chart(
  df   = df_bar,
  inst = instructions[[5]]
)

## Slide 18: Two real comparison groups with gender subsets + target line ------
draw_bar_chart(
  df   = df_bar,
  inst = instructions[[6]]
)

## Slide 65: Two real comparison groups (same group, different subsets) --------
draw_bar_chart(
  df   = df_bar,
  inst = instructions[[7]]
)

## One real + one placeholder comparison group, trend line enabled ------------
draw_bar_chart(
  df   = df_bar,
  inst = instructions[[8]]
)

## Slide 67: Bar chart by income (focal group only), trend line enabled --------
draw_bar_chart(
  df   = df_bar,
  inst = instructions[[9]]
)

## Slide 73: Bar chart by number of kids (focal group only),trend line enabled -
draw_bar_chart(
  df   = df_bar,
  inst = instructions[[10]]
)

## Slide 74: Bar chart by reunion class (focal group only) ---------------------
draw_bar_chart(
  df   = df_bar,
  inst = instructions[[11]]
)
