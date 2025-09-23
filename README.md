# Automated Slide Deck Generation (Phase 4 – Shiny App)

## Overview

This README documents the **Phase 4 implementation of the LIFE Matrix project**, where the automated slide pipeline from Phase 3 was extended with a **Shiny application**. The app provides a clear, professional, and user‑friendly interface that allows users to set parameters, preview charts instantly, and append slides sequentially.
This builds the PowerPoint deck efficiently without editing the underlying code.

## Folder Structure
```
R/
├── 10_mod_density_*                 # Density chart module (ui, server, function)
├── 20_mod_circle_*                  # Circle chart module (ui, server, function)
├── 30_mod_tile_*                    # Tile chart module (ui, server, function)
├── 40_mod_scatter_*                 # Scatter chart module (ui, server, function)
├── 50_mod_donut_*                   # Donut chart module (ui, server, function)
├── 60_mod_line_*                    # Line chart module (ui, server, function)
├── 70_mod_bar_metric_*              # Metric-based bar chart module
├── 71_mod_bar_category_*            # Category-based bar chart module
├── 72_mod_bar_horizontal_*          # Horizontal bar chart module
├── 73_mod_bar_stacked_vertical_*   # Stacked vertical bar chart module
├── 74_mod_bar_stacked_horizontal_*  # Stacked horizontal bar chart module
├── helpers_data.R                   # Data helpers (e.g., NA normalization)
├── helpers_graph.R                  # Chart helper functions (themes, scales, etc.)
└── helpers_modules.R                # Shared UI/server utilities (e.g., module_registry)

data/
└── simulated_pipeline_input.csv   # Preprocessed survey data for pipeline

inputs/
├── template.pptx                  # PowerPoint template for slide layout
└── mapping_file.csv               # Mapping of survey metrics to display labels

output/
├── generated_slides.pptx          # Generated PowerPoint deck
└── generated_instructions/        # Saved `.rds` instruction checkpoints

www/
└── css/custom.css                 # Custom CSS for Shiny app styling

provided_reference_deck.pptx       # Reference slide deck for validation
reference_instructions.R           # Reference instructions list
```
## Features and Improvements

* **Build slides interactively**: Configure parameters with dynamic, intuitive controls that adapt to the chosen slide function and subgroup.
* **Preview before committing**: Generate a real-time graph preview and review it in the main panel before adding it to the deck.
* **Save reproducible checkpoints**: Each preview automatically saves the configuration as an `.rds` file in `outputs/generated_instructions/`.
* **Grow the deck slide by slide**: Append slides into a session-specific `.pptx` file, ensuring no conflicts when multiple sessions are active.
* **Stay informed of issues**: Receive clear notifications if inputs are invalid or if saving to PowerPoint fails (e.g., when the file is open).
* **Work without coding**: Replace manual editing of `instructions.R` with a point-and-click interface that updates options dynamically.
* **Validate inputs dynamically**: Prevent invalid selections (e.g., missing categories) with UI elements that adjust to available options.
* **Extend easily**: Uses the `generate_*_slide` functions built in **Phase 3**, registered once in the central `module_registry.R`, and automatically exposed in the Shiny app. This makes it simple to expand with new chart types.

## Workflow

1. Run the app with `shiny::runApp()`.
2. Choose a slide function from the sidebar. Each option corresponds to a `generate_*_slide` function from Phase 3.
3. Set parameters such as metrics, categories, and subsets.
4. Click **Build Graph** to call the underlying slide function, generate a preview, and save its instruction.
5. Use **Append Slide** to add the previewed slide to the active PowerPoint deck.
