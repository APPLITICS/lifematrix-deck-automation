# ==============================================================================
# LIFE Matrix – UI (with function picker)
# ==============================================================================

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  useShinyjs(),
  includeCSS("www/css/custom.css"),  # External CSS file
  
  # ------ Custom Title Bar ---------------------------------------------------
  tags$div(
    class = "app-title",
    "Automated Slide Builder"
  ),  
  div(
    class = "description",
    p("This application automates the creation of PowerPoint slides, 
      ensuring speed and consistency. Choose a chart type and set its parameters 
      in the sidebar. Review the live preview before adding it to your deck.")
  ),
  
  sidebarLayout(
    # ------ SIDEBAR -----------------------------------------------------------
    sidebarPanel(
      width = 4,
      card(
        card_body(
          # Sidebar title (styled globally in CSS)
          h3("Slide Builder Controls", class = "sidebar-title"),
          
          # ------ Function selector -------------------------------------------
          pickerInput(
            inputId = "fn_choice",
            label   = "Slide function",
            choices = c(
              "generate_density_slide",
              "generate_bar_metric_slide",
              "generate_bar_category_slide",
              "generate_circle_slide",
              "generate_donut_slide",
              "generate_line_slide",
              "generate_stacked_vertical_slide",
              "generate_stacked_horizontal_slide",
              "generate_horizontal_bar_slide",
              "generate_scatter_slide",
              "generate_tile_slide"
            ),
            options  = list(`live-search` = TRUE),
            multiple = FALSE
          ),
          
          # ------ Dynamic module UI placeholder -------------------------------
          uiOutput("module_ui"),
          
          tags$hr(),
          actionButton("build_slide", "Build Slide", class = "btn-primary")
        )
      )
    ),
    
    # ------ MAIN PANEL --------------------------------------------------------
    mainPanel(
      class = "main-panel",
      card(
        card_header(
          h3("Slide Preview"),
          p("This section shows a live preview of your slide. 
             Adjust parameters on the left and watch the preview update 
             before you add it to your deck.")
        ),
        card_body(
          div(
            class = "center-plot",
            plotOutput("slide_preview", width = "100%", height = "100%")
          )
        ),
        div(
          class = "card-footer",
          actionButton("append_slide", "Append Slide", class = "btn-primary")
        )
      )
    )
  )
)
