ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  shinyjs::useShinyjs(),
  includeCSS("www/css/custom.css"),
  
  # ------ CUSTOM TITLE BAR ----------------------------------------------------
  tags$div(
    class = "app-title",
    "Automated Slide Builder"
  ),
  
  # ------ APP DESCRIPTION -----------------------------------------------------
  div(
    class = "description",
    p(
      "This application automates the creation of PowerPoint slides, ",
      "ensuring speed and consistency. Choose a chart type and set its ",
      "parameters in the sidebar. Review the live preview before adding ",
      "it to your deck."
    )
  ),
  
  # ------ LAYOUT --------------------------------------------------------------
  sidebarLayout(
    
    # ------ SIDEBAR -----------------------------------------------------------
    sidebarPanel(
      width = 4,
      bslib::card(
        bslib::card_body(
          h3("Slide Builder Controls", class = "sidebar-title"),
          
          # Function selector
          pickerInput(
            inputId = "fn_choice",
            label = "Slide function",
            choices = slide_functions,
            options = list(`live-search` = TRUE),
            multiple = FALSE
          ),
          
          # Dynamic module UI placeholder
          uiOutput("module_ui"),
          
          tags$hr(),
          fluidRow(
            column(
              width = 6,
              actionButton(
                "build_graph",
                "Build Graph",
                class = "btn-primary w-100"
              )
            ),
            column(
              width = 6,
              actionButton(
                "append_slide",
                "Append Slide",
                class = "btn-primary w-100"
              )
            )
          )
        )
      )
    ),
    
    # ------ MAIN PANEL --------------------------------------------------------
    mainPanel(
      class = "main-panel",
      bslib::card(
        bslib::card_header(
          h3("Slide Preview"),
          p(
            "This section shows a live preview of your slide. ",
            "Adjust parameters on the left and watch the preview ",
            "update before you add it to your deck."
          )
        ),
        bslib::card_body(
          div(
            class = "center-plot",
            plotOutput(
              "slide_preview",
              width = "100%",
              height = "100%"
            )
          )
        )
      )
    )
  )
)
