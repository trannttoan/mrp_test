#' analyze_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analyze_upload_ui <- function(id){
  ns <- NS(id)
  tags$div(class = "pad_top",
    sidebarLayout(
      sidebarPanel(width = 3,
        fileInput(
          inputId = ns("input_data"),
          label = "Choose a file (CSV/Excel/SAS)",
          accept = c(".csv", ".xlsx", ".sas7bdat")
        ),
        tags$p(
          "For ", tags$u("requirements for input data"), "and preprocessing code, go to the",
          actionLink(
            inputId = ns("to_interface"),
            label = "Interface",
            class = "action_link"
          ),
          "page. For a detailed description of the prepropressing procedure, go to the",
          actionLink(
            inputId = ns("to_preprocess"),
            label = "Preprocessing",
            class = "action_link"
          ),
          "page."
        )
      ),
      mainPanel(width = 9,
        uiOutput(outputId = ns("main_panel"))
      )
    )
  )
}

#' analyze_upload Server Functions
#'
#' @noRd
mod_analyze_upload_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    rawdata <- reactiveVal()

    output$main_panel <- renderUI({
      req(input$input_data)

      tagList(
        tags$div(
          class = "justify",
          shinyWidgets::radioGroupButtons(
            inputId = ns("toggle_table"),
            label = NULL,
            choices = c("Raw", "Modified")
          ),
          shinyBS::bsTooltip(ns("toggle_table"), "\"Modified\" table only shows when input meets all requirements.", placement = "right"),
          tags$p("*The table only shows a subset of the data")
        ),
        DT::dataTableOutput(outputId = ns("table"))
      )
    })


    output$table <- DT::renderDataTable({
      df <- if(input$toggle_table == "Raw") rawdata() else global$data

      df |>
        head(100) |>
        DT::datatable(
          options = list(
            scrollX = TRUE,
            lengthChange = FALSE
          )
        )
    })

    observeEvent(input$input_data, {

      # reset variables
      global$data <- NULL
      global$mrp_input <- NULL
      global$plotdata <- list()


      path <- input$input_data$datapath

      # read in data
      if(stringr::str_ends(path, "csv")) {
        read.csv(path) |> rawdata()
      } else if (stringr::str_ends(path, "(xlsx|xls)")) {
        readxl::read_excel(path) |> rawdata()
      } else if (stringr::str_ends(path, "sas7bdat")) {
        haven::read_sas(path) |> rawdata()
      } # else no necessary due to fileInput constraint

      # check input data
      errors <- check_input_data(
        df = rawdata(),
        expected_columns = global$static$expected_columns,
        demo_levels = global$static$levels
      )

      if(length(errors) == 0) {
        title <- tagList(icon("bell", "fa"), "Notification")
        content <- "All requirements are met. You may proceed to the next page."

        renamed <- rawdata() |> find_columns(global$static$expected_columns)
        global$data <- renamed |> prep(global$static$levels, unique(global$extdata$zip_tract$zip))
        global$plotdata$dates <- create_timeline(renamed)

      } else if(length(errors) == 1 & "date" %in% names(errors)) {
        title <- tagList(icon("triangle-exclamation", "fa"), "Warning")
        content <- errors$date

        global$data <- rawdata() |>
          find_columns(global$static$expected_columns) |>
          prep(global$static$levels, unique(global$extdata$zip_tract$zip)) |>
          select(-date)

      } else {
        title <- tagList(icon("triangle-exclamation", "fa"), "Warning")
        content <- tagList(
          tags$p("The input data has the following errors:"),
          tags$ul(
            purrr::map(unlist(errors), ~ tags$li(.x))
          )
        )
      }

      showModal(
        modalDialog(
          title = title,
          content
        ),
        session = global$session
      )


    })


    observeEvent(input$to_interface, {
      updateNavbarPage(global$session,
        inputId = "navbar",
        selected = "nav_learn_interface"
      )
    })

    observeEvent(input$to_preprocess, {
      updateNavbarPage(global$session,
        inputId = "navbar",
        selected = "nav_learn_preprocess"
      )
    })

  })
}

## To be copied in the UI
# mod_analyze_upload_ui("analyze_upload_1")

## To be copied in the server
# mod_analyze_upload_server("analyze_upload_1")
