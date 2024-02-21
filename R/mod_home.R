#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(
      class = "landing_container",
      tags$h1("M.R.P.", class = "landing_header"),
      tags$p("A user-friendly graphical interface for applying Multilevel Regression and Poststratification (MRP) to your dataset.", class = "landing_subheader"),
      tags$p("In the absence of any successful public or academic campaign for comprehensive or random testing, we have developed a proxy method for synthetic random sampling, based on viral RNA testing of patients who present for elective procedures within a hospital system. We present here an approach under MRP to collecting and analyzing data on viral exposure among patients in a hospital system and performing statistical adjustment to estimate true viral incidence and trends in the community.", class = "landing_text"),
      tags$div(
        class = "landing_buttons",
        actionButton(
          inputId = ns("to_analyze"),
          label = "Start analyzing",
          class = "landing_button"
        ),
        actionButton(
          inputId = ns("to_learn_mrp"),
          label = "Learn about MRP",
          class = "landing_button"
        )
      )
    )
  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$to_analyze, {
      updateNavbarPage(global$session,
                       inputId = "navbar",
                       selected = "nav_analyze")
    })


    observeEvent(input$to_learn_mrp, {
      updateNavbarPage(global$session,
                       inputId = "navbar",
                       selected = "nav_learn_mrp")
    })


  })
}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")
