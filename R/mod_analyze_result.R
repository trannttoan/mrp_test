#' analyze_result UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
mod_analyze_result_ui <- function(id){
  ns <- NS(id)
  tags$div(class = "pad_top",
    navlistPanel(widths = c(3, 9),
      tabPanel(
        selectInput(
          inputId = ns("model_select"),
          label = "Select a model",
          choices = NULL
        )
      ),
      tabPanel("Raw vs MRP",
        plotOutput(outputId = ns("est_overall"))
      ),
      tabPanel("By subgroup",
        tabsetPanel(
          tabPanel("Sex",
            plotOutput(outputId = ns("est_sex"))
          ),
          tabPanel("Race",
            plotOutput(outputId = ns("est_race"))
          ),
          tabPanel("Age",
            plotOutput(outputId = ns("est_age"))
          ),
          tabPanel("County",
            tags$div(class = "pad_top",
              plotly::plotlyOutput(outputId = ns("est_county_map"),
                                   height = "700px")
            ),
            tags$div(class = "pad_top",
              selectizeInput(
                inputId = ns("counties_select"),
                label = "Select one or more counties (max = 5)",
                choices = NULL,
                multiple = TRUE,
                options = list(maxItems = 5)
              ),
              actionButton(
                inputId = ns("counties_btn"),
                label = "Plot"
              ),
              plotOutput(outputId = ns("est_county_line"))
            )
          )
        )
      )
    )
  )
}

#' analyze_result Server Functions
#'
#' @noRd
mod_analyze_result_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(global$input$navbar_analyze, {
      if(global$input$navbar_analyze == "nav_analyze_result") {
        if(is.null(global$data)) {
          showModal(
            modalDialog(
              title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
              "Invalid input data.",
              footer = actionButton(
                inputId = ns("to_upload"),
                label = "Go to data upload page"
              )
            ),
            session = global$session
          )
        } else {
          if(is.null(global$mrp_input)) {
            global$mrp_input <- list()

            c(global$mrp_input$patient,
              global$mrp_input$pstrat_data,
              global$mrp_input$covariates,
              global$mrp_input$levels,
              global$plotdata$raw_covariates,
              selected_states) %<-% link_ACS(
                global$data,
                global$extdata$tract_data,
                global$extdata$zip_tract,
                global$static$levels
              )

            c(global$mrp_input$brms_input,
              global$mrp_input$brms_new,
              global$mrp_input$vars) %<-% prepare_brms_data(
                global$mrp_input$patient,
                global$mrp_input$pstrat_data,
                global$mrp_input$covariates,
                global$mrp_input$levels
              )

            global$plotdata$geojson <- filter_geojson(
              global$extdata$map_geojson,
              global$mrp_input$covariates$fips
            )
          }

          if(length(global$models) > 0) {
            updateSelectInput(global$session,
              inputId = ns("model_select"),
              choices = names(global$models)
            )

            fips_df <- global$extdata$fips |> filter(fips %in% global$mrp_input$covariates$fips)
            choices <- sort(fips_df$county)
            updateSelectizeInput(global$session,
              inputId = ns("counties_select"),
              choices = choices,
              selected = choices[1]
            )
          } else {
            showModal(
              modalDialog(
                title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
                "No model detected.",
                footer = actionButton(
                  inputId = ns("to_model"),
                  label = "Go to model page"
                )
              ),
              session = global$session
            )
          }

        }
      }
    })

    observeEvent(input$to_upload, {
      updateTabsetPanel(global$session,
        inputId = "navbar_analyze",
        selected = "nav_analyze_upload"
      )

      removeModal(global$session)
    })

    observeEvent(input$to_model, {
      updateTabsetPanel(global$session,
        inputId = "navbar_analyze",
        selected = "nav_analyze_model"
      )

      removeModal(global$session)
    })

    output$est_overall <- renderPlot({
      req(names(global$models))

      plot_prev(
        global$mrp_input$brms_input,
        global$plotdata$dates,
        global$models[[input$model_select]]$overall
      )
    }, height = function() global$static$ui$plot_height)

    output$est_sex <- renderPlot({
      req(names(global$models))

      plot_estimate(
        global$models[[input$model_select]]$sex,
        global$plotdata$dates
      )

    }, height = function() global$static$ui$subplot_height * (length(global$mrp_input$levels$sex) + 1))

    output$est_race <- renderPlot({
      req(names(global$models))

      plot_estimate(
        global$models[[input$model_select]]$race,
        global$plotdata$dates
      )

    }, height = function() global$static$ui$subplot_height * (length(global$mrp_input$levels$race) + 1))

    output$est_age <- renderPlot({
      req(names(global$models))

      plot_estimate(
        global$models[[input$model_select]]$age,
        global$plotdata$dates
      )

    }, height = function() global$static$ui$subplot_height * (length(global$mrp_input$levels$age) + 1))

    output$est_county_map <- plotly::renderPlotly({
      req(names(global$models))

      waiter::waiter_show(
        id = ns("est_county_map"),
        html = waiter_ui("map")
      )

      p <- map_estimate(
        global$models[[input$model_select]]$county,
        global$extdata$fips,
        global$plotdata$geojson,
        global$plotdata$dates
      )

      waiter::waiter_hide()

      return(p)
    })


    n_counties <- reactiveVal(2)
    output$est_county_line <- renderPlot({
      req(names(global$models))
      input$counties_btn

      selected_counties <- isolate(input$counties_select)
      length(selected_counties) |> n_counties()

      global$models[[input$model_select]]$county |>
        mutate(fips = factor) |>
        left_join(
          global$extdata$fips,
          by = "fips"
        ) |>
        select(time, county, est, std) |>
        filter(county %in% selected_counties) |>
        rename("factor" = "county") |>
        plot_estimate(global$plotdata$dates)

    }, height = function() global$static$ui$subplot_height * (n_counties() + 1))

  })
}

## To be copied in the UI
# mod_analyze_result_ui("analyze_result_1")

## To be copied in the server
# mod_analyze_result_server("analyze_result_1")
