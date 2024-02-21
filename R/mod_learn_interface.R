#' learn_interface UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_interface_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6, offset = 3,
        tags$br(),
        tags$h3("Workflow"),
        tags$p("The interface implements a complete workflow of statistical analyses, from data description, model fitting, and diagnostics, to result presentation."),
        tags$ul(
          tags$li("First, the interface reads and displays the input data."),
          tags$li("Second, it presents descriptive statistics of the patient records and geographic areas, where examples of raw data are also displayed."),
          tags$li("Third, users can specify and fit different models with various choices of covariates and fixed/varying effects. The model fitting is via Bayesian computation with Markov chain Monte Carlo algorithms in Stan. Model summaries are shown."),
          tags$li("Fourth, the interface shows a comparison between different models and presents their diagnostics results."),
          tags$li("Finally, interface graphs demonstrate the estimated infection prevalence over time for the targeted population and demographic and geographic subpopulations, for the selected model."),
        ),
        tags$h4("Uploading Data"),
        tags$p("In accordance with HIPAA rules, the interface operates on the assumption that the input data has been anonymized through aggregation. Specificallly, individual test records must be grouped into cells that correspond to unique combinations of all the geographic-demographic factors and cells that contain less than 5 records must be omitted. Raw values of the factors must be categorized as follows:"),
        tags$ul(
          tags$li("Sex: male, female"),
          tags$li("Race: Black, White, other"),
          tags$li("Age (in years): 0-17, 18-34, 35-64, 65-74, 75+"),
          tags$li("ZIP code: each ZIP code is treated as a category"),
          tags$li("Week index: test result dates are grouped into weeks with index 1 assigned to the earliest week in the data. An optional column containing the date of the first day of each week can be included for visualization purposes.")
        ),
        tags$p("Users can manually aggregate their data but we recommend using our preprocessing code. Here are the steps for using our code with RStudio:"),
        tags$ol(
          tags$li("Download the code."),
          tags$li("Start a new session in RStudio (Session > New Session)."),
          tags$li("Execute ", tags$code("source(\"PATH_TO_FILE/preprocess.R\")"), "."),
          tags$li("Execute ", tags$code("aggregate(\"PATH_TO_FILE/data_file_name\")"), "."),
          tags$li("The code will save the aggregated data as a CSV file in the same directory as the original data file.")
        ),
        tags$p("For manually aggregrated data, users must indentify relevant columns which will be renamed as the interface uses specific column names to extract data from the input table. Below is a simulated dataset that exemplifies the expected data input."),
        DT::dataTableOutput(outputId = ns("example")),
        tags$div(
          class = "interface_buttons",
          downloadButton(
            outputId = ns("download_code"),
            label = "Download preprocessing code",
            class = "interface_button"
          ),
          downloadButton(
            outputId = ns("download_example"),
            label = "Download example data",
            class = "interface_button"
          ),
          downloadButton(
            outputId = ns("download_week_table"),
            label = "Download week conversion table",
            class = "interface_button"
          )
        ),
        tags$h4("Fitting & Comparing Models"),
        tags$p("Users can select variables to include in a model and specify whether they are fixed or varying effects through simple drag-and-drop interactions. The variables are divided into four categories:"),
        tags$ul(
          tags$li(tags$b("Individual-level Predictors"), " are demographic information about the patients associated with the provided test records."),
          tags$li("The ", tags$b("Geographic-area-level Indicator"), " of choice is ZIP code which allows models to utilize quantities defined at the ZIP-code level."),
          tags$li(tags$b("Geographic-area-level Predictors"), " includes relevant quantities that are retrieved from the American Census Survey (ACS) and linked to individual records through ZIP codes."),
          tags$li(tags$b("Interactions"), " can be included in models to account for how the effect of one predictor is conditioned on that of another.")
        ),
        tags$p("Model parameters are estimated using samples generated in sequences. Multiple sequences or chains can be generated in parallel to expedite the model fitting process through multithreading. The interface automatically use one core for each chain so choose the number of chains carefully based on the available computing resouces. Learn more about sampling ",
               tags$a("here", href = "https://paul-buerkner.github.io/brms/articles/brms_threading.html", target = "_blank"))
      )
    )
  )
}

#' learn_interface Server Functions
#'
#' @noRd
mod_learn_interface_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  output$example <- DT::renderDataTable({
    read.csv(app_sys("extdata/example.csv")) |> head(100)
  })

  output$download_code <- downloadHandler(
    filename = function() { "preprocess.R" },
    content = function(file) {
      readLines(app_sys("extdata/preprocess.R")) |> writeLines(file)
    }
  )

  output$download_example <- downloadHandler(
    filename = function() { "example.csv" },
    content = function(file) {
      read.csv(app_sys("extdata/example.csv")) |> write.csv(file, row.names = FALSE)
    }
  )

  output$download_week_table <- downloadHandler(
    filename = function() { "week_conversion.csv" },
    content = function(file) {
      read.csv(app_sys("extdata/week_conversion.csv")) |> write.csv(file, row.names = FALSE)
    }
  )

  })
}

## To be copied in the UI
# mod_learn_interface_ui("learn_interface_1")

## To be copied in the server
# mod_learn_interface_server("learn_interface_1")
