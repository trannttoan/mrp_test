#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  global <- reactiveValues(
    session = session,
    input = input,
    static = list(
      levels = list(
        sex = c("male", "female"),
        race = c("white", "black", "other"),
        age = c("0-17", "18-34", "35-64", "65-74", "75+")
      ),
      expected_columns = c("sex", "race", "age", "zip", "time", "date", "tests", "cases"),
      ui = list(
        max_model = 5,
        iter_range = c(100, 5000),
        chain_range = c(1, 8),
        plot_height = 500,
        subplot_height = 300
      )
    ),
    extdata = list(
      zip_tract = readr::read_csv(app_sys("extdata/zip_tract.csv"), show_col_types = FALSE),
      tract_data = readr::read_csv(app_sys("extdata/acs_data.csv"), show_col_types = FALSE),
      map_geojson = readRDS(app_sys("extdata/map_data.RDS")),
      fips = readr::read_csv(app_sys("extdata/fips.csv"), show_col_types = FALSE)
    ),
    mrp_input = NULL,
    plotdata = NULL,
    models = NULL,
    reset = FALSE
  )

  mod_home_server(module_ids$home, global)
  mod_analyze_upload_server(module_ids$analyze$upload, global)
  mod_analyze_visualize_server(module_ids$analyze$visualize, global)
  mod_analyze_model_server(module_ids$analyze$model, global)
  mod_analyze_result_server(module_ids$analyze$result, global)
  mod_learn_interface_server(module_ids$learn$interface, global)
}
