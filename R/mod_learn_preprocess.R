#' learn_preprocess UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_preprocess_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6, offset = 3,
        tags$br(),
        tags$img(src = "www/preprocess.png", width = "100%"),
        tags$h3("Data Description"),
        tags$h4("COVID-19 Test Records"),
        tags$p("The current version of the interface can handle COVID-19 test records from hospitals that adopts the Epic system. With this format restriction, the program can automatically identify the relevant columns in the data frame that are required for the modeling step without user intervention. Specifically, the input data frame must contain columns corresponding to the following geographic-demographic factors:"),
        tags$ul(
          tags$li("Sex"),
          tags$li("Race"),
          tags$li("Age"),
          tags$li("Zip code"),
          tags$li("Test result"),
          tags$li("Date of test result")
        ),
        tags$h4("American Community Survey Data"),
        tags$p("To account for the structured differences among zip code areas, we include quantities that are defined based on geographic areas. The ideal geophaphic scope would be zip but the American Community Survey (ACS) only provides data at the state, county and tract levels. To get a zip-level estimate, we retrieve following quantities at tract level then aggregage them across tracts that overlap with individual zip code area:"),
        tags$ul(
          tags$li("Binary indicators of whether tracts are classified as urban or not"),
          tags$li("Population sizes based on levels of education"),
          tags$li("Population sizes based on ratios of income to poverty level in the past 12 months"),
          tags$li("Population sizes based on employment status"),
          tags$li("Median household income in the past 12 months"),
          tags$li(tags$p("Area Deprivation Index (ADI). For definition, go to ", tags$a(href = "https://www.neighborhoodatlas.medicine.wisc.edu", target = "_blank")))
        ),
        h4("USPS Crosswalk Table"),
        tags$p(" Patient records contains zip codes whereas the ACS data are obtained at tract level. With the USPS crosswalk table which contains the links between zip codes and tracts, we can compute the zip-level estimates of the selected tract-level data by aggregating across all the overlapping tracts of each zip code. The details of the aggregation is provided in the data preparation section."),
        h3("Data Preparation"),
        tags$p("The entire data prepping process is illuestrated by the flowchart at the top of this page."),
        h4("Patient Data"),
        tags$p("One of the first step in preping the data for the modeling step is filtering out zip codes and states with small sample sizes. Specifically, we omit samples from states that account for less than one percent of the data and then zip codes with five or less records. The expectation is that the vast majority of test samples were obtained from people living in the proximity of the hospital and our test datasets were consistent with this expection. Next, we impute values that are missing or invalid for sex, race and age based on the frequency of occurence in the data. Finally, we aggregate the raw values for each of these demographic variables as follows:"),
        tags$ul(
          tags$li("Sex: male, female"),
          tags$li("Race: white, black, others"),
          tags$li("Age: 0-17, 8-34, 35-64, 65-74, 75+")
        ),
        tags$p("Future versions will allow for specification of how the values are grouped but these are the default categories for the current version. Another future feature is allowing user to aggregate result dates into either weeks or months. The current implementation only allows for the former."),
        h4("Postratification Table"),
        tags$p("Multilevel Regression and Post-stratificaiton (MRP) is comprised of two main stages: obtaining a model that can predict the outcome measure given a set of factors and weighting the predictions based on the population breakdown. The latter is refered to as post-stratification which requires the number of people in the population of interest that corresponds to each combination of factors. An example would be the number of white males who are between 8 and 34 years old, lives at an address with 48104 as zip code, and receive test result on the same week."),
        h4("Geographic-area-level Predictors"),
        tags$p("We compute the estimates for the zip-level predictors as follows:"),
        tags$ul(
          tags$li("Urbanicity of a zip code is defined as the percentage of covered census tracts classified as urban, weighted by tract population counts."),
          tags$li("Higher education measure of a zip code is defined as the percentage of the residing population who have earned an Associate's degree or higher."),
          tags$li("Poverty measure of a zip code is defined as the percentage of the residing population whose ratio of income to poverty level in the past 12 months is below 100%."),
          tags$li("Employment rate of a zip code is defined as the percentage of the residing population who are employed as a part of the civilian labor force."),
          tags$li("Income measure of a zip code is defined as the average value of tract-level median household income in the past 12 months, weighted by tract population counts."),
          tags$li("Area Deprivation Index (ADI) of a zip code is the average ADI across covered census tracts, weighted by tract population counts")
        ),
        tags$p("The result of the data prepping process consists of the cleaned and categorized patient data, the post-stratification table and the zip-level predictors which are used as inputs for the modeling stage.")
      )
    )
  )
}

#' learn_preprocess Server Functions
#'
#' @noRd
mod_learn_preprocess_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_learn_preprocess_ui("learn_preprocess_1")

## To be copied in the server
# mod_learn_preprocess_server("learn_preprocess_1")
