#' learn_mrp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learn_mrp_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6, offset = 3,
        tags$div(
          class = "learn_mrp",
          tags$br(),
          withMathJax("We use a Bayesian framework to account for unknown sensitivity and specificity and apply MRP to testing records for population representation, here using the following adjustment variables: the biological variable of sex, age, race, and zip codes. MRP has two key steps: (1) fit a multilevel model for the prevalence with the adjustment variables based on the testing data; and (2) poststratify using the population distribution of the adjustment variables, yielding prevalence estimates in the target population."),
          tags$br(), tags$br(),
          withMathJax("We denote the PCR test result for individual \\(i\\) as \\(y_i\\), where \\(y_i=1\\) indicating a positive result and \\(y_i=0\\) indicating negative. With poststratification cells, we can directly model cell-wise summaries. We can obtain aggregated counts as the number of tests \\(n_k\\) and the number of positive cases \\(y^*_k\\) in group \\(k\\), defined as a cell \\(k\\) in the cross-tabulation of sex, age, race, zip code and indicators of time in weeks based on the test result dates. We assume that individuals in the same group have the same probability of being infected."),
          tags$br(), tags$br(),
          withMathJax("Let \\(p_k=\\textrm{Pr}(y_{k[i]}=1)\\) be the probability that person \\(i\\) in group \\(k\\) tests positive. The analytic incidence \\(p_k\\) is a function of the test sensitivity \\(\\delta\\), specificity \\(\\gamma\\), and the true incidence \\(\\pi_k\\) for individuals in group \\(k\\):"),
          withMathJax("$$p_k=(1-\\gamma)(1-\\pi_k )+\\delta \\pi_k.$$"),
          withMathJax("We will start by fitting a Binomial model \\(y^*_k\\), \\(y^*_k \\sim \\textrm{Binomial}(n_k, p_k)\\) with a logit function for \\(\\pi_k\\) with covariates including sex, age, race, zip codes, and time in weeks to allow time variation of prevalence over time in the multilevel model parameters."),
          withMathJax("$$\\textrm{logit}(\\pi_k)=\\beta_1+\\beta_2male_k+\\alpha_{age[k]}^{age}+\\alpha_{race[k]}^{race}+\\alpha_{zip[k]}^{zip}+\\alpha_{time[k]}^{time},$$"),
          withMathJax("where \\(male_k\\) is an indicator for men; age[k], race[k], and zip[k] represent age, race, and zip code levels; and time[k] indices the time in weeks when the test result is observed for group \\(k\\). We include zip code level covariates in the model as geographic measures \\(\\vec{Z}^{zip}_{j}\\) listed above for zip code \\(j\\),"),
          withMathJax("$$\\alpha_{j}^{zip} =\\vec{\\alpha}\\vec{Z}^{zip}_{j} +  e_j.$$"),
          withMathJax("Here \\(e_j\\) denotes the zip code level error term and follows a normal distribution or a spatial distribution to capture the geospatial dependency (e.g., the conditional autoregressive model)."),
          tags$br(), tags$br(),
          withMathJax("In the Bayesian framework we assign hierarchical priors to varying intercepts \\(\\alpha^{name}\\) or error terms \\(e_j\\):"),
          withMathJax("$$\\alpha^{name} \\sim normal(0,\\sigma^{name} )\\mbox{,  } \\sigma^{name}\\sim normal_+ (a,b),$$"),
          withMathJax("for \\(name \\in \\{age, race\\}\\). Here, \\(normal_+ (a,b)\\) represents a half-normal distribution with the mean \\(a\\) and standard deviation \\(b\\) restricted to positive values, with pre-specified values of \\((a,b)\\)."),
          tags$br(), tags$br(),
          withMathJax("According to the test protocol, the sensitivity is unknown, and the specificity is around 100\\%. We solicit prior information from previous testing results and try different values of the hyper-parameters for sensitivity analysis."),
          tags$br(), tags$br(),
          withMathJax("Using the estimated incidence \\(\\hat{\\pi}_k\\) based on the Bayesian model, we adjust for selection bias by applying the socio-demographic distributions in the community to generate the population-level prevalence estimates, as the poststratification step in MRP. For each of the cells in the cross-tabulation table of sex, age, race, and zip code (40 levels), we have the cell-wise incidence estimate \\(\\hat{\\pi}_c\\) and population count \\(N_c\\), where \\(c\\) is the cell index, and calculate the weekly prevalence estimate in the population,"),
          withMathJax("$$\\hat{pi}_{avg} = \\sum_c N_c\\hat{\\pi}_c/\\sum_c N_c,$$"),
          withMathJax("which can be restricted to subdomains of interest, as another property of MRP to yield robust estimates for small areas, e.g., on the county level.")
        )
      )
    )
  )
}

#' learn_mrp Server Functions
#'
#' @noRd
mod_learn_mrp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_learn_mrp_ui("learn_mrp_1")

## To be copied in the server
# mod_learn_mrp_server("learn_mrp_1")
