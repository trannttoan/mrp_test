#' analyze_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import sortable
#' @import zeallot
mod_analyze_model_ui <- function(id){
  ns <- NS(id)
  tags$div(class = "pad_top",
    sidebarLayout(
      sidebarPanel(width = 3,
        uiOutput(outputId = ns("model_spec"))
      ),
      mainPanel(width = 9,
        tabsetPanel(id = ns("navbar_model"),
          tabPanel("Model Comparison",
            value = "nav_compare",
            uiOutput(outputId = ns("model_compare"))
          )
        )
      )
    )
  )
}

#' analyze_model Server Functions
#'
#' @noRd
mod_analyze_model_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(global$input$navbar_analyze, {
      if(global$input$navbar_analyze == "nav_analyze_model") {
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
        }
      }
    })

    observeEvent(input$to_upload, {
      updateTabsetPanel(global$session,
                        inputId = "navbar_analyze",
                        selected = "nav_analyze_upload")

      removeModal(global$session)
    })


    # Model Specification Panel
    output$model_spec <- renderUI({
      tagList(
        tags$div(
          class = "justify",
          tags$div(id = "model_spec_title",
            tags$h4("Model Specification", class = "inline"),
            icon("info-sign", lib = "glyphicon", id = "model_spec_icon")
          ),
          tags$div(
            class = "small_buttons",
            actionButton(
              inputId = ns("reset_btn"),
              label = icon("repeat", lib = "glyphicon"),
            ),
            actionButton(
              inputId = ns("add_btn"),
              label = icon("chevron-right", "fa")
            )
          ),
          shinyBS::bsTooltip("model_spec_icon", "Specify models by dragging elements from boxes on the left to ones on the right. Drag individual elements out of the \"Effect\" boxes to remove them or use the \"Reset\" button to start over. Press the \"Arrow\" button to fit and save a model."),
          shinyBS::bsTooltip(ns("reset_btn"), "Reset fields"),
          shinyBS::bsTooltip(ns("add_btn"), "Fit model")
        ),
        tags$div(
          id = ns("remove_panel"),
          fluidRow(
            column(
              width = 6,
              selectInput(
                inputId = ns("predictor_select"),
                label = NULL,
                choices = c("Individual-level Predictor",
                            "Geographic-area-level Indicator",
                            "Geographic-area-level Predictor",
                            "Interaction")
              ),
              conditionalPanel(ns = ns,
                condition = paste0("input.predictor_select == 'Individual-level Predictor'"),
                tags$div(
                  class = "panel panel-success predictor-panel",
                  tags$div(
                    class = "panel-body",
                    id = ns("pred_indiv"),
                    isolate(global$mrp_input$vars$individual) |> lapply(create_drag_item)
                  )
                )
              ),
              conditionalPanel(ns = ns,
                condition = paste0("input.predictor_select == 'Geographic-area-level Indicator'"),
                tags$div(
                  class = "panel panel-success",
                  tags$div(
                    class = "panel-body",
                    id = ns("pred_geo_indi"),
                    isolate(global$mrp_input$vars$geo_indicator) |> create_drag_item()
                  )
                )
              ),
              conditionalPanel(ns = ns,
                condition = paste0("input.predictor_select == 'Geographic-area-level Predictor'"),
                tags$div(
                  class = "panel panel-success",
                  tags$div(
                    class = "panel-body",
                    id = ns("pred_geo"),
                    isolate(global$mrp_input$vars$geographic) |> lapply(create_drag_item)
                  )
                )
              ),
              conditionalPanel(ns = ns,
                condition = paste0("input.predictor_select == 'Interaction'"),
                tags$div(
                  class = "panel panel-success",
                  tags$div(
                    class = "panel-body",
                    id = ns("pred_interact"),
                    isolate(global$mrp_input$vars$individual) |>
                      create_interactions() |>
                      lapply(create_drag_item)
                  )
                )
              )
            ),
            column(
              width = 6,
              tags$div(  # fixed effect box
                class = "panel panel-primary",
                tags$div(
                  class = "panel-heading",
                  "Fixed Effect"
                ),
                tags$div(
                  class = "panel-body effect-box",
                  id = ns("effect_fixed")
                )
              ),
              tags$div(  # varying effect box
                class = "panel panel-primary",
                tags$div(
                  class = "panel-heading",
                  "Varying Effect"
                ),
                tags$div(
                  class = "panel-body effect-box",
                  id = ns("effect_varying")
                )
              )
            )
          ),
          sortable_js(
            ns("pred_indiv"),
            options = sortable_options(
              group = list(
                name = "indiv",
                pull = "clone",
                put = FALSE
              ),
              sort = FALSE
            )
          ),
          sortable_js(
            ns("pred_geo_indi"),
            options = sortable_options(
              group = list(
                name = "geo_indi",
                pull = "clone",
                put = FALSE
              ),
              sort = FALSE
            )
          ),
          sortable_js(
            ns("pred_geo"),
            options = sortable_options(
              group = list(
                name = "geo_pred",
                pull = "clone",
                put = FALSE
              ),
              sort = FALSE
            )
          ),
          sortable_js(
            ns("pred_interact"),
            options = sortable_options(
              group = list(
                name = "interact",
                pull = "clone",
                put = FALSE
              ),
              sort = FALSE
            )
          ),
          sortable_js(
            ns("effect_fixed"),
            options = sortable_options(
              group = list(
                name = "fixed",
                pull = TRUE,
                put = c("indiv", "geo_indi", "geo_pred", "interact")
              ),
              onSort = sortable_js_capture_input(ns("fixed_effects"))
            )
          ),
          sortable_js(
            ns("effect_varying"),
            options = sortable_options(
              group = list(
                name = "varying",
                pull = TRUE,
                put = c("indiv", "geo_indi", "interact")
              ),
              onSort = sortable_js_capture_input(ns("varying_effects"))
            )
          ),
          sortable_js(
            ns("remove_panel"),
            options = sortable_options(
              group = list(
                name = "remove",
                pull = FALSE,
                put = c("fixed", "varying")
              ),
              onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }"),
              animation = 0
            )
          )
        ),
        selectInput(
          inputId = ns("iter_select"),
          label = "Select the number of iterations",
          choices = c("100 (Test)", "500 (Low)", "2000 (Medium)", "5000 (High)", "Custom"),
          selected = "2000 (Medium)"
        ),
        conditionalPanel(ns = ns,
          condition = paste0("input.iter_select == 'Custom'"),
          numericInput(
            inputId = ns("iter_kb"),
            label = "Enter the number of iterations",
            min = 100, max = 5000, step = 100,
            value = 1000
          )
        ),
        numericInput(
          inputId = ns("chain_select"),
          label = "Select the number of chains",
          min = 1, max = 8, step = 1,
          value = 4
        ),
        tags$p(
          "For details about the model fitting process, go to ",
          actionLink(
            inputId = ns("to_interface"),
            label = "Interface",
            class = "action_link"
          )
        )
      )
    })

    # Model Comparison Tab
    output$model_compare <- renderUI({
      tags$div(class = "pad_top",
        selectizeInput(
          inputId = ns("model_select"),
          label = "Select one or more models",
          choices = names(global$models),
          multiple = TRUE
        ),
        actionButton(
          inputId = ns("diagnos_btn"),
          label = "Run diagnostics"
        ),
        tags$h4("Leave-one-out Cross-validation", class = "break_title"),
        tags$hr(class = "break_line"),
        create_text_box(
          title = tags$b("Note"),
          tags$p("Generally, ", tags$code("elpd_diff"), "beng less than 4 indicates small difference in the predictive power between models. For values of ", tags$code("elpd_diff"), "greater than 4, ", tags$code("se_diff"), ", the standard error of ", tags$code("elpd_diff"), "can account for the uncertainty in the difference. Find more details about how to inteprete these terms ", tags$a("here", href = "https://mc-stan.org/loo/articles/online-only/faq.html#elpd_interpretation", target = "_blank"), ".")
        ),
        uiOutput(outputId = ns("loo_warning")),
        tableOutput(outputId = ns("loo_table")),
        tags$h4("Posterior Predictive Check", class = "break_title"),
        tags$hr(class = "break_line"),
        create_text_box(
          title = tags$b("Note"),
          tags$p("The plot shows the weekly prevalence computed based on the observed and replicated data generated from the posterior predictive distributtion with the color band representing the 95% predictive intervals.")),
        uiOutput(outputId = ns("ppc_plots"))
      )
    })


    output$loo_warning <- renderUI({
      input$diagnos_btn

      if(length(isolate(input$model_select)) == 1) {
        tags$p("*Two or more models are required")
      }
    })

    output$loo_table <- renderTable({
      input$diagnos_btn
      global$data

      selected_names <- isolate(input$model_select)

      if(length(selected_names) > 1) {
        waiter::waiter_show(
          html = waiter_ui("loo"),
          color = waiter::transparent(0.9)
        )

        res <- global$models[selected_names] |>
          purrr::map(function(m) m$fit) |>
          unname() %>%
          do.call(brms::loo, .)

        rownames(res$diffs) <- selected_names
        res$diffs <- res$diffs |> as.data.frame() |> select(elpd_diff, se_diff)

        waiter::waiter_hide()

      } else {
        res <- list()
      }


      return(res$diffs)

    })

    # generate PPC plots
    output$ppc_plots <- renderUI({
      input$diagnos_btn
      global$data

      selected_names <- isolate(input$model_select)

      if(length(selected_names) > 0) {
        structs <- purrr::map(global$models[selected_names], function(m) m$mean_structure)

        purrr::map(1:length(structs), ~ list(
          HTML(paste0("<h4><u>Model ", .x, "</u>", ": ", structs[[.x]], "</h4>")),
          plotOutput(ns(paste0("ppc", .x)))
        ))
      }

    })

    # render PPC plots
    observeEvent(input$diagnos_btn, {
      selected_names <- isolate(input$model_select)

      if(length(selected_names) > 0) {
        yrep_mats <- purrr::map(global$models[selected_names], function(m) m$yrep_mat)

        purrr::map(1:length(yrep_mats), function(i) {
          output[[paste0("ppc", i)]] <- renderPlot({
            yrep_mats[[i]] |>
              custom_pp_check(global$mrp_input$brms_input) |>
              plot_pp_check(global$mrp_input$brms_input, global$plotdata$dates)
          })
        })
      }

    })


    # reset input fields
    observeEvent(input$reset_btn, {
      shinyjs::runjs("$('.effect-box').empty();")
      shinyjs::reset("predictor_select")
      shinyjs::reset("iter_select")
      shinyjs::reset("iter_kb")
      shinyjs::reset("chain_select")
      sortable::sortable_js_capture_input(ns("fixed_effects"))
      sortable::sortable_js_capture_input(ns("varying_effects"))
      print(input$fixed_effects)
    })

    # add model
    observeEvent(input$add_btn, {
      n_iter <- if(input$iter_select == "Custom") input$iter_kb else as.integer(strsplit(input$iter_select, " ")[[1]][1])
      n_chains <- input$chain_select

      if(is.null(global$models)) {
        global$models <- list()
      }

      # check if number of iterations and number of chains are within defined range
      c(within_range, msg_range) %<-% check_iter_chain(
        n_iter, global$static$ui$iter_range,
        n_chains, global$static$ui$chain_range
      )

      if(within_range) {
        if(length(global$models) <= global$static$ui$max_model) {

          # check if the formula is valid
          c(formula, mean_structure, valid) %<-% create_formula(
            input$fixed_effects |> unique() |> sort(),
            input$varying_effects |> unique() |> sort()
          )

          if(valid) {
            # check if model has been created
            if(!(paste0(mean_structure, n_iter) %in% purrr::map(global$models, function(m) m$id))) {
              waiter::waiter_show(
                html = waiter_ui("fit"),
                color = waiter::transparent(0.9)
              )

              # create a list to store model info
              model_name <- paste0("Model ", length(global$models) + 1)
              model <- list()
              model$id <- paste0(mean_structure, n_iter)
              model$mean_structure <- mean_structure

              # fit model
              model$fit <- run_brms(
                formula,
                global$mrp_input$brms_input,
                n_iter = n_iter,
                n_chains = n_chains
              )

              # compute estimates
              model$pred_mat <- brms::posterior_linpred(
                model$fit,
                newdata = global$mrp_input$brms_new,
                ndraws = 100,
                transform = TRUE,
                allow_new_levels = TRUE
              ) |> t()

              # generate replicated data
              model$yrep_mat <- brms::posterior_predict(
                model$fit,
                ndraws = 100
              ) |> t()

              # process brms output for plotting
              model$overall <- global$mrp_input$brms_new |>
                mutate(factor = 1) |>
                process_brms_output(model$pred_mat)

              model$sex <- global$mrp_input$brms_new |>
                mutate(factor = sex) |>
                process_brms_output(model$pred_mat)

              model$race <- global$mrp_input$brms_new |>
                mutate(factor = race) |>
                process_brms_output(model$pred_mat)

              model$age <- global$mrp_input$brms_new |>
                mutate(factor = age) |>
                process_brms_output(model$pred_mat)

              model$county <- global$mrp_input$brms_new |>
                mutate(factor = fips) |>
                process_brms_output(model$pred_mat)

              model_summary <- summary(model$fit)

              # UI element IDs
              model$IDs <- list(
                fixed = paste0(model$id, "_fixed"),
                varying = paste0(model$id, "_varying"),
                ppc = paste0(model$id, "_ppc"),
                tab = paste0(model$id, "_tab"),
                title = paste0(model$id, "_title"),
                button = paste0(model$id, "_button")
              )

              # create new tab
              tab_header <- tagList(
                textOutput(
                  outputId = ns(model$IDs$title),
                  inline = TRUE
                ),
                actionButton(
                  inputId = ns(model$IDs$button),
                  label = NULL,
                  icon = icon("remove", lib = "glyphicon"),
                  class = "btn-xs remove_model"
                )
              )
              print(names(global$models))
              appendTab("navbar_model",
                select = TRUE,
                tabPanel(title = tab_header,
                  value = model$IDs$tab,
                  tags$div(class = "pad_top",
                    HTML(paste0("<h4>", "Formula: ", formula, "</h4>")),
                    tags$h5(paste0("A binomial model with a logit function of the prevalence. ",
                                   "Samples are generated using ", model_summary$chains, " chains with ", model_summary$iter - model_summary$warmup, " post-warmup iterations each.")),
                    create_text_box(
                      title = tags$b("Note"),
                      tags$ul(
                        tags$li("Values for ", tags$code("Convergence"), " that are greater than 1.1 indicates the chains have not yet converged and it is necessary to run more iterations and/or set stronger priors."),
                        tags$li("Low values for ", tags$code("Bulk-ESS"), " and ", tags$code("Tail-ESS"), " (ESS stands for Effective Sample Size) also suggest that more iterations are required.")
                      )
                    ),
                    tags$h4("Fixed Effects", class = "break_title"),
                    tags$hr(class = "break_line"),
                    tableOutput(ns(model$IDs$fixed)),
                    tags$h4("Varying Effects", class = "break_title"),
                    tags$hr(class = "break_line"),
                    purrr::map(names(model_summary$random),  ~ list(
                      tags$em(tags$h4(.x)),
                      gsub(':', '_', .x) %>%
                        paste0(model$IDs$varying, '_', .) |>
                        ns() |>
                        tableOutput()
                    )),
                    tags$h4("Posterior Predictive Check", class = "break_title"),
                    tags$hr(class = "break_line"),
                    create_text_box(
                      title = tags$b("Note"),
                      tags$p("Modeling fitting is evaluated by comparing data generated from the posterior model distribtutions to the raw data. The plot shows the weekly prevalence rates computed from the observed and replicated data with the color band representing the 95% predictive intervals.")
                    ),
                    plotOutput(outputId = ns(model$IDs$ppc))
                  )
                )
              )

              # changeable tab title
              output[[model$IDs$title]] <- renderText(model_name)

              # render fixed effect table
              output[[model$IDs$fixed]] <- renderTable({
                model_summary$fixed |>
                  rename("Convergence" = "Rhat") |>
                  mutate(
                    Bulk_ESS = as.integer(Bulk_ESS),
                    Tail_ESS = as.integer(Tail_ESS)
                  )
              }, rownames = TRUE)


              # render varying effect tables
              purrr::map(names(model_summary$random), function(s) {
                id <- gsub(':', '_', s) %>% paste0(model$IDs$varying, '_', .)
                output[[id]] <- renderTable(
                  model_summary$random[[s]] |>
                    rename("Convergence" = "Rhat") |>
                    mutate(
                      Bulk_ESS = as.integer(Bulk_ESS),
                      Tail_ESS = as.integer(Tail_ESS)
                    ),
                  rownames = TRUE
                )
              })

              # # render ppc plot
              output[[model$IDs$ppc]] <- renderPlot(
                model$yrep_mat |>
                  custom_pp_check(global$mrp_input$brms_input) |>
                  plot_pp_check(global$mrp_input$brms_input, global$plotdata$dates)
              )

              observeEvent(input[[model$IDs$button]], {
                # remove model object and tab
                global$models[[model_name]] <- NULL
                removeTab("navbar_model", model$IDs$tab, session)
                print(1234)
                print(names(global$models))
                # re-index model objects and tabs
                names(global$models) <- if(length(global$models) > 0) paste0("Model ", 1:length(global$models)) else character()
                purrr::map(names(global$models), function(name) {
                  output[[global$models[[name]]$IDs$title]] <- renderText(name)
                })
              })


              global$models[[model_name]] <- model
              print(123)
              print(names(global$models))
              waiter::waiter_hide()

            } else {
              showModal(
                modalDialog(
                  title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
                  "This model has already been added."
                ),
                session = global$session
              )
            }
          } else {
            showModal(
              modalDialog(
                title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
                formula
              ),
              session = global$session
            )
          }
        } else {
          showModal(
            modalDialog(
              title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
              "Maximum number of models reached. Please removed existing models to add more."
            ),
            session = global$session
          )
        }
      } else {
        showModal(
          modalDialog(
            title = tagList(icon("triangle-exclamation", "fa"), "Warning"),
            msg_range
          ),
          session = global$session
        )
      }
    })

    # reset everything when new data is uploaded
    observeEvent(global$data, {
      # reset input fields
      shinyjs::runjs("$('.effect-box').empty();")
      shinyjs::reset("predictor_select")
      shinyjs::reset("iter_select")
      shinyjs::reset("iter_kb")
      shinyjs::reset("chain_select")

      # delete all model tabs
      purrr::map(purrr::map(global$models, function(m) m$IDs$tab), function(id) {
        removeTab("navbar_model", id, session)
      })

      updateTabsetPanel(session,
                        inputId = "navbar_model",
                        selected = "nav_compare")

      # clear model object list
      global$models <- NULL

    })
  })
}

## To be copied in the UI
# mod_analyze_model_ui("analyze_model_1")

## To be copied in the server
# mod_analyze_model_server("analyze_model_1")
