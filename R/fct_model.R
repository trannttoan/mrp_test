#' model
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @import dplyr
run_brms <- function(
    fstr,
    input_data,
    hyper_param = list(
      intercept_prior_mean = 0,
      intercept_prior_scale = 5,
      coef_prior_scale = 2.5
    ),
    n_iter = 1000,
    n_chains = 4
) {

  formula <- as.formula(fstr)

  binomial_sens_spec_vec <- brms::custom_family(
    "binomial_sens_spec", dpars = c("mu"),
    links = c("logit"), lb = c(NA),
    type="int", vars= c("vint1", "vreal1", "vreal2"), loop = FALSE
  )

  stan_density_vec <- "
    real binomial_sens_spec_lpmf(array[] int y, vector mu, array[] int N, array[] real vreal1, array[] real vreal2) {
      return binomial_lpmf(y | N, mu * vreal1[1] + (1 - mu) * (1 - vreal2[1]));
    }
    array[] int binomial_sens_spec_rng(vector mu, array[] int N) {
      return binomial_rng(N, mu);
    }
  "

  stanvars_vec <- brms::stanvar(scode = stan_density_vec, block = "functions")

  default_prior <- brms::get_prior(formula, input_data, binomial_sens_spec_vec)
  classes <- unique(default_prior$class)
  custom_prior <- brms::set_prior(paste0("normal(", hyper_param$intercept_prior_mean, ",", hyper_param$intercept_prior_scale, ")"), class = "Intercept")
  if("b" %in% classes) {
    custom_prior <- custom_prior + brms::set_prior(paste0("normal(0,", hyper_param$coef_prior_scale, ")"), class = "b")
  }
  if("sd" %in% classes) {
    custom_prior <- custom_prior + brms::set_prior(paste0("normal(0,", hyper_param$coef_prior_scale, ")"), class = "sd")
  }


  fit <- brms::brm(formula = formula,
                   data = input_data,
                   family = binomial_sens_spec_vec,
                   prior = custom_prior,
                   stanvars = stanvars_vec,
                   chains = n_chains,
                   cores = n_chains,
                   iter = n_iter,
                   backend = "rstan")

  brms::expose_functions(fit, vectorize = TRUE)

  return(fit)
}

log_lik_binomial_sens_spec <- function(i, prep) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  trials <- prep$data$vint1[i]
  sens <- prep$data$vreal1[i]
  spec <- prep$data$vreal2[i]
  y <- prep$data$Y[i]

  binomial_sens_spec_lpmf(y, mu, trials, sens, spec)
}

posterior_predict_binomial_sens_spec <- function(i, prep, ...) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  trials <- prep$data$vint1[i]

  binomial_sens_spec_rng(mu, trials)
}

process_brms_output <- function(
  brms_newdata,
  pred_mat
) {

  df_out <- (brms_newdata$proportion * pred_mat) |>
    as.data.frame() |>
    mutate(
      factor = brms_newdata$factor,
      time = brms_newdata$time
    ) |>
    group_by(time, factor) |>
    summarize_all(sum) |>
    ungroup()

  df_out <- df_out |>
    mutate(
      est = df_out |> select(-c(time, factor)) |> apply(1, mean) |> round(4),
      std = df_out |> select(-c(time, factor)) |> apply(1, sd) |> round(4)
    ) |>
    select(time, factor, est, std)

  return(df_out)
}

custom_pp_check <- function(
  yrep_mat,
  brms_input,
  pred_interval = 0.95
) {

  qlower <- (1 - pred_interval) / 2
  qupper <- 1 - qlower

  agg_df <- yrep_mat |>
    as.data.frame() |>
    mutate(
      time = brms_input$time,
      tests = brms_input$tests
    ) |>
    group_by(time) |>
    summarise_all(sum) |>
    ungroup()

  agg_tests <- agg_df$tests
  time <- agg_df$time

  prevs <- agg_df |>
    select(-c(time, tests)) |>
    mutate_all(function(c) c / agg_tests)

  df_out <- data.frame(
    time = time,
    upper = prevs |> apply(1, quantile, qlower),
    lower = prevs |> apply(1, quantile, qupper),
    median = prevs |> apply(1, quantile, 0.5)
  )


  return(df_out)
}
