#' data
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @import dplyr
#' @import zeallot

find_columns <- function(df, expected_columns) {
  # find columns using string search
  all_names <- names(df)

  old_names <- expected_columns |> sapply(function(s) all_names[grepl(s, all_names, ignore.case = TRUE)]) |> unlist()

  df <- df |> select(all_of(old_names))
  names(df) <- names(old_names)
  missing <- setdiff(expected_columns, names(old_names))
  df[, missing] <- NA

  return(df)
}

check_input_data <- function(df, expected_columns, demo_levels) {
  errors <- list()

  df <- find_columns(df, expected_columns)
  missing <- df |> lapply(function(c) all(is.na(c))) |> unlist()
  missing_names <- names(missing)[missing]

  if(length(missing_names) > 1 | (length(missing_names) == 1 & !"date" %in% missing_names)) {
    errors$missing_column <- paste0("The following columns are missing: ",
                                    paste(missing_names, collapse = ", "),
                                    " (\"date\" column is optional and is only used for plotting)")
  } else {
    # check data types
    valid_types <- c(
      is.character(df$sex),
      is.character(df$race),
      is.character(df$age),
      is.character(df$zip) | is.numeric(df$zip),
      is.numeric(df$time),
      is.character(df$date),
      is.numeric(df$tests),
      is.numeric(df$cases)
    )

    if(any(!valid_types)) {
      errors$data_type <- paste0("Columns corresponding to the following variables have inappropriate data types: ",
                                 paste(expected_columns[!valid_types], collapse = ", "))
    } else {
      df <- df |> na.omit()

      # check if week indices start at 1
      if(min(df$time) != 1) {
        errors$week <- "The lowest week index must be 1"
      }

      # check if dates are in the right format
      if("date" %in% missing_names) {
        errors$date <- "Dates are not provided (\"date\" column is optional and is only used for plotting)"
      } else {
        if (anyNA(as.Date(df$date, optional = TRUE))) {
          errors$date <- "Provided dates are not in expected format (\"date\" column is optional and is only used for plotting)"
        }
      }

      # check if cases are less than or equal to cases
      if(any(df$cases > df$tests)) {
        errors$cases <- "The number of cases cannot be greater than the number of tests"
      }
    }
  }

  return(errors)
}

create_timeline <- function(df) {
  df$date |>
    na.omit() |>
    unique() |>
    as.Date() |>
    sort() |>
    format("%d%b\n%Y") |>
    as.character()
}


filter_geojson <- function(map_data, zip_codes) {
  counties_all <- map_data$features
  counties_in_data <- purrr::keep(counties_all, function(c) c$properties$GEOID %in% zip_codes)
  map_data$features <- counties_in_data

  return(map_data)
}


prep <- function(df, demo_levels, all_zips) {

  # regularize data types and values
  df <- df |> mutate(
    sex = tolower(sex),
    race = tolower(race),
    zip = as.character(zip),
    time = as.integer(time),
    tests = as.integer(tests),
    cases = as.integer(cases)
  )

  # keep only expected values
  df <- df |> filter(
    sex %in% demo_levels$sex,
    race %in% demo_levels$race,
    age %in% demo_levels$age,
    zip %in% all_zips
  )

}


get_zip_tract <- function(key) {
  url <- "https://www.huduser.gov/hudapi/public/usps"
  response <- httr::GET(url, query = list(type = 1, query = "All"), httr::add_headers(Authorization = paste("Bearer", key)))

  httr::http_error(response)
  output <- httr::content(response)

  zip_tract <- dplyr::bind_rows(output$data$results)

  return(zip_tract)
}

get_bounds <- function(strings) {
  bounds <- strings |> sapply(function(s) str_split_1(s, "!!") |> tail(1) |> str_split_1(" ") |> head(1) |> as.numeric())

  return(c(0, unname(bounds)))
}

collapse <- function(
    new_bounds,
    old_bounds,
    df_in,
    offset
) {

  df_out <- data.frame(init_column = 1:nrow(df_in))
  indices <- match(new_bounds, old_bounds)
  N <- length(indices)

  if(any(is.na(indices))) {
    print("Invalid bounds!")
  }
  else {

    for(i in 1:(N-1)) {
      i_beg <- indices[i]
      i_end <- indices[i + 1]
      colname <- paste0(old_bounds[i_beg],'-', old_bounds[i_end] - offset)
      df_out[colname] <-  df_in |> select(all_of(i_beg:(i_end-1))) |> rowSums()
    }

    colname <- paste0(new_bounds[N], '+')
    df_out[colname] <- df_in |> select(all_of(indices[N]:ncol(df_in))) |> rowSums()

  }
  df_out <- df_out |> select(-1)

  if(!identical(rowSums(df_in), rowSums(df_out))) {
    print("Inconsistent row sums.")
  }

  return(df_out)
}

# Retrieve ACS data using tidycensus package
get_tract_data <- function(
    state_codes,
    age_bounds,
    poverty_bounds,
    year
) {

  gen_vars <- function(str, seq) seq |> sapply(\(i) sprintf("%s_%03d", str, i))

  # look-up table
  # https://www.census.gov/programs-surveys/acs/data/data-tables/table-ids-explained.html
  lookup_df <- load_variables(2021, dataset = "acs5", cache = TRUE)

  # the indices are chosen based on the look-up table
  # for extracting table labels from look-up table
  age_indices_one <- 4:16
  age_indices_all <- 283:304
  poverty_indices <- 26397:26402

  # for level-based aggregation of columns of get_acs output
  education_levels <- c("below_college", "above_college")
  education_indices <- list(1:19, 20:24)
  employment_levels <- c("employed", "unemployed", "other")
  employment_indices <- list(3, 4, 5:6)

  # generate variable names
  group_names <- c("male_white", "female_white",
                   "male_black", "female_black",
                   "male_all", "female_all",
                   "education", "poverty",
                   "employment", "income",
                   "pop_size")

  group_prefixes <- c("B01001A", "B01001A",
                      "B01001B", "B01001B",
                      "B01001", "B01001",
                      "B15003", "C17002",
                      "B23025", "B19013",
                      "B01001")

  group_table_numbers <- list(3:16, 18:31,
                              3:16, 18:31,
                              3:25, 27:49,
                              2:25, 2:8,
                              2:7, 1,
                              1)

  group_vars <- c()
  for(i in 1:length(group_names)) {
    group_vars <- group_vars |> c(gen_vars(group_prefixes[i], group_table_numbers[[i]]))
  }

  # retrieve ACS tables
  all_tables <- get_acs(
    geography = "tract",
    variables = group_vars,
    state = state_codes,
    output = "wide",
    year = year
  )

  geoID <- all_tables[1]
  all_tables <- all_tables[seq(3, ncol(all_tables), by = 2)]

  group_dfs <- list()
  ind <- 0
  for(i in 1:length(group_names)) {
    n_tables <- length(group_table_numbers[[i]])
    group_dfs[group_names[i]] <- all_tables[(ind+1):(ind+n_tables)] |> list()
    ind <- ind + n_tables
  }


  df_all <- data.frame(GEOID = geoID)

  # POPULATION SIZE
  names(group_dfs$pop_size) <- "pop_size"
  df_all <- cbind(df_all, group_dfs$pop_size)

  ### SEX, RACE, AGE
  # aggregate columns
  age_bounds_acs_all <- get_bounds(lookup_df$label[age_indices_all])
  age_bounds_acs_one <- get_bounds(lookup_df$label[age_indices_one])

  for(name in c("male_all", "female_all")) {
    group_dfs[name] <- collapse(age_bounds,
                                age_bounds_acs_all,
                                group_dfs[[name]],
                                1) |> list()
  }
  for(name in c("male_white", "female_white",
                "male_black", "female_black")) {
    group_dfs[name] <- collapse(age_bounds,
                                age_bounds_acs_one,
                                group_dfs[[name]],
                                1) |> list()
  }

  # subtract white and black from total
  group_dfs$male_other <- group_dfs$male_all - (group_dfs$male_white + group_dfs$male_black)
  group_dfs$female_other <- group_dfs$female_all - (group_dfs$female_white + group_dfs$female_black)

  # rename columns and combine data frames
  for(name in c("male_white", "male_black", "male_other",
                "female_white", "female_black", "female_other")) {
    names(group_dfs[[name]]) <- paste0(name, '_', names(group_dfs[[name]]))
    df_all <- cbind(df_all, group_dfs[[name]])
  }

  # URBANICITY
  urbanicity <- read_sas("z_us_tract_uac.sas7bdat") |>
    rename(
      "urbanicity" = "uac_yn",
      "GEOID" = "geocode"
    )
  df_all <- df_all |> inner_join(urbanicity, by = "GEOID")


  # EDUCATION
  for(i in 1:length(education_indices)) {
    name <- education_levels[i]
    inds <- education_indices[[i]]
    df_all[[name]] <- group_dfs$education[inds] |> rowSums()
  }

  # POVERTY
  poverty_bounds_acs <- get_bounds(lookup_df$label[poverty_indices])
  group_dfs$poverty <- collapse(poverty_bounds,
                                poverty_bounds_acs,
                                group_dfs$poverty,
                                0.01) |> list()

  df_all <- cbind(df_all, group_dfs$poverty)

  # EMPLOYMENT
  for(i in 1:length(employment_indices)) {
    name <- employment_levels[i]
    inds <- employment_indices[[i]]
    df_all[[name]] <- group_dfs$employment[inds] |> rowSums()
  }

  # INCOME
  names(group_dfs$income) <- "household_income"
  df_all <- cbind(df_all, group_dfs$income)

  # ADI
  adi_data <- read_sas("z_adi_bg_v3_2019.sas7bdat") |>
    na.omit() |>
    group_by(state_cty_tract_cd) |>
    summarize(
      adi = mean(us_adi_rank_num)
    ) |>
    rename("GEOID" = "state_cty_tract_cd")
  df_all <- df_all |> inner_join(adi_data, by = "GEOID")

  df_all <- na.omit(df_all)

  return(df_all)
}

filter_state_zip <- function(
    df_zip,
    zip_tract,
    zip_threshold = 5,
    state_threshold = 0.01
) {

  n_records <- sum(df_zip$zip_count, na.rm = TRUE)

  # create a table containing state, zip and zip count
  state_zip <- zip_tract |>
    mutate(state_code = substr(geoid, 1, 2)) |>
    select(zip, state, state_code) |>
    distinct(zip, .keep_all = TRUE) |>
    right_join(df_zip, by = "zip")

  # filter based on proportion of state
  state_zip <- state_zip |>
    group_by(state) |>
    filter(sum(zip_count) > state_threshold * n_records) |>
    ungroup()

  # filter based on number of zip
  state_zip <- state_zip |>
    group_by(zip) |>
    filter(sum(zip_count) > zip_threshold) |>
    ungroup()

  state_zip_select <- state_zip |>
    select(state_code, zip) |>
    sapply(unique)

  return(state_zip_select)
}

# For each zip, find tracts and aggregate data over them
combine_tracts <- function(
    patient,
    tract_data,
    zip_tract
) {

  by_zip <- zip_tract |>
    filter(zip %in% unique(patient$zip)) |>
    select(geoid, zip) |>
    rename("GEOID" = "geoid") |>
    inner_join(
      tract_data,
      by = "GEOID"
    ) |>
    mutate(fips = substr(GEOID, 1, 5)) |>
    group_by(zip)

  all_colnames <- names(tract_data)
  pstrat_colnames <- all_colnames[grepl("male|female", all_colnames)]
  pstrat_data <- by_zip |>
    summarise(
      fips = first(fips),
      across(all_of(pstrat_colnames), sum)
    )

  covariates <- by_zip |>
    summarize(
      fips = first(fips),
      urbanicity  = 1 - sum((pop_size / sum(pop_size)) * (urbanicity == "N")),
      college     = sum(above_college) / (sum(below_college) + sum(above_college)),
      employment  = sum(employed) / (sum(employed) + sum(unemployed) + sum(other)),
      poverty     = sum(`0-0.99`) / (sum(`0-0.99`) + sum(`1-1.99`) + sum(`2+`)),
      income      = sum((pop_size / sum(pop_size)) * household_income),
      ADI         = sum((pop_size / sum(pop_size)) * adi)
    )

  # remove zip codes without tract in USPS crosswalk table
  patient <- patient |> filter(zip %in% pstrat_data$zip)

  return(list(patient, pstrat_data, covariates))
}

link_ACS <- function(
    patient,
    tract_data,
    zip_tract,
    demo_levels
) {

  # filter out state and zip codes with small sample sizes
  state_zip_select <- filter_state_zip(
    patient |> group_by(zip) |> summarize(zip_count = sum(tests)),
    zip_tract
  )
  patient <- patient |> filter(zip %in% state_zip_select$zip)

  # compute zip-level data
  c(patient, pstrat_data, covariates) %<-% combine_tracts(
    patient,
    tract_data,
    zip_tract
  )

  # save unstandardized covariates for plotting
  raw_covariates <- covariates

  # standardize zip-level covariates
  covariates <- covariates |> mutate(
    across(!zip & !fips, function(c) c |> scale() |> as.vector() |> unlist(use.names=FALSE))
  )

  # create lists of all factor levels
  levels <- lapply(c("sex", "race", "age"), function(n) patient[[n]] |> unique() |> factor(levels = demo_levels[[n]]) |> sort())
  names(levels) <- c("sex", "race", "age")
  levels$county <- pstrat_data$fips |> unique()

  return(list(patient, pstrat_data, covariates, levels, raw_covariates, state_zip_select$state_code))
}

prepare_brms_data <- function(
    patient,
    pstrat_data,
    covariates,
    levels,
    spec = 0.999,
    sens = 0.7,
    threshold = 5
) {

  pstrat_counts <- pstrat_data[-c(1, 2)] |> t() |> c()
  pstrat_proportions <- pstrat_counts / sum(pstrat_counts)
  time_indices <- 1:max(patient$time)
  n_time_indices <- length(time_indices)

  input_data <- patient |>
    left_join(covariates, by = "zip") |>
    mutate(
      spec = spec,
      sens = sens
    )

  new_data <- tidyr::expand_grid(
    sex = levels$sex,
    race = levels$race,
    age = levels$age,
    time = time_indices,
    zip = covariates$zip
  ) |>
    arrange(time, zip, sex, race, age) |>
    mutate(
      tests = rep(pstrat_counts, n_time_indices),
      proportion = rep(pstrat_proportions, n_time_indices),
      spec = spec,
      sens = sens
    ) |>
    left_join(covariates, by = "zip")

  vars <- list()
  vars$individual <- names(patient) |> setdiff(c("zip", "date", "tests", "cases"))
  vars$geographic <- names(covariates) |> setdiff(c("zip", "fips"))
  vars$geo_indicator <- "zip"

  return(list(input_data, new_data, vars))
}
