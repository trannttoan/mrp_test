#' plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @import dplyr
#' @import ggplot2
#' @import plotly
plot_individual <- function(
    brms_input,
    pstrat_data,
    levels,
    separate = TRUE
) {

  n_tests <- sum(brms_input$tests)
  brms_input <- brms_input |>
    group_by(demo) |>
    summarize(perc = sum(tests) / n_tests)


  counts <- levels |> sapply(function(s) {
    cond <- grepl(s, names(pstrat_data))
    if(s == "male") {
      cond <- cond & !grepl("female", names(pstrat_data))
    }

    pstrat_data[, cond] |> rowSums() |> sum()
  })

  acs <- data.frame(
    demo = levels,
    perc = counts / sum(counts)
  )

  plot_df <- rbind(brms_input, acs)
  datasets <- c("Input Data", "ACS Data")
  plot_df <- plot_df |> mutate(
    dataset = rep(datasets, each = length(levels)),
    demo = factor(demo, levels = levels)
  )

  if(separate) {
    p1 <- ggplot(
      data = plot_df |> filter(dataset == datasets[1]),
      aes(x = demo,
          y = perc)
    ) +
      geom_bar(
        stat = "identity",
        position = "dodge"
      ) +
      labs(
        title = datasets[1],
        caption = sprintf("Sample size: %d", n_tests)
      )

    p2 <- ggplot(
      data = plot_df |> filter(dataset == datasets[2]),
      aes(x = demo,
          y = perc)
    ) +
      geom_bar(
        stat = "identity",
        position = "dodge"
      ) +
      labs(
        title = datasets[2],
        caption = sprintf("Sample size: %d", sum(counts))
      )

    p <- p1 + p2

  } else {
    p <- ggplot(
      data = plot_df,
      aes(
        x = demo,
        y = perc,
        fill = dataset
      )
    ) +
      geom_bar(
        stat = "identity",
        position = "dodge"
      )
  }

  p <- p & scale_x_discrete(
      labels = tools::toTitleCase(as.character(levels))
    ) & scale_y_continuous(
      labels = scales::percent,
      limits = c(0, 1),
      expand = c(0, 0)
    ) &
    labs(x = "", y = "Percent") &
    theme_bw() &
    theme(
      text = element_text(size = 18),
      plot.title = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )


  return(p)
}


map_sample_size <- function(
    brms_input,
    fips_county_state,
    map_geojson
) {

  total <- sum(brms_input$tests)
  plot_df <- brms_input |>
    group_by(fips) |>
    summarize(
      count = sum(tests),
      perc = round((sum(tests) / total), 4) * 100
    ) |>
    left_join(fips_county_state,by = "fips") |>
    mutate(hover = paste0(
        state_name, '\n',
        county, '\n',
        "Sample size: ", count,
        " (", perc, "%)"
    ))



  g <- list(
    fitbounds = "locations",
    visible = FALSE
  )

  fontstyle <- list(
    size = 16,
    color = "black"
  )

  label <- list(
    bgcolor = "white",
    bordercolor = "black",
    font = fontstyle
  )

  fig <- plot_ly() |>
    add_trace(
      type = "choropleth",
      geojson = map_geojson,
      locations = plot_df$fips,
      z = plot_df$count,
      zmin = 0,
      zmax = max(plot_df$count),
      featureidkey = "properties.GEOID",
      colorscale = "Electric",
      text = plot_df$hover,
      hoverinfo = "text"
    ) |>
    layout(
      geo = g,
      title = "Map of County Sample Size"
    ) |>
    style(
      hoverlabel = label
    ) |>
    colorbar(
      title = "Sample\nSize"
    ) |>
    plotly::config(
      displayModeBar = FALSE
    )

  return(fig)
}

plot_geographic <- function(
    covariates,
    breaks,
    description,
    definition,
    name
) {

  p <- ggplot(
    data = covariates,
    aes(x = covar)
  ) +
    geom_histogram(breaks = breaks) +
    scale_x_continuous(
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, .1))
    ) +
    labs(
      title = "",
      subtitle = description,
      caption = definition,
      x = name, y = "Number of zip codes"
    ) +
    theme_bw() +
    theme(
      text = element_text(size = 18),
      plot.subtitle = element_text(size = 15, hjust = 0),
      plot.caption = element_text(size = 15, hjust = 0.5),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )

  return(p)
}

plot_prev <- function(
  raw,
  dates,
  estimate = NULL,
  raw_color = "darkblue",
  mrp_color = "darkorange"
) {

  plot_df <- raw |>
    group_by(time) |>
    summarize(prev = sum(cases) / sum(tests)) |>
    right_join(data.frame(time = 1:max(raw$time)),
               by = "time")

  if(!is.null(estimate)) {
    plot_df <- plot_df |>
      left_join(estimate, by = "time") |>
      mutate(
        bound_upper = est + std,
        bound_lower = est - std
      )
    plot_df$bound_lower[plot_df$bound_lower < 0] <- 0
  }

  p <- ggplot(
    data = plot_df,
    aes(x = time)
  ) +
    geom_line(
      aes(
        y = prev,
        color = "Raw"
      ),
      linewidth = 1.5
    )

  if(!is.null(estimate)) {
    p <- p +
      geom_line(
        aes(
          y = est,
          color = "MRP"
        ),
        linewidth = 1.5
      ) +
      geom_ribbon(
        aes(
          y = est,
          ymin = bound_lower,
          ymax = bound_upper
        ),
        fill = mrp_color,
        alpha = 0.5
      )
  }

  xticks <- seq(1, max(raw$time), 9)
  xticklabels <- if(!is.null(dates)) dates[xticks] else xticks

  p <- p +
    labs(
      title = "",
      x = if(is.null(dates)) "Week index" else "",
      y = "Prevalence"
    ) +
    scale_x_continuous(
      breaks = xticks,
      labels = xticklabels,
      expand = c(0, 0.1)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(5e-3, 0.1))
    ) +
    scale_color_manual(
      values = c("Raw" = raw_color, "MRP" = mrp_color)
    ) +
    theme_bw() +
    theme(
      text = element_text(size = 18),
      legend.title = element_blank(),
      legend.position = if(is.null(estimate)) "none" else "bottom",
      plot.margin = margin(1, 1, 1, 1, "cm")
    )

  return(p)

}

map_prev <- function(
    brms_input,
    fips_county_state,
    map_geojson
) {

  # calculate weekly prevalence and test counts for each county
  plot_df <- brms_input |>
    group_by(fips, time) |>
    summarize(
      prev = sum(cases) / sum(tests),
      tests = sum(tests)
    ) |>
    ungroup()

  # compute max and min weekly prevalence for each county
  plot_df <- plot_df |>
    group_by(fips) |>
    summarize(
      max_prev = max(prev),
      min_prev = min(prev),
      max_prev_sample = tests[which.max(prev)],
      min_prev_sample = tests[which.min(prev)]
    ) |>
    full_join(fips_county_state, by = "fips") |>
    mutate(hover = paste0(
      state_name, '\n',
      county, '\n',
      "Highest: ", round(max_prev, 4),
      " (", round(max_prev_sample * max_prev), '/', max_prev_sample, ")\n",
      "Lowest: ", round(min_prev, 4),
      " (", round(min_prev_sample * min_prev), '/', min_prev_sample, ")\n"
    ))

  g <- list(
    fitbounds = "locations",
    visible = FALSE
  )

  fontstyle <- list(
    size = 16,
    color = "black"
  )

  label <- list(
    bgcolor = "white",
    bordercolor = "black",
    font = fontstyle
  )

  fig <- plot_ly() |>
    add_trace(
      type = "choropleth",
      geojson = map_geojson,
      locations = plot_df$fips,
      z = plot_df$max_prev,
      zmin = 0,
      zmax = max(plot_df$max_prev),
      featureidkey = "properties.GEOID",
      colorscale = "Electric",
      text = plot_df$hover,
      hoverinfo = "text"
    ) |>
    layout(
      geo = g,
      title = "Raw Prevalence Across Weeks"
    ) |>
    style(
      hoverlabel = label
    ) |>
    colorbar(
      title = "Highest\nWeekly\nPrevalence"
    ) |>
    plotly::config(
      displayModeBar = FALSE
    )

  return(fig)
}

plot_pp_check <- function(
    yrep,
    raw,
    dates,
    yrep_color = "darkorange",
    raw_color = "darkblue"
) {

  plot_df <- raw |>
    group_by(time) |>
    summarise(
      prev = sum(cases) / sum(tests)
    ) |>
    right_join(
      data.frame(time = 1:max(raw$time)),
      by = "time"
    ) |>
    left_join(yrep, by = "time")

  plot_df$lower[plot_df$lower < 0] <- 0


  xticks <- seq(1, max(raw$time), 9)
  xticklabels <- if(!is.null(dates)) dates[xticks] else xticks

  p <- ggplot(
    data = plot_df,
    aes(x = time)
  ) +
    geom_line(
      aes(
        y = prev,
        color = "Raw"
      ),
      linewidth = 1.5
    ) +
    geom_line(
      aes(
        y = median,
        color = "Replicated"
      ),
      linewidth = 1.5
    ) +
    geom_ribbon(
      aes(
        y = median,
        ymin = lower,
        ymax = upper
      ),
      fill = yrep_color,
      alpha = 0.5
    ) +
    labs(
      title = "",
      x = if(is.null(dates)) "Week index" else "",
      y = "Prevalence"
    ) +
    scale_x_continuous(
      breaks = xticks,
      labels = xticklabels,
      expand = c(0, 0.1)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(5e-3, 0.1))
    ) +
    scale_color_manual(
      values = c("Raw" = raw_color, "Replicated" = yrep_color)
    ) +
    theme_bw() +
    theme(
      text = element_text(size = 18),
      legend.title = element_blank(),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )

  return(p)
}

plot_estimate <- function(df, dates) {
  levels <- unique(df$factor) |> sort()
  labels <- levels |> as.character() |> tools::toTitleCase()

  colors <- RColorBrewer::brewer.pal(8, "Set1")[1:length(levels)]
  xticks <- seq(1, max(df$time), 9)
  xticklabels <- if(!is.null(dates)) dates[xticks] else xticks

  df <- df |> mutate(
    bound_lower = est - std,
    bound_upper = est + std
  )
  df$bound_lower[df$bound_lower < 0] <- 0
  limits <- c(0, max(df$bound_upper, na.rm = TRUE))

  plot_list <- list()
  i = 1

  plot_list[[i]] <- ggplot(
    data = df,
    aes(
      x = time,
      y = est,
      group = factor
    )
  ) +
    geom_line(
      aes(colour = factor),
      alpha = 0.8,
      linewidth = 1.5
    ) +
    scale_color_manual(
      values = colors,
      labels = labels
    )

  for(level in levels) {
    i <- i + 1
    plot_list[[i]] <- ggplot(
      data = df |> filter(factor == level),
      aes(
        x = time,
        y = est,
        group = factor
      )
    ) +
      geom_line(
        aes(color = factor),
        alpha = 0.8,
        linewidth = 1.5
      ) +
      geom_ribbon(
        aes(
          fill = factor,
          ymin = bound_lower,
          ymax = bound_upper
        ),
        alpha = 0.5
      ) +
      scale_color_manual(values = colors[i - 1], labels = labels[i -1]) +
      scale_fill_manual(values = colors[i - 1], labels = labels[i - 1])
  }

  for(i in 1:length(plot_list)) {
    plot_list[[i]] <- plot_list[[i]] +
      labs(title = "",
           x = if(is.null(dates)) "Week index" else "",
           y = "Prevalence") +
      scale_x_continuous(
        breaks = xticks,
        labels = xticklabels,
        expand = c(0, 0.1)
      ) +
      scale_y_continuous(
        limits = limits,
        expand = expansion(mult = c(5e-3, 0.1))
      ) +
      theme_bw() +
      theme(
        text = element_text(size = 18),
        legend.title = element_blank(),
        legend.position = "right",
        plot.margin = margin(0, 1, 0, 1, "cm")
      )
  }

  p <- patchwork::wrap_plots(plot_list,
                             ncol = 1,
                             nrow = length(levels) + 1)

  return(p)
}

map_estimate <- function(df, fips_county_state, map_geojson, dates) {

  plot_df <- df |>
    mutate(fips = factor) |>
    left_join(
      fips_county_state,
      by = "fips"
    ) |>
    mutate(hover = paste0(
      state_name, '\n',
      county, '\n',
      "Estimate: ", est, '\n',
      "Standard error: ", std
    ))


  sq <- 1:max(df$time)
  plot_df <- plot_df |>
    mutate(
      time = factor(
        time,
        levels = sq,
        labels = if(!is.null(dates)) dates else paste0("Week ", sq)
      )
    )

  g <- list(
    fitbounds = "locations",
    visible = FALSE
  )

  fontstyle <- list(
    size = 15,
    color = "black"
  )

  label <- list(
    bgcolor = "white",
    bordercolor = "black",
    font = fontstyle
  )

  fig <- plot_ly(
    frame = plot_df$time
  ) |>
    add_trace(
      type = "choropleth",
      geojson = map_geojson,
      locations = plot_df$fips,
      z = plot_df$est,
      zmin = 0,
      zmax = max(plot_df$est, na.rm = TRUE),
      featureidkey = "properties.GEOID",
      colorscale = "Electric",
      text = plot_df$hover,
      hoverinfo = "text"
    ) |>
    layout(
      geo = g,
      title = "MRP-estimated Prevalence"
    ) |>
    style(
      hoverlabel = label
    ) |>
    colorbar(
      title = "Prevalence"
    ) |>
    plotly::config(
      displayModeBar = FALSE
    )

  return(fig)
}

