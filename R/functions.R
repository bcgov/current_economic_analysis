# download from cansim and a bit of processing------------
get_cansim_unfiltered <- function(cansim_id, add_label, multiply_value_by = 1, source_text) {
  cansim::get_cansim(cansim_id, factors = FALSE) %>% # change back if breaks
    janitor::clean_names() %>%
    mutate(
      geo=str_trim(geo),
      Series = add_label,
      Month = tsibble::yearmonth(ref_date),
      Value = value * multiply_value_by,
      Source = paste("Statistics Canada. Table", cansim_id, source_text, sep=" ")
    ) %>%
    filter(Month > tsibble::yearmonth(today() - years(10)))
}
# convert JSON data to tibble------------
df_from_JSON <- function(url, add_label, index_date) {
  tbbl <- jsonlite::fromJSON(url)
  tbbl <- as.data.frame(tbbl)
  tbbl <- as.data.frame(tbbl$series.data)
  colnames(tbbl) <- c("ref_date", "value")
  tbbl <- tbbl %>%
    mutate(
      Month = tsibble::yearmonth(paste0(substring(ref_date, 1, 4), "-", substring(ref_date, 5, 6))),
      Series = add_label,
      Value = as.numeric(value),
      Source = url
    )
  df_index_value <- tbbl %>%
    filter(Month == index_date) %>%
    pull(Value)
  tbbl <- tbbl %>%
    mutate(Value = 100 * Value / df_index_value) %>%
    filter(Month > tsibble::yearmonth(today() - years(10))) %>%
    select(Month, Series, Value, Source)
}
# extracts the provincial totals from births and deaths dataframes----------------
get_totals <- function(tbbl, year, label) {
  tbbl <- tbbl %>%
    filter(Community.Health.Service.Area == "Provincial Total") %>%
    select(-Community.Health.Service.Area, -Total)
  colnames(tbbl) <- paste(year, colnames(tbbl), sep = "-")
  tbbl <- tbbl %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column(var = "ref_date") %>%
    as_tibble()
  colnames(tbbl) <- c("Month", "Value")
  tbbl <- tbbl %>%
    mutate(
      Month = tsibble::yearmonth(Month),
      Value = str_replace_all(Value, ",", ""),
      Value = as.numeric(Value),
      Series = label
    )
  return(tbbl)
}
# calculate percentage changes------------
percent_change <- function(tbbl) {
  time_increment <- tbbl %>%
    group_by(Series) %>%
    summarise(grp_mean = mean(Month - lag(Month), na.rm = TRUE)) %>%
    ungroup() %>%
    summarize(mean(grp_mean)) %>%
    pull()
  num_lags <- case_when(
    time_increment == 1 ~ 12,
    time_increment == 3 ~ 4,
    time_increment == 12 ~ 1
  )
  tbbl <- tbbl %>%
    group_by(Series) %>%
    arrange(Month) %>%
    mutate(
      `Change` = Value / lag(Value) - 1,
      `Annual Change` = Value / lag(Value, n = num_lags) - 1
    )
  }
# plotly time series plot------------
plotly_ts <- function(tbbl, thing, format_as, tt_text) {
  plt <- ggplot(tbbl, aes(yearmonth(Month),
    {{ thing }},
    colour = Series,
    group = Series,
    text = paste(
      "\nIn",
      Month,
      tt_text,
      "\n",
      Series,
      "was",
      format_as({{ thing }}, accuracy = .01),
      "\n",
      ... = commentary
    )
  )) +
    geom_line() +
    labs(x = "Month") +
    scale_y_continuous(label = format_as) +
    scale_colour_manual(values = cbPalette) +
    theme_minimal()
  plotly::ggplotly(plt, tooltip = "text", dynamicTicks = "y")
}
# plotly heatmap plot for mpi------------
plotly_mpi <- function(tbbl, x_var, y_var, facet_var) {
  no_missing_df <- tbbl %>%
    mutate(`Estimated Cost (M)` = `Estimated Cost (M)` + 1) %>% # log10 scale... can't have zero.
    complete({{ x_var }}, {{ y_var }}, {{ facet_var }})

  plt <- ggplot(no_missing_df, aes({{ x_var }},
    {{ y_var }},
    fill = `Estimated Cost (M)`,
    text = str_to_title(paste0(
      "For ",
      {{ facet_var }},
      " - ",
      {{ x_var }},
      " projects in the",
      str_sub({{ y_var }}, start = 3),
      " region the Estimated Cost is ",
      scales::comma(`Estimated Cost (M)` - 1,
        prefix = "$",
        suffix = "(M)"
      ),
      "."
    ))
  )) +
    geom_tile() +
    scale_fill_viridis_c(trans = "log10", labels = scales::comma) +
    facet_wrap(vars({{ facet_var }})) +
    theme_minimal()
  plt <- aest::aest_fix_labs(plt)
  plotly::ggplotly(plt, tooltip = "text")
}
