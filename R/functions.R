# download from cansim and a bit of processing------------
get_cansim_unfiltered <- function(cansim_id, add_label, multiply_value_by = 1, source_text, date_parse = lubridate::ym) {
  temp <- cansim::get_cansim(cansim_id, factors = FALSE) %>% # change back if breaks
    janitor::clean_names()
  #  browser()
  temp <- temp %>%
    mutate(
      geo = str_trim(geo),
      Series = add_label,
      `Period Starting` = date_parse(ref_date),
      Value = value * multiply_value_by,
      Source = paste("Statistics Canada. Table", cansim_id, source_text, sep = " ")
    ) %>%
    filter(`Period Starting` > today() - years(11))
}
# convert JSON data to tibble------------
df_from_JSON <- function(url, add_label, index_date) {
  tbbl <- jsonlite::fromJSON(url)
  tbbl <- as.data.frame(tbbl)
  tbbl <- as.data.frame(tbbl$series.data)
  colnames(tbbl) <- c("ref_date", "value")
  #  browser()
  tbbl <- tbbl %>%
    mutate(
      `Period Starting` = lubridate::ym(paste0(substring(ref_date, 1, 4), "-", substring(ref_date, 5, 6))),
      Series = add_label,
      Value = as.numeric(value),
      Source = url
    )
  df_index_value <- tbbl %>%
    filter(`Period Starting` == index_date) %>%
    pull(Value)
  tbbl <- tbbl %>%
    mutate(Value = 100 * Value / df_index_value) %>%
    filter(`Period Starting` > today() - years(11)) %>%
    select(`Period Starting`, Series, Value, Source)
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
  colnames(tbbl) <- c("Period Starting", "Value")
  # browser()
  tbbl <- tbbl %>%
    mutate(
      `Period Starting` = lubridate::ym(`Period Starting`),
      Value = str_replace_all(Value, ",", ""),
      Value = as.numeric(Value),
      Series = label
    )
  return(tbbl)
}
# calculate some stats------------
make_stats <- function(tbbl) {
  frequency_all_series <- tbbl %>%
    group_by(Series) %>%
    summarize(frequency = case_when(
      all(near(1, diff(`Period Starting`), tol = 1)) ~ "daily",
      all(near(7, diff(`Period Starting`), tol = 2)) ~ "weekly",
      all(near(30.4375, diff(`Period Starting`), tol = 3)) ~ "monthly",
      all(near(91.3125, diff(`Period Starting`), tol = 3)) ~ "quarterly",
      all(near(182.625, diff(`Period Starting`), tol = 2)) ~ "semi",
      all(near(365.25, diff(`Period Starting`), tol = 1)) ~ "annual",
    ))
  assertthat::assert_that(length(unique(frequency_all_series$frequency)) == 1) # make sure time series are (pretty much) regular.
  frequency <- frequency_all_series$frequency[[1]]
  num_lags <- case_when(
    frequency == "daily" ~ 365,
    frequency == "weekly" ~ 52,
    frequency == "monthly" ~ 12,
    frequency == "quarterly" ~ 4,
    frequency == "semi" ~ 2,
    frequency == "annual" ~ 1,
  )
  tbbl <- tbbl %>%
    group_by(Series) %>%
    arrange(`Period Starting`) %>%
    mutate(
      `Change` = (Value - lag(Value)) / abs(lag(Value)),
      `Annual Change` = (Value - lag(Value, n = num_lags)) / abs(lag(Value, n = num_lags)),
      `Binned Level` = ntile(Value, 100),
      `Rescaled Level` = range01(Value, na.rm = TRUE),
      `Binned Change` = ntile(Change, 100),
      `Rescaled Change` = range01(Change, na.rm = TRUE),
      `Binned Annual Change` = ntile(`Annual Change`, 100),
      `Rescaled Annual Change` = range01(`Annual Change`, na.rm = TRUE)
    )
}
# Extractor function (retrieve cell)----------
extract_cell <- function(tbbl, nm) {
  tbbl <- tbbl %>%
    filter(name == nm) %>%
    select(value) %>%
    pull()
  tbbl <- tbbl[[1]]
  return(tbbl)
}
# plotly time series plot------------
plotly_ts <- function(tbbl, thing, format_as, tt_text, theme, type, pal, title, spn, el = FALSE) {
  plt <- ggplot(tbbl, aes(`Period Starting`,
    {{ thing }},
    colour = Series,
    fill = Series,
    group = Series,
    text = paste(
      "\nIn",
      format.Date(`Period Starting`, "%B %Y"),
      tt_text,
      "\n",
      Series,
      "was",
      if (format_as == "percent") {
        scales::percent({{ thing }}, accuracy = .01)
      } else {
        if (str_detect(title, "Merchandise Trade")) {
          scales::dollar({{ thing }}, accuracy = 1)
        } else if (str_detect(tbbl$Series[[1]], "dollars")) {
          scales::dollar({{ thing }}, accuracy = 1)
        } else if (str_detect(tbbl$Series[[1]], "yield")) {
          scales::percent({{ thing }}, accuracy = .01)
        } else if (str_detect(tbbl$Series[[1]], "rate")) {
          scales::percent({{ thing }}, accuracy = .01)
        } else if (str_detect(title, "Exchange Rate")) {
          scales::dollar({{ thing }}, accuracy = .01)
        } else {
          scales::comma({{ thing }}, accuracy = 1)
        }
      }
    )
  )) +
    scale_x_date(limits = c(today() - years(10), today())) +
    get(theme)()
  if (type == "column") {
    plt <- plt +
      geom_col(position = "dodge", size = 0)
  } else if (type == "line") {
    plt <- plt +
      geom_line()
  } else {
    plt <- plt +
      geom_smooth(se = FALSE, span = spn, lwd = .5) +
      geom_line(alpha = .2, lwd = .2)
  }
  if (pal == "Viridis") {
    plt <- plt +
      scale_colour_viridis_d() +
      scale_fill_viridis_d()
  } else {
    plt <- plt +
      scale_colour_brewer(palette = pal) +
      scale_fill_brewer(palette = pal)
  }
  plt <- plt +
    labs(
      title = str_split(title, ":")[[1]][1],
      x = ""
    )
  if (el == TRUE) {
    plt <- plt +
      expand_limits(y = 0)
  }
  if (format_as == "percent") {
    plt <- plt +
      scale_y_continuous(labels = scales::percent)
  } else {
    if (str_detect(title, "Merchandise Trade")) {
      plt <- plt + scale_y_continuous(labels = scales::dollar)
    } else if (str_detect(tbbl$Series[[1]], "dollars")) {
      plt <- plt + scale_y_continuous(labels = scales::dollar)
    } else if (str_detect(tbbl$Series[[1]], "yield")) {
      plt <- plt + scale_y_continuous(labels = scales::percent)
    } else if (str_detect(tbbl$Series[[1]], "rate")) {
      plt <- plt + scale_y_continuous(labels = scales::percent)
    } else if (str_detect(title, "Exchange Rate")) {
      plt <- plt + scale_y_continuous(labels = scales::dollar)
    } else {
      plt <- plt + scale_y_continuous(labels = scales::comma)
    }
  }
  plotly::ggplotly(plt, tooltip = "text")
}

range01 <- function(x, ...) {
  (x - min(x, ...)) / (max(x, ...) - min(x, ...))
}

my_formatter <- function(name, value) {
  value <- case_when(
    str_detect(name, "Merchandise Trade") ~ scales::dollar(value, accuracy = 1),
    str_detect(name, "dollars") ~ scales::dollar(value, accuracy = 1),
    str_detect(name, "yield") ~ scales::percent(value, accuracy = .01),
    str_detect(name, "rate") ~ scales::percent(value, accuracy = .01),
    str_detect(name, "Exchange Rate") ~ scales::dollar(value, accuracy = .01),
    TRUE ~ scales::comma(value, accuracy = 1)
  )
}
