# download from cansim and a bit of processing------------
get_cansim_unfiltered <- function(cansim_id, add_label, multiply_value_by = 1, source_text, date_parse = lubridate::ym) {
  temp <- cansim::get_cansim(cansim_id, factors = FALSE) %>% # change back if breaks
    janitor::clean_names() 
#  browser()
  temp <- temp%>%
    mutate(
      geo=str_trim(geo),
      Series = add_label,
      `Period Starting` = date_parse(ref_date),
      Value = value * multiply_value_by,
      Source = paste("Statistics Canada. Table", cansim_id, source_text, sep=" ")
    ) %>%
    filter(`Period Starting` > today() - years(10))
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
    filter(`Period Starting` > today() - years(10)) %>%
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
# calculate percentage changes------------
percent_change <- function(tbbl) {
  frequency_all_series <- tbbl%>%
    group_by(Series)%>%
    summarize(frequency=case_when(all(near(1, diff(`Period Starting`), tol=1))~"daily",
                                  all(near(7, diff(`Period Starting`), tol=2))~"weekly",
                                  all(near(30, diff(`Period Starting`), tol=3))~"monthly",
                                  all(near(90, diff(`Period Starting`), tol=4))~"quarterly",
                                  all(near(180, diff(`Period Starting`), tol=5))~"semi",
                                  all(near(365, diff(`Period Starting`), tol=6))~"annual",
    ))
  assertthat::assert_that(length(unique(frequency_all_series$frequency))==1)
  frequency <-frequency_all_series$frequency[[1]] 
  num_lags <- case_when(
    frequency== "daily" ~ 365,
    frequency== "weekly" ~ 52,
    frequency== "monthly" ~ 12,
    frequency== "quarterly" ~ 4,
    frequency== "semi" ~ 2,
    frequency== "annual" ~ 1,
  )
  tbbl <- tbbl %>%
    group_by(Series) %>%
    arrange(`Period Starting`) %>%
    mutate(
      `Change` = Value / lag(Value) - 1,
      `Annual Change` = Value / lag(Value, n = num_lags) - 1
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
                            "\nIn the period starting", `Period Starting`, tt_text, "\n", Series,
                            "was", format_as({{ thing }}, accuracy = .01)
                          )
  )) +
    scale_y_continuous(label = format_as) +
    get(theme)()
  if (type == "column") {
    plt <- plt +
      geom_col(position = "dodge", size = 0)
  } else if (type == "line") {
    plt <- plt +
      geom_line()
  } else {
    plt <- plt +
      geom_smooth(se=FALSE, span = spn, lwd=.5)+
      geom_line(alpha=.2, lwd=.2)
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
  plt <- plt+
    labs(title=str_split(title,":")[[1]][1],
         x="")
  if(el == TRUE){ 
    plt <- plt + 
      expand_limits(y=0)
  }
  plotly::ggplotly(plt, tooltip = "text")
}





