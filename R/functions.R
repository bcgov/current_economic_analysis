#download from cansim and a bit of processing------------
get_cansim_unfiltered <- function(cansim_id, add_label, multiply_value_by = 1){
  cansim::get_cansim(cansim_id)%>%
    janitor::clean_names()%>%
    mutate(Series = add_label,
           Month = tsibble::yearmonth(ref_date),
           Value = value*multiply_value_by,
           Source = paste0("Cansim: ", cansim_id))%>%
    filter(Month > tsibble::yearmonth(today()-years(10)))
}
#convert JSON data to tibble------------
df_from_JSON <- function(url, add_label, index_date){
  df <- jsonlite::fromJSON(url)
  df <- as.data.frame(df)
  df <- as.data.frame(df$series.data)
  colnames(df) <- c("ref_date", "value")
  df <- df%>%
    mutate(Month = tsibble::yearmonth(paste0(substring(ref_date, 1, 4), "-", substring(ref_date, 5, 6))),
           Series = add_label,
           Value = as.numeric(value),
           Source = url)
  df_index_value <- df%>%
    filter(Month == index_date)%>%
    pull(Value)
  df <- df%>%
    mutate(Value = 100*Value/df_index_value)%>%
    filter(Month > tsibble::yearmonth(today()-years(10)))%>%
    select(Month, Series, Value, Source)
}
#extracts the provincial totals from births and deaths dataframes----------------
get_totals <- function(df, year, label){
  df <- df%>%
    filter(Community.Health.Service.Area == "Provincial Total")%>%
    select(-Community.Health.Service.Area, -Total)
  colnames(df) <- paste(year, colnames(df), sep = "-")
  df <- df%>%
    t()%>%
    as.data.frame()%>%
    rownames_to_column(var = "ref_date")%>%
    as_tibble()
  colnames(df) <-c("Month", "Value")
  df <- df%>%
    mutate(Month = tsibble::yearmonth(Month),
           Value = str_replace_all(Value,",",""),
           Value = as.numeric(Value),
           Series = label)
  return(df)
}
#calculate percentage changes------------
percent_change <- function(df){
  df%>%
    group_by(Series)%>%
    arrange(Month)%>%
    mutate(`Monthly Change` = Value/lag(Value)-1,
           `Annual Change` = Value/lag(Value, n = 12)-1)
}
