# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#This file: loads data from various websources to be used in the current economic analysis dashboard (and other outputs?).
#Output is a nested dataframe saved as an RDS
#libraries-----------
library(tidyverse)
library(lubridate)
#functions----------
source(here::here("R", "functions.R"))
#create a directory for raw data if does not exist----------
if (!file.exists(here::here("raw_data"))) dir.create(here::here("raw_data"))
if (!file.exists(here::here("processed_data"))) dir.create(here::here("processed_data"))
#make getSymbols quiet----------
options("getSymbols.warning4.0" = FALSE)
options("getSymbols.yahoo.warning" = FALSE)
#constants---------
index_date <- tsibble::yearmonth(ym("2014-01"))# date for indexing forestry, oil, natural gas
df_list <- list() #storage list for all dataframes
#commodity prices-------
commodity_url <- "https://www.bankofcanada.ca/valet/observations/group/BCPI_MONTHLY/csv?start_date=1972-01-01"
commodity_names <- read_csv(commodity_url,
                            skip = 10, #THIS COULD BREAK
                            n_max = 7 )%>% #THIS COULD BREAK
  select(id, label)%>%
  mutate(Series = word(label, sep = "\\-")) #THIS COULD BREAK

commodity <- read_csv(commodity_url,
                    skip = 20)%>% #THIS COULD BREAK
  pivot_longer(cols = -date, names_to = "name", values_to = "Value")%>%
  left_join(commodity_names, by = c("name" = "id"))%>%
  select(-name)%>%
  mutate(Month = tsibble::yearmonth(date),
         Source = commodity_url)%>%
  filter(Month > tsibble::yearmonth(today()-years(10)))%>%
  select(Month, Series, Value, Source)
df_list$`Bank of Canada Commodity Price Indicies` <- commodity
#Major Project Inventory (just 5 most recent quarters)---------
mpi_url_to_scrape <- "https://www2.gov.bc.ca/gov/content/employment-business/economic-development/industry/bc-major-projects-inventory/recent-reports"
mpi_scraped <- rvest::read_html(mpi_url_to_scrape)
mpi_links <- rvest::html_attr(rvest::html_nodes(mpi_scraped, "a"), "href") #all the links
mpi_links <- mpi_links[mpi_links%>%startsWith("/assets/") & mpi_links%>%endsWith(".xlsx")]%>% #stubs of the links we want.
  na.omit()%>%
  head(n = 5)
mpi_links <- paste0("https://www2.gov.bc.ca", mpi_links) #paste the head onto the stubs
mpi_files <- str_sub(mpi_links, start = -24) #names for the files
mapply(download.file, mpi_links, here::here("raw_data", mpi_files)) #downloads all the mpi files into folder raw_data
mpi_all_sheets <- sapply(here::here("raw_data", mpi_files), readxl::excel_sheets) #gets all the sheets
mpi_keep_sheets <- lapply(mpi_all_sheets, function(x) x[startsWith(x, "mpi")])%>% #only the sheets we want
  unlist(use.names = FALSE)
mpi_nested <- tibble(file = here::here("raw_data", mpi_files), sheet = mpi_keep_sheets)%>%
  mutate(data = map2(file, sheet, readxl::read_excel))
mpi <- data.table::rbindlist(mpi_nested$data, use.names=FALSE)%>%
  as_tibble()%>%
  janitor::clean_names()%>%
  select(estimated_cost,
         construction_type,
         region,
         project_status,
         project_category_name,
         first_entry_date,
         last_update)%>%
  mutate(source=mpi_url_to_scrape)%>%
  arrange(last_update)
#insolvencies---------
#THIS COULD EASILY BREAK... terrible file structure
#the first 134 rows of the excel file currently contain the info required.
#broken into 3 sections: total (46 rows), consumer(46 rows), business(42 rows)
insol_url_to_scrape <- "https://open.canada.ca/data/en/dataset/4444b25a-cd38-46b8-bfb8-15e5d28ba4e7"
insol_scraped <- rvest::read_html(insol_url_to_scrape)
insol_links <- rvest::html_attr(rvest::html_nodes(insol_scraped, "a"), "href") #all of the links
insol_links <- insol_links[insol_links%>%endsWith(".xlsx") & !is.na(insol_links)] #the links we want.
insol_files <- str_sub(insol_links, start = -23) #the destination file names
mapply(download.file, insol_links, here::here("raw_data", insol_files)) #download the excel files
insol <- tibble(file = here::here("raw_data", insol_files))%>% #create a dataframe with path to files
  mutate(data = map(file,
                    readxl::read_excel,
                    sheet = 1,
                    n_max = 134,
                    col_names = FALSE))%>% #THIS COULD BREAK
  unnest(data)%>%#combine all the years together
  janitor::clean_names()%>%
mutate(type = rep(c(rep("total", 46),#THIS COULD BREAK
                  rep("consumer", 46),#THIS COULD BREAK
                  rep("business", 42)),#THIS COULD BREAK
                length(insol_files)))%>%
  select(type, everything())

colnames(insol) <- c("Series", "file", "thing", month.abb)

df_list$`B.C. Insolvancies` <- insol%>%
  mutate(year = strex::str_extract_numbers(file))%>% #THIS COULD BREAK (it assumes year is in file name, but no other numbers)
  filter(thing == "British Columbia/Colombie-Britannique")%>%
  select(-file,-thing)%>%
  pivot_longer(cols = all_of(month.abb), names_to = "name", values_to = "Value")%>%
  unite("ref_date", name, year, sep="-")%>%
  mutate(Month = tsibble::yearmonth(ref_date),
         Source = insol_url_to_scrape,
         Value = as.numeric(Value))%>%
  filter(Month > tsibble::yearmonth(today()-years(10)))%>%
  select(Month, Series, Value, Source)
#S&P500--------
df_list$`S&P 500 stock price index` <- quantmod::getSymbols("^GSPC",
                                                  from = today()-years(10),
                                                  warnings = FALSE,
                                                  auto.assign = FALSE)%>%
  broom::tidy()%>%
  filter(series == "GSPC.Close")%>%
  mutate(Month = tsibble::yearmonth(index))%>%
  group_by(Month)%>%
  summarize(Value = mean(value))%>%
  mutate(Source = "yahoo finance",
         Series = "GSPC.Close")

#GDP_canada----------
df_list$`GDP Canada: Chained (2012) dollars` <- get_cansim_unfiltered("36-10-0434",
                                                                      add_label = "GDP Canada: Chained (2012) dollars",
                                                                      multiply_value_by = 1000)%>%
  filter(seasonal_adjustment == "Seasonally adjusted at annual rates",
        prices == "Chained (2012) dollars",
        north_american_industry_classification_system_naics == "All industries [T001]")%>%
  select(Month, Series, Value, Source)
#non farm payroll ----------------
nonfarm_url <- "https://data.bls.gov/pdq/SurveyOutputServlet?request_action=wh&graph_name=CE_cesbref1"
nonfarm_html <- rvest::read_html(nonfarm_url)
nonfarm_read_table <- rvest::html_nodes(nonfarm_html, "table")#COULD BREAK: assumes one table on page with data we want.
nonfarm <- rvest::html_table(nonfarm_read_table[2])[[1]] #dataframe is first element of list.
df_list$`USA Non-farm Payroll` <- nonfarm[-nrow(nonfarm), -ncol(nonfarm)]%>% #last row and column garbage.
  pivot_longer(cols = all_of(month.abb), names_to = "month", values_to = "value")%>%
  mutate(value = strex::str_extract_numbers(value),
         Value = as.numeric(value)* 1000,
         Month = tsibble::yearmonth(paste(Year,month,sep="-")),
         Series = "US non-farm payroll",
         Source = nonfarm_url)%>%
  select(Month, Series, Value, Source)
#interest rates and mortgage rates------------
interest_rates<-get_cansim_unfiltered("10-10-0122",
                                      add_label = "")%>%
   filter(rates %in% c('Treasury bill auction - average yields: 3 month',
                      'Bank rate',
                      'Selected Government of Canada benchmark bond yields: 10 years'))%>%
  select(Month, Series = rates, Value, Source)
mortgage_rates <- get_cansim_unfiltered("34-10-0145",
                                        add_label = "5 year mortgage rate")%>%
  select(Month, Series, Value, Source)
df_list$`Canadian interest rates/yields` <- bind_rows(interest_rates, mortgage_rates)
#CADUSD exchange_rate---------
#(series broken into 2 files that overlap)
exchange_rate <- get_cansim_unfiltered("33-10-0163",
                                       add_label = "CAD in USD")%>%
  filter(type_of_currency == "U.S. dollar, monthly average")%>%
  mutate(Value = 1/Value)%>%
  select(Month, Series, Value, Source)
exchange_rate2 <- get_cansim_unfiltered("10-10-0009",
                                        add_label = "CAD in USD")%>%
  filter(type_of_currency == "United States dollar, noon spot rate, average")%>%
  mutate(Value = 1/Value)%>%
  select(Month, Series, Value, Source)
exchange_rate <- bind_rows(exchange_rate, exchange_rate2)%>% #this includes overlap
  group_by(Month, Series)%>% #note that we do NOT group by source, so source gets dropped.
  summarize(Value = mean(Value, na.rm = TRUE))%>% # this gets rid of overlap
  mutate(Source = exchange_rate$Source[1]) # add the source back in.
df_list$`U.S. Exchange Rate` <- exchange_rate
#BC merchandise trade-------
df_list$`B.C. Merchandise Trade` <- get_cansim_unfiltered("12-10-0119",
                                                          add_label = "",
                                                          multiply_value_by = 1000)%>%
  filter(north_american_product_classification_system_napcs == "Total of all merchandise",
         principal_trading_partners == "All countries",
         geo == "British Columbia")%>%
  select(Month, Series = trade, Value, Source)
#tourist flows--------
df_list$`B.C. Tourism` <- get_cansim_unfiltered("24-10-0043",
                                                add_label = "")%>%
  filter(seasonal_adjustment == "Seasonally adjusted",
        traveller_characteristics %in% c("Total non resident tourists",
                                          "Total Canadian tourists returning from abroad"),
        geo == "British Columbia")%>%
  select(Month, Series = traveller_characteristics, Value, Source)
#Natural Resource indicies---------
# (normalized to index_date)
# Natural gas (US data)
natural_gas <- df_from_JSON("https://api.eia.gov/series/?api_key=27362c25d353053742ba1bfe76c54bb8&series_id=NG.RNGWHHD.M",
                            "Natural Gas Index",
                            index_date)
# Oil (US data)
oil <- df_from_JSON("https://api.eia.gov/series/?api_key=27362c25d353053742ba1bfe76c54bb8&series_id=PET.RWTC.M",
                    "Oil Index",
                    index_date)
#forestry (Canadian data) ...weird file structure... THIS COULD BREAK
forestry_url <- "https://www.bankofcanada.ca/valet/observations/group/BCPI_MONTHLY/json?start_date=1972-01-01"
forestry <- jsonlite::fromJSON(forestry_url)$observations
forestry <- forestry%>%
  as_tibble()%>%
  janitor::clean_names()%>%
  select(ref_date = d, value = m_fopr)%>%
  mutate(Month = tsibble::yearmonth(ymd(ref_date)),
         Value = as.numeric(unlist(value)),
         Series = "Forestry Index",
         Source = forestry_url)
forestry_index_value <- forestry%>%
  filter(Month == index_date)%>%
  pull(Value)
forestry <- forestry%>%
mutate(Value = 100*Value/forestry_index_value)%>%
  filter(Month > tsibble::yearmonth(today()-years(10)))%>%
  select(Month, Series, Value, Source)
df_list$`Forestry and Energy Indicies` <- bind_rows(natural_gas, oil, forestry)
#manufacturing: sales ---------------
df_list$`B.C. Manufacturing Sales` <- get_cansim_unfiltered("16-10-0048",
                                                            add_label = "Manufacturing Sales",
                                                            multiply_value_by = 1000)%>%
  filter(seasonal_adjustment == "Seasonally adjusted",
         north_american_industry_classification_system_naics == "Manufacturing [31-33]",
         geo == "British Columbia")%>%
  select(Month, Series, Value, Source)
#manufacturing: employment ---------------
df_list$`B.C. Manufacturing Employment` <- get_cansim_unfiltered("14-10-0355",
                                                                 add_label = "Manufacturing Employment",
                                                                 multiply_value_by = 1000)%>%
  filter(statistics == "Estimate",
        data_type == "Seasonally adjusted",
        north_american_industry_classification_system_naics == "Manufacturing [31-33]",
        geo == "British Columbia")%>%
  select(Month, Series, Value, Source)

#housing starts------
df_list$`B.C. Housing Starts`<- get_cansim_unfiltered("34-10-0158",
                                                      add_label = "BC housing starts",
                                                      multiply_value_by = 1000)%>%
  filter(geo == "British Columbia")%>%
  select(Month, Series, Value, Source)
#consumer price index---------
df_list$`B.C. Consumer Price Index`<- get_cansim_unfiltered("18-10-0004",
                                                            add_label = "BC consumer price index")%>%
  filter(geo == "British Columbia",
         products_and_product_groups == "All-items")%>%
  select(Month, Series, Value, Source)
#retail trade-------------
df_list$`B.C. Retail Trade`<- get_cansim_unfiltered("20-10-0008",
                                                    add_label = "B.C. Retail Trade",
                                                    multiply_value_by = 1000)%>%
  filter(geo == "British Columbia",
        north_american_industry_classification_system_naics == "Retail trade [44-45]",
        adjustments == "Seasonally adjusted")%>%
  select(Month, Series, Value, Source)
#food_and_drinking sales--------------
df_list$`B.C Food and Drink Sales` <- get_cansim_unfiltered("21-10-0019",
                                                           add_label = "BC food and drinking sales",
                                                           multiply_value_by = 1000)%>%
  filter(geo == "British Columbia",
        north_american_industry_classification_system_naics == "Total, food services and drinking places",
        seasonal_adjustment == "Seasonally adjusted")%>%
  select(Month, Series, Value, Source)
#food and drinking employment-----------
df_list$`B.C Food and Drink Employment`<- get_cansim_unfiltered("14-10-0201",
                                                                add_label = "B.C food and drinking employment")%>%
  filter(geo == "British Columbia",
         north_american_industry_classification_system_naics == "Food services and drinking places [722]",
         type_of_employee == "All employees")%>%
  select(Month, Series, Value, Source)
#business confidence------------
guess_last_report_date <- format(today()-months(1), format="%Y-%m") #THIS COULD BREAK
business_confidence_url <- paste0("https://content.cfib-fcei.ca/sites/default/files/",
                                  guess_last_report_date,
                                  "/business-barometer-data-donnes.xlsx")
download.file(business_confidence_url, here::here("raw_data", "business_confidence.xlsx"))
business_confidence <- readxl::read_excel(here::here("raw_data", "business_confidence.xlsx"),
                                          skip = 2,
                                          n_max = 31) #THIS COULD BREAK
date_column_names <- colnames(business_confidence)[-c(1:3)]%>% #THIS COULD BREAK
  str_sub(end = 5)%>% #trims off some garbage
  as.numeric()%>%
  as.Date(origin = "1899-12-30")%>%
  tsibble::yearmonth()

colnames(business_confidence) <- c("english",
                                   "french",
                                   "moving_average_description",
                                   date_column_names)

value <- business_confidence[business_confidence$english == "British Columbia", -c(1:3)]%>%
  na.omit()%>%
  unlist()

df_list$`B.C. CFIB Business Barometer Index`<- tibble(Month = date_column_names,
                                Series = "B.C. CFIB Business Barometer Index",
                                value = value,
                                Source = business_confidence_url)%>%
  group_by(Month, Series, Source)%>%
  summarize(Value = mean(value))%>% #THERE ARE SOME DUPLICATE MONTHS.... TAKE AVERAGE.
  filter(Month > tsibble::yearmonth(today()-years(10)))
#motor vehicle sales---------------
df_list$`B.C. + Territories New Vehicle Sales`<- get_cansim_unfiltered("20-10-0001",
                                                                       add_label = "Units")%>%
  filter(geo == "British Columbia and the Territories",
        seasonal_adjustment == "Unadjusted",
        vehicle_type == "Total, new motor vehicles",
        origin_of_manufacture == "Total, country of manufacture",
        sales == "Units")%>%
  select(Month, Series, Value, Source)
#lumber------------------
df_list$`B.C. Total Softwood Production`<- get_cansim_unfiltered("16-10-0017",
                                                                 add_label = "B.C. Total Softwood Production")%>%
  filter(north_american_product_classification_system_napcs == "Total softwood, production [24112]",
         geo == "British Columbia")%>%
  select(Month, Series, Value, Source)
#yvr this requries manual intervention--------------
# yvr <- "https://www.yvr.ca/en/about-yvr/facts-and-stats"
# yvr_html <- read_html(yvr)
# yvr_links <- html_nodes(yvr_html,"a") %>% html_attr("href")
# yvr_links <- yvr_links[grep("facts-sheets",yvr_links)]
# yvr_links <- yvr_links[grep("passenger",yvr_links)]
#
# yvr <- paste0("https://www.yvr.ca",yvr_links)
# enplaned_deplaned <- as.data.frame(extract_areas(yvr,into="data.frame")) #MANUAL selection from PDF
# colnames(enplaned_deplaned) <- c("year","label",month.abb)
#
# df_list$enplaned_deplaned <- enplaned_deplaned%>%
#   mutate(year=as.numeric(year))%>%
#   fill(year)%>%
#   pivot_longer(cols=-c(year,label), names_to = "month", values_to = "value")%>%
#   mutate(value=str_replace_all(value,",",""),
#          value=as.numeric(value))%>%
#   unite("ref_date",year,month,sep="-")%>%
#   mutate(ref_date=yearmonth(ref_date))
#births-----------
#function tabulizer::extract_tables complains about an illegal operation (might fail in future.)
births <- "https://www2.gov.bc.ca/gov/content/life-events/statistics-reports/births"
births_html <- rvest::read_html(births)
births_links <- rvest::html_nodes(births_html, "a") %>%
  rvest::html_attr("href")

births <- tibble(url = paste0("https://www2.gov.bc.ca",
                              births_links[grep("birth-reports",
                              births_links)]))%>% #COULD BREAK
  mutate(year = str_sub(url,start = -8, end = -5), #COULD BREAK
        data = map(url, tabulizer::extract_tables, pages = 4, output = "data.frame"), #COULD BREAK
        data = map(data, unlist, recursive = FALSE),
        data = map(data, as_tibble),
        just_the_totals = map2(data, year, get_totals, "births"),
        Source = births)%>%
  select(-data)%>%
  unnest(just_the_totals)%>%
  select(-url, -year)%>%
  arrange(Month)%>%
  filter(Month < tsibble::yearmonth(today()-months(1)),
        Month > tsibble::yearmonth(today()-years(10)))
#deaths--------------
deaths <- "https://www2.gov.bc.ca/gov/content/life-events/statistics-reports/deaths"
deaths_html <- rvest::read_html(deaths)
deaths_links <- rvest::html_nodes(deaths_html, "a") %>%
  rvest::html_attr("href")

deaths <- tibble(url = paste0("https://www2.gov.bc.ca",
                              deaths_links[grep("death-reports",
                              deaths_links)]))%>%
  mutate(year = str_sub(url, start = -8, end = -5), #COULD BREAK
         data=map(url, tabulizer::extract_tables, pages = 4, output = "data.frame"),#COULD BREAK
         data=map(data, unlist, recursive=FALSE),
         data=map(data, as_tibble),
         just_the_totals=map2(data, year, get_totals, "deaths"),
         Source = deaths)%>%
  select(-data)%>%
  unnest(just_the_totals)%>%
  select(-url, -year)%>%
  arrange(Month)%>%
  filter(Month < tsibble::yearmonth(today()-months(1)),
        Month > tsibble::yearmonth(today()-years(10)))
df_list$`B.C. Births and Deaths` <- bind_rows(births, deaths)
#interprovincial migration------------
df_list$`B.C. Interprovincial Migration`<- get_cansim_unfiltered("17-10-0020",
                                                                 add_label = "")%>%
  filter(geo == "British Columbia")%>%
  select(Month, Value, Series = interprovincial_migration, Source)
#international migration---------------
df_list$`B.C. International Migration` <- get_cansim_unfiltered("17-10-0040",
                                                                add_label = "")%>%
  filter(geo == "British Columbia")%>%
  select(Month, Value, Series = components_of_population_growth, Source)
#nest the data----------
nested_dataframe <- enframe(df_list)%>%
  mutate(value=map(value, percent_change))
#save the data-----------
write_rds(nested_dataframe, here::here("processed_data", "nested_dataframe.rds"))
write_rds(mpi, here::here("processed_data", "mpi.rds"))

