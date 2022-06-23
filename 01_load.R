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

#This file: loads data from various web sources to be used in the current economic analysis dashboard (and other outputs?).
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
index_date <- ymd("2014-01-01")# date for indexing forestry, oil, natural gas
df_list <- list() #storage list for all dataframes
#BC labour market info---------
temp <- get_cansim_unfiltered("14-10-0287-03",
                              add_label ="",
                              source_text="Labour force characteristics by province, monthly, seasonally adjusted")%>%
  filter(geo=="British Columbia",
         sex=="Both sexes",
         age_group=="15 years and over",
         statistics=="Estimate",
         data_type=="Seasonally adjusted")%>%
  dplyr::select(`Period Starting`,
                Series=labour_force_characteristics,
                Value=val_norm,
                Source)
df_list$`B.C. Monthly Labour Market` <- temp%>%
  filter(str_detect(Series,"rate"))
df_list$`B.C. Monthly Labour Market Levels` <- temp%>%
  filter(!str_detect(Series,"rate"))
#building permits HUGE file... only download once per week.-----
permit_file <- here::here("processed_data","building_permits.rds")
permit_file_exists <- file.exists(permit_file)
if(permit_file_exists){
  permit_file_young <- now()-file.info(permit_file)$mtime < days(7)
}else{
  permit_file_young <- FALSE
}
if(permit_file_exists & permit_file_young){
  print("permit file up to date: skipping download")
  df_list$`B.C. Monthly Building Permits`<-readRDS(permit_file)
}else{
  print("permit file out of date: this could take a while")
  cansim_id <- "34-10-0066-01"
  connection <- cansim::get_cansim_sqlite(cansim_id)
  building_permits<- connection %>%
    filter(GEO=="British Columbia",
           Variables=="Value of permits",
           `Seasonal adjustment`=="Seasonally adjusted, constant",
           `Type of work`=="Types of work, total",
           `Type of structure` %in% c("Total residential", "Total non-residential", "Total residential and non-residential")
    )%>%
    cansim::collect_and_normalize()%>%
    janitor::clean_names()%>%
    mutate(
      Series = paste(type_of_structure,"(dollars)"),
      `Period Starting` = lubridate::ym(ref_date),
      Value = value * 1000,
      Source = paste("Statistics Canada. Table", cansim_id, "Building permits, by type of structure and type of work (x 1,000)")
    ) %>%
    filter(`Period Starting` > lubridate::today() - lubridate::years(11))%>%
    select(`Period Starting`, Series, Value, Source)
  saveRDS(building_permits, permit_file)
  df_list$`B.C. Monthly Building Permits` <- building_permits
  cansim::disconnect_cansim_sqlite(connection)
}
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
  mutate(`Period Starting` = date,
         Source = commodity_url)%>%
  filter(`Period Starting` > today()-years(11))%>%
  select(`Period Starting`, Series, Value, Source)
df_list$`Canada Monthly Commodity Price Indicies (Jan 1972 = 100)` <- commodity
#Major Project Inventory---------
mpi_url_to_scrape <- "https://www2.gov.bc.ca/gov/content/employment-business/economic-development/industry/bc-major-projects-inventory/recent-reports"
mpi_scraped <- rvest::read_html(mpi_url_to_scrape)
mpi_links <- rvest::html_attr(rvest::html_nodes(mpi_scraped, "a"), "href") #all the links
mpi_links <- mpi_links[mpi_links%>%startsWith("/assets/") & mpi_links%>%endsWith(".xlsx")]%>% #stubs of the links we want.
  na.omit()%>%
  head(n = 1) #(just most recent quarter)
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
  filter(last_update==max(last_update)) #file has 3 records where last update non consistent..

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

df_list$`B.C. Monthly Insolvencies` <- insol%>%
  mutate(year = strex::str_extract_numbers(file))%>% #THIS COULD BREAK (it assumes year is in file name, but no other numbers)
  filter(thing == "British Columbia/Colombie-Britannique")%>%
  select(-file, -thing)%>%
  pivot_longer(cols = all_of(month.abb), names_to = "name", values_to = "Value")%>%
  unite("ref_date", name, year, sep="-")%>%
  mutate(`Period Starting` = lubridate::my(ref_date),
         Source = insol_url_to_scrape,
         Value = as.numeric(Value))%>%
  filter(`Period Starting` > today()-years(11))%>%
  na.omit()%>%
  select(`Period Starting`, Series, Value, Source)
#S&P500--------
df_list$`USA Monthly S&P 500 (Mar 1957 = 44.03)` <- quantmod::getSymbols("^GSPC",
                                                  from = today()-years(11),
                                                  warnings = FALSE,
                                                  auto.assign = FALSE)%>%
  broom::tidy()%>%
  filter(series == "GSPC.Close")%>%
  mutate(`Period Starting` = index)%>%
  group_by(Month=tsibble::yearmonth(`Period Starting`))%>%
  summarize(Value = round(mean(value, na.rm=TRUE), 2))%>%
  mutate(`Period Starting`=lubridate::ym(Month),
        Source = "https://ca.finance.yahoo.com/quote/%5EGSPC",
        Series = "GSPC.Close"
        )%>%
  filter(`Period Starting` > today()-years(11))%>%
  select(-Month)

#GDP_canada----------
df_list$`Canada Monthly GDP` <- get_cansim_unfiltered("36-10-0434",
                                                                      add_label = "Chained (2012) dollars",
                                                                      multiply_value_by = 100000,
                                                                      "Gross domestic product (GDP) at basic prices, by industry, monthly (x 1,000,000)"
                                                                      )%>%
  filter(seasonal_adjustment == "Seasonally adjusted at annual rates",
        prices == "Chained (2012) dollars",
        north_american_industry_classification_system_naics == "All industries [T001]")%>%
  select(`Period Starting`, Series, Value, Source)
#non farm payroll ----------------
nonfarm_url <- "https://data.bls.gov/pdq/SurveyOutputServlet?request_action=wh&graph_name=CE_cesbref1"
nonfarm_html <- rvest::read_html(nonfarm_url)
nonfarm_read_table <- rvest::html_nodes(nonfarm_html, "table")#COULD BREAK: assumes one table on page with data we want.
nonfarm <- rvest::html_table(nonfarm_read_table[2])[[1]] #dataframe is first element of list.
df_list$`USA Monthly Non-farm Payroll` <- nonfarm[-nrow(nonfarm), -ncol(nonfarm)]%>% #last row and column garbage.
  pivot_longer(cols = all_of(month.abb), names_to = "month", values_to = "value")%>%
  mutate(value = strex::str_extract_numbers(value),
         Value = as.numeric(value)* 1000,
         `Period Starting` = lubridate::ym(paste(Year, month,sep="-")),
         Series = "Employees",
         Source = nonfarm_url)%>%
  arrange(`Period Starting`)%>%
  select(`Period Starting`, Series, Value, Source)%>%
  filter(`Period Starting` > today()-years(11))%>%
  na.omit()
#interest rates and mortgage rates------------
interest_rates<-get_cansim_unfiltered("10-10-0122",
                                      add_label = "",
                                      multiply_value_by = .01,
                                      source_text = "Financial market statistics, last Wednesday unless otherwise stated, Bank of Canada"
                                      )%>%
   filter(rates %in% c('Treasury bill auction - average yields: 3 month',
                      'Bank rate',
                      'Selected Government of Canada benchmark bond yields: 10 years'))%>%
  select(`Period Starting`, Series = rates, Value, Source)
mortgage_rates <- get_cansim_unfiltered("34-10-0145",
                                        add_label = "5 year mortgage rate",
                                        multiply_value_by = .01,
                                        source_text = "Canada Mortgage and Housing Corporation, conventional mortgage lending rate, 5-year term"
                                        )%>%
  select(`Period Starting`, Series, Value, Source)
df_list$`Canada Monthly Rates/Yields` <- bind_rows(interest_rates, mortgage_rates)%>%
  mutate(Source = paste(interest_rates$Source[1],mortgage_rates$Source[1], sep = " AND "))
#CADUSD exchange_rate---------
#(series broken into 2 files that overlap)
exchange_rate1 <- get_cansim_unfiltered("33-10-0163",
                                       add_label = "CAD in USD",
                                       source_text = "Monthly average foreign exchange rates in Canadian dollars, Bank of Canada"
                                       )%>%
  filter(type_of_currency == "U.S. dollar, monthly average")%>%
  mutate(Value = 1/Value)%>%
  select(`Period Starting`, Series, Value, Source)
exchange_rate2 <- get_cansim_unfiltered("10-10-0009",
                                        add_label = "CAD in USD",
                                        source_text = "Foreign exchange rates in Canadian dollars, Bank of Canada, monthly"
                                        )%>%
  filter(type_of_currency == "United States dollar, noon spot rate, average")%>%
  mutate(Value = 1/Value)%>%
  select(`Period Starting`, Series, Value, Source)
exchange_rate <- bind_rows(exchange_rate1, exchange_rate2)%>% #this includes overlap
  group_by(`Period Starting`, Series)%>% #note that we do NOT group by source, so source gets dropped.
  summarize(Value = mean(Value, na.rm = TRUE))%>% # this gets rid of overlap
  mutate(Source = paste(exchange_rate1$Source[1],exchange_rate2$Source[1], sep = " AND ")) # add the source back in.
df_list$`USA Monthly Exchange Rate` <- exchange_rate
#BC merchandise trade-------
df_list$`B.C. Monthly International Merchandise Trade` <- get_cansim_unfiltered("12-10-0119",
                                                          add_label = "",
                                                          multiply_value_by =1000,
                                                          source_text = "International merchandise trade by province, commodity, and Principal Trading Partners (x 1,000)"
                                                          )%>%
  filter(north_american_product_classification_system_napcs == "Total of all merchandise",
         principal_trading_partners == "All countries",
         geo == "British Columbia")%>%
  select(`Period Starting`, Series = trade, Value, Source)
#tourist flows--------
df_list$`B.C. Monthly visitors` <- get_cansim_unfiltered("24-10-0050-01",
                            add_label = "", 
                            source_text = "Non-resident visitors entering Canada, by country of residence")%>%
  filter(geo=="British Columbia", 
         country_of_residence=="Non-resident visitors entering Canada"
         )%>%
  select(`Period Starting`, Series=country_of_residence, Value, Source)

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
  mutate(`Period Starting` = ymd(ref_date),
         Value = as.numeric(unlist(value)),
         Series = "Forestry Index",
         Source = forestry_url)
forestry_index_value <- forestry%>%
  filter(`Period Starting` == index_date)%>%
  pull(Value)
forestry <- forestry%>%
mutate(Value = 100*Value/forestry_index_value)%>%
  filter(`Period Starting` > today()-years(11))%>%
  select(`Period Starting`, Series, Value, Source)
df_list$`Canada Monthly Forestry and Energy Indicies (Jan 2014 = 100)` <- bind_rows(natural_gas, oil, forestry)%>%
  arrange(`Period Starting`)
#manufacturing: sales ---------------
df_list$`B.C. Monthly Manufacturing Sales` <- get_cansim_unfiltered("16-10-0048",
                                                            add_label = "Manufacturing Sales (dollars)",
                                                            multiply_value_by = 1000,
                                                            source_text = "Manufacturing sales by industry and province, monthly (dollars unless otherwise noted) (x 1,000)"
                                                            )%>%
  filter(seasonal_adjustment == "Seasonally adjusted",
         north_american_industry_classification_system_naics == "Manufacturing [31-33]",
         geo == "British Columbia")%>%
  select(`Period Starting`, Series, Value, Source)
#manufacturing: employment ---------------
df_list$`B.C. Monthly Manufacturing Employment` <- get_cansim_unfiltered("14-10-0355",
                                                                 add_label = "Manufacturing Employment",
                                                                 multiply_value_by = 1000,
                                                                 source_text = "Employment by industry, monthly, seasonally adjusted (x 1,000)"
                                                                 )%>%
  filter(statistics == "Estimate",
        data_type == "Seasonally adjusted",
        north_american_industry_classification_system_naics == "Manufacturing [31-33]",
        geo == "British Columbia")%>%
  select(`Period Starting`, Series, Value, Source)

#housing starts------
df_list$`B.C. Monthly Housing`<- get_cansim_unfiltered("34-10-0158",
                                                      add_label = "starts",
                                                      multiply_value_by = 1000,
                                                      source_text = "Canada Mortgage and Housing Corporation, housing starts, all areas, Canada and provinces, seasonally adjusted at annual rates, monthly (x 1,000)"
                                                      )%>%
  filter(geo == "British Columbia")%>%
  select(`Period Starting`, Series, Value, Source)
#consumer price index---------
df_list$`B.C. Monthly CPI (April 2002 = 100)`<- get_cansim_unfiltered("18-10-0004",
                                                            add_label = "consumer price index",
                                                            source_text = "Consumer Price Index, by geography, monthly, percentage change, not seasonally adjusted, provinces, Whitehorse and Yellowknife"
                                                            )%>%
  filter(geo == "British Columbia",
         products_and_product_groups == "All-items")%>%
  select(`Period Starting`, Series, Value, Source)
#retail trade-------------
df_list$`B.C. Monthly Retail Trade`<- get_cansim_unfiltered("20-10-0008",
                                                    add_label = "Sales (dollars)",
                                                    multiply_value_by = 1000,
                                                    source_text = "Retail trade sales by province and territory (x 1,000)"
                                                    )%>%
  filter(geo == "British Columbia",
        north_american_industry_classification_system_naics == "Retail trade [44-45]",
        adjustments == "Seasonally adjusted")%>%
  select(`Period Starting`, Series, Value, Source)
#food_and_drinking sales--------------
df_list$`B.C. Monthly Food and Drink Sales` <- get_cansim_unfiltered("21-10-0019",
                                                           add_label = "Total (dollars)",
                                                           multiply_value_by = 1000,
                                                           source_text = "Monthly survey of food services and drinking places (x 1,000)"
                                                           )%>%
  filter(geo == "British Columbia",
        north_american_industry_classification_system_naics == "Total, food services and drinking places",
        seasonal_adjustment == "Seasonally adjusted")%>%
  select(`Period Starting`, Series, Value, Source)
#food and drinking employment-----------
df_list$`B.C. Monthly Food and Drink Employment`<- get_cansim_unfiltered("14-10-0201",
                                                                add_label = "Unadjusted Employment",
                                                                source_text = "Employment by industry, monthly, unadjusted for seasonality"
                                                                )%>%
  filter(geo == "British Columbia",
         north_american_industry_classification_system_naics == "Food services and drinking places [722]",
         type_of_employee == "All employees")%>%
  select(`Period Starting`, Series, Value, Source)
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
  as.Date(origin = "1899-12-30")

colnames(business_confidence) <- c("english",
                                   "french",
                                   "moving_average_description",
                                   date_column_names)

value <- business_confidence[business_confidence$english == "British Columbia", -c(1:3)]%>%
  na.omit()%>%
  unlist()

df_list$`B.C. Monthly CFIB Business Barometer`<- tibble(`Period Starting` = date_column_names,
                                Series = "CFIB Index",
                                value = value,
                                Source = business_confidence_url)%>%
  group_by(`Period Starting`, Series, Source)%>%
  summarize(Value = mean(value))%>% #THERE ARE SOME DUPLICATE MONTHS.... TAKE AVERAGE.
  filter(`Period Starting` > today()-years(11))
#motor vehicle sales---------------
df_list$`B.C. Monthly New Vehicle Sales`<- get_cansim_unfiltered("20-10-0001",
                                                                       add_label = "Units",
                                                                       source_text = "New motor vehicle sales"
                                                                       )%>%
  filter(geo == "British Columbia and the Territories",
        seasonal_adjustment == "Unadjusted",
        vehicle_type == "Total, new motor vehicles",
        origin_of_manufacture == "Total, country of manufacture",
        sales == "Units")%>%
  select(`Period Starting`, Series, Value, Source)
#lumber------------------
df_list$`B.C. Monthly Total Softwood`<- get_cansim_unfiltered("16-10-0017-05",
                                                                 add_label = "Production (cubic meters)",
                                                                 multiply_value_by = 1000,
                                                                 source_text = "British Columbia lumber production, monthly (x 1,000)"
                                                                 )%>%
  filter(north_american_product_classification_system_napcs == "Total softwood, production [24112]",
         geo == "British Columbia")%>%
  select(`Period Starting`, Series, Value, Source)
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
        just_the_totals = map2(data, year, get_totals, "births")
        )%>%
  select(-data)%>%
  unnest(just_the_totals)%>%
  select(-url, -year)%>%
  arrange(`Period Starting`)%>%
  filter(`Period Starting` < today()-months(1),
        `Period Starting` > today()-years(11))
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
         just_the_totals=map2(data, year, get_totals, "deaths")
         )%>%
  select(-data)%>%
  unnest(just_the_totals)%>%
  select(-url, -year)%>%
  arrange(`Period Starting`)%>%
  filter(`Period Starting` < today()-months(1),
        `Period Starting` > today()-years(11))

df_list$`B.C. Monthly Births and Deaths` <- bind_rows(births, deaths)%>%
  pivot_wider(id_cols = c(`Period Starting`),
              names_from = Series, 
              values_from = Value)%>%
  mutate(`net difference` = births - deaths)%>%
  pivot_longer(cols = -`Period Starting`, names_to = "Series", values_to = "Value")%>%
  mutate(Source="https://www2.gov.bc.ca/gov/content/life-events/statistics-reports")

#interprovincial migration------------
df_list$`B.C. Quarterly Interprovincial Migration`<- get_cansim_unfiltered("17-10-0020",
                              add_label = "",
                              source_text = "Estimates of the components of interprovincial migration, quarterly"
)%>%
  filter(geo == "British Columbia")%>%
  select(`Period Starting`, Value, Series = interprovincial_migration, Source)%>%
  pivot_wider(id_cols = c(`Period Starting`,Source), names_from = Series, values_from = Value)%>%
  mutate(`Net inter-provincial in-migration`=`In-migrants`-`Out-migrants`)%>%
  pivot_longer(cols=-c(`Period Starting`,Source), names_to = "Series", values_to = "Value")
#international migration---------------
df_list$`B.C. Quarterly International Migration` <- get_cansim_unfiltered("17-10-0040",
                              add_label = "",
                              source_text = "Estimates of the components of international migration, quarterly"
)%>%
  filter(geo == "British Columbia")%>%
  select(`Period Starting`, Value, Series = components_of_population_growth, Source)%>%
  pivot_wider(id_cols = c(`Period Starting`,Source), names_from = Series, values_from = Value)%>%
  mutate(`Net international in-migration`=Immigrants+`Net non-permanent residents`-Emigrants-`Net temporary emigrants`+`Returning emigrants`)%>%
  pivot_longer(cols=-c(`Period Starting`,Source), names_to = "Series", values_to = "Value")
# experimental economic activity index-----------
df_list$`B.C. Monthly Economic Activity (Jan 2002=100)` <- get_cansim_unfiltered("36-10-0633-01",
                      add_label = "Simple index",
                      source_text = "Experimental indexes of economic activity in the provinces and territories"
)%>%
  filter(geo=="British Columbia",
         activity_index=="Simple economic activity index"
  )%>%
  select(`Period Starting`, Series, Value, Source)
#Real-time Local Business Condition Index (RTLBCI)-----------
df_list$`B.C. Weekly Local Business Condition Index (Aug 2020=100)`<-get_cansim_unfiltered("33-10-0398-01",
                            add_label = "",
                            source_text = "Real-time Local Business Condition Index (RTLBCI)",
                            date_parse = lubridate::ymd
)%>%
  janitor::clean_names()%>%
  filter(geo %in% c("Vancouver, British Columbia (0973)","Victoria, British Columbia (0984)")) %>%
  mutate(ref_date=lubridate::ymd(ref_date))%>%
  select(`Period Starting` = ref_date, Series = geo, Value=value, Source=source)


#nest the data to calculate some statistics----------
nested_dataframe <- enframe(df_list)%>%
  mutate(value=map(value, make_stats))

#prepare data for heatmap----------
for_heatmap <- nested_dataframe%>%
  filter(str_detect(name, "Monthly"))%>%
  mutate(measure =map(value, function(x) x %>% select(`Series`,
                                                      `Period Starting`, 
                                                      `Binned Level`, 
                                                      `Rescaled Level`,
                                                      `Binned Change`,
                                                      `Rescaled Change`,
                                                      `Binned Annual Change`,
                                                      `Rescaled Annual Change`
                                                      )))%>%
  select(-value)%>%
  unnest(measure)%>%
  unite(longname, name, Series, sep=": ")%>%
  arrange(`Period Starting`)%>%
  filter(`Period Starting` > today()-years(10))

#notable recent MONTHLY levels-------------
low <- for_heatmap%>%
  group_by(longname)%>%
  filter(`Period Starting`==max(`Period Starting`))%>%
  select(`Period Starting`, `Rescaled Level`)%>%
  ungroup()%>%
  slice_min(`Rescaled Level`, n = 5)%>%
  mutate(relatively="low")

high <- for_heatmap%>%
  group_by(longname)%>%
  filter(`Period Starting`==max(`Period Starting`))%>%
  select(`Period Starting`, `Rescaled Level`)%>%
  ungroup()%>%
  slice_max(`Rescaled Level`, n = 5)%>%
  mutate(relatively="high")
#look for fresh data-------------

notables <- bind_rows(high, low)

#prepare data for for up down page------------
for_up_down <- nested_dataframe%>%
  unnest(value)%>%
  group_by(name)%>%
  filter(`Period Starting`==max(`Period Starting`))%>%
  select(name, Series, `Period Starting`, Value, Change, `Annual Change`)%>%
  unite(Name, name, Series, sep=": ")%>%
  arrange(desc(Name))

commentary <- notables%>%
  left_join(for_up_down, by=c("longname"="Name","Period Starting"="Period Starting"))%>%
  arrange(relatively, longname)%>%
  mutate(commentary=paste("<b>",
                          longname,
                          "</b>=",
                          my_formatter(longname, Value),
                          "in the month starting",
                          `Period Starting`,
                          if_else(Change>0,"up","down"),
                          scales::percent(abs(Change), accuracy = .1),
                          "from the previous month and",
                          if_else(`Annual Change`>0,"up","down"),
                          scales::percent(abs(`Annual Change`), accuracy = .1),
                          "from the previous year."), sep=" ")%>%
  ungroup()%>%
  select(relatively, commentary)

#save the data-----------
print(paste("using data from ", dim(nested_dataframe)[1], "sources"))
write_rds(for_random_forest, here::here("processed_data", "for_random_forest.rds"))
write_rds(commentary, here::here("processed_data", "commentary.rds"))
write_rds(for_up_down, here::here("processed_data", "for_up_down.rds"))
write_rds(nested_dataframe, here::here("processed_data", "nested_dataframe.rds"))
write_rds(mpi, here::here("processed_data", "mpi.rds"))
write_rds(for_heatmap, here::here("processed_data", "for_heatmap.rds"))



