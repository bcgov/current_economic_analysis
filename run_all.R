#!/usr/local/bin/Rscript
#Copyright 2022 Province of British Columbia
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

#This file: builds and deploys the project

#load the data, with no stopping for errors-----------
load_data <- parse(file = "01_load.R")
for (i in seq_along(load_data)){
  tryCatch(eval(load_data[[i]]),
           error = function(e) message("Oops!  ", as.character(e)))
}
#deploys the shiny app to shinyapps.io-----------
rsconnect::deployApp("02_dashboard.Rmd", forceUpdate =TRUE)
#knit the dull dashboard------------
rmarkdown::render("02_dull_dashboard.Rmd")
