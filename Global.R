###############################################################################
#                         LOAD PACKAGES AND MODULES                          #
###############################################################################

library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(DT)
library(plotly)
library(purrr)
library(glue)
library(rhandsontable)
library(tidyr)
library(shinyalert)
library(shinyjs)
library(lubridate)
library(vistime)
library(timevis)
library(rmarkdown)
library(digest)
library(shinyauthr)
library(tibble)
library(RCurl)
library(data.table)
library(googlesheets4)
#-------------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------LOAD DATA----------------------------------------------------------------------------------------------------------                                     #


data_schedule <- read.csv(url("https://raw.githubusercontent.com/tesfahunboshe/Workforce-Management-App-with-Shiny/main/Data/schedule.csv"))
data_coverage <- read.csv(url("https://raw.githubusercontent.com/tesfahunboshe/Workforce-Management-App-with-Shiny/main/Data/coverage.csv"))
data_requirement <- read.csv(url("https://raw.githubusercontent.com/tesfahunboshe/Workforce-Management-App-with-Shiny/main/Data/requirement.csv"))


Agent_names <- unique(data_schedule$Agent.name)
market_names <- unique(data_coverage$Market)


# For submissions 

fieldsAll <- c("name","email", "market","favourite_shift", "any_nowork_interval", "nowork_intervals")
responsesDir <- file.path("responses")
epochTime <- function() {
  as.integer(Sys.time())
}

# Mandatory fields
fieldsMandatory <- c("name","email", "market","favourite_shift", "any_nowork_interval", "nowork_intervals")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# mark the mandatory fields sign red
appCSS <-
  ".mandatory_star { color: red; }
   #error { color: red; }"

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

# will retrieve all submissions
loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  data <- do.call(rbind, data)
  data
}

# admin users
# adminUsers <- c("XX", "YY") 


# google sheet link

# gs4_auth_configure(api_key = "A________________5xOA")
# SHEET_ID <- gs4_get("https://docs.google.com/")

