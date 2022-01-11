###############################################################################
#                         LOAD PACKAGES AND MODULES                          #
###############################################################################

library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(ggplot2)
library(ggrepel)
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

market_names <- unique(data_coverage$Market)
market_names2 <- unique(data_schedule$Market)
names(data_schedule)[1] <- "Agent.name"
Agent_names <- unique(data_schedule[,1])
names(data_requirement)[1] <- "Interval"
names(data_coverage)[1] <- "Interval"
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
