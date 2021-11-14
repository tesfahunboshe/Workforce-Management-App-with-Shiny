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
# library(supercaliheatmapwidget)
library(lubridate)
library(vistime)
library(timevis)
library(rmarkdown)
library(digest)
library(shinyauthr)
library(tibble)

###############################################################################
#                               LOAD DATA                                     #
###############################################################################


data_schedule <- read.csv("./Data/schedule.csv")
data_coverage <- read.csv("./Data/coverage.csv")
data_requirement <- read.csv("./Data/requirement.csv")


Agent_names <- unique(data_schedule$Agent.name)
market_names <- unique(data_coverage$Market)


# For submissions 

fieldsAll <- c("name","email", "market","favourite_shift", "any_nowork_interval", "nowork_intervals")
responsesDir <- file.path("responses")
epochTime <- function() {
  as.integer(Sys.time())
}

fieldsMandatory <- c("name","email", "market","any_nowork_interval", "nowork_intervals")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

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
adminUsers <- c("Tesfa", "sally") 


