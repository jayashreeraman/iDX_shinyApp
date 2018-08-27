### ADD LIBRARIES TO THIS SECTION
require(shiny)||install.packages("shiny")
require(dplyr)||install.packages("dplyr")
require(shinyjs)||install.packages("shinyjs")
require(V8)||install.packages("V8")
require(shinydashboard)||install.packages("shinydashboard")
require(shinythemes)||install.packages("shinythemes")
require(DT)||install.packages("DT")
require(plotly)||install.packages("plotly")
require(zipcode)||install.packages("zipcode")
require(tidyr)||install.packages("tidyr")
require(digest)||install.packages("digest")
require(scales) || install.packages("scales")
require(ggrepel) || install.packages("ggrepel")
##################################

# Reactive value for adding the asset
value1 <- "Lightspeed 16"
value2 <- "GE"

# Read base data file
machine_data <- read.csv("mockdata.csv",stringsAsFactors= F, row.names = NULL)
metaTable <- machine_data
metaTable$row.names <- NULL

# remove warnings from console
options(warn = -1)

# source scripts for running secondary functions
source("mapping.R")
source("pricing_info.R")
source("dashboard.R")
