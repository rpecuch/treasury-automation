library(shiny)
library(httr)
library(httr2)
library(jsonlite)
library(openssl)
library(DT)
library(lubridate)
library(dplyr)
library(openxlsx)
library(stringr)
library(shinycssloaders)

# T0DO: modal/spinner during stripe progress

# R2 TODO:
# add fundraising keywords as well and if none present prompt me to categorize

# Load environmental vars
if (Sys.getenv("APP_ENV") == "DEVELOPMENT"){
  library(dotenv)
  load_dot_env()
  options(
    shiny.port = as.numeric(Sys.getenv("LOCAL_PORT"))
  )
}

# Source UI and server
source("functions/ui_utils.R")
source("functions/quickbooks_api_utils.R")
source("functions/stripe_api_utils.R")
source("components/ui.R")
source("components/server.R")

# Run app
shinyApp(ui, server)