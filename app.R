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

# R2 TODO:
# all cardholder updates for Stripe need to be added to one of the sales transactions - verify with Mitch after meeting about May recon issues
  # if entered as expenses, cannot be matches with a bank deposit in quickbooks
# FRU updates
# modal/spinner during stripe progress
# add fundraising keywords as well and if none present prompt me to categorize

# Load environmental vars
if (Sys.getenv("APP_ENV") != "production"){
  library(dotenv)
  load_dot_env()
  options(
    shiny.port = as.numeric(Sys.getenv("LOCAL_PORT"))
  )
}

# Source UI and server
source("functions/ui_utils.R")
source("functions/server_modules.R")
source("functions/quickbooks_api_utils.R")
source("functions/stripe_api_utils.R")
source("components/global_vars.R")
source("components/ui.R")
source("components/server.R")

# Run app
shinyApp(ui, server)