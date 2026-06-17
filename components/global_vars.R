# --- CONFIG ---
client_id <- Sys.getenv("CLIENT_ID")
client_secret <- Sys.getenv("CLIENT_SECRET")
redirect_uri <-  Sys.getenv("REDIRECT_URI")
token_url <- "https://oauth.platform.intuit.com/oauth2/v1/tokens/bearer"

if (Sys.getenv("APP_ENV") == "DEVELOPMENT"){
  intuit_url <- "https://sandbox-quickbooks.api.intuit.com/v3/company/"
} else{
  intuit_url <- "https://quickbooks.api.intuit.com/v3/company/"
}

# API keyss
stripe_api_key <- Sys.getenv("STRIPE_API_KEY")

# Configurable inputs for expense and payment entries
if (Sys.getenv("APP_ENV") == "DEVELOPMENT"){
  expense_path <- "config/expense_config_SANDBOX.json"
  payment_path <- "config/payment_config_SANDBOX.json"
} else{
  expense_path <- "config/expense_config.json"
  payment_path <- "config/payment_config.json"
}
expense_config <- read_json(expense_path)
payment_config <- read_json(payment_path)

# Keywords for differentiating between memorial and fundraising donations
memorial_keywords <- c(
  "memory",
  "memorial",
  "honor",
  "honour",
  "passed away",
  "deceased",
  "tribute",
  "dear",
  "beloved",
  "obituary", 
  "funeral",
  "remembrance",
  "remembering",
  "condolences"
)
memorial_pattern <- regex(
  paste(str_replace_all(str_to_lower(str_trim(memorial_keywords)),
                        "([.|()\\[\\]{}+*?^$\\\\])", "\\\\\\1"),
        collapse = "|"),
  ignore_case = TRUE
)