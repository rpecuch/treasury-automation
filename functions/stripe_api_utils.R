convert_to_posix <- function(df, col_name) {
  df[[col_name]] <- as.POSIXct(df[[col_name]], origin = "1970-01-01", tz = "UTC")
  return(df)
}

format_amt <- function(df, col_name){
  df[[col_name]] <- df[[col_name]] / 100
  return(df)
}

get_payouts <- function(stripe_api_key, start_date, end_date){
  res <- GET(
    url = "https://api.stripe.com/v1/payouts",
    authenticate(stripe_api_key, ""),
    query = list(limit = 60)
  )
  
  # parse data
  content_json <- content(res, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(content_json, flatten = TRUE)
  data <- parsed$data
  
  # Format
  data <- convert_to_posix(data, "arrival_date")
  data <- format_amt(data, "amount")
  
  # Filter and subset
  data_paid <- data %>% filter(status == "paid" & 
                                 arrival_date >= start_date & arrival_date <= end_date) %>%
    select(
      arrival_date, amount, id
    )
  return(data_paid)
}

get_payout_charges <- function(payout_id, stripe_api_key){
  # Stripe API endpoint
  url <- "https://api.stripe.com/v1/balance_transactions"
  
  # Make the request
  res <- GET(
    url,
    authenticate(stripe_api_key, ""),
    query = list(
      payout = payout_id,
      limit = 500
    )
  )
  
  # Parse response
  content_json <- content(res, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(content_json, simplifyDataFrame = TRUE)
  transactions <- parsed$data
  
  # Extract charges
  charges <- transactions[transactions$type != "payout", ]
  # Format
  charges <- convert_to_posix(charges, "available_on")
  charges <- format_amt(charges, "amount")
  charges <- format_amt(charges, "fee")
  charges <- format_amt(charges, "net")
  
  return(charges)
}

get_charge_details <- function(charge_id, stripe_api_key){
  url <- paste0("https://api.stripe.com/v1/charges/", charge_id)
  
  res <- GET(
    url,
    authenticate(stripe_api_key, "")
  )
  
  charge <- content(res, as = "text", encoding = "UTF-8")
  charge <- fromJSON(charge, simplifyDataFrame = TRUE)
  # Formating
  charge$created <- as.POSIXct(charge$created, origin = "1970-01-01", tz = "UTC")
  charge$amount <- charge$amount / 100
  
  return(charge)
}

get_checkout_session <- function(payment_intent_id, stripe_api_key){
  # Search checkout sessions for the payment intent
  req <- request(
    paste0(
      "https://api.stripe.com/v1/checkout/sessions",
      "?payment_intent=", payment_intent_id
    )
  ) |>
    req_auth_bearer_token(stripe_api_key)
  
  resp <- req_perform(req)
  
  sessions <- resp_body_json(resp)
  
  checkout_session_id <- sessions$data[[1]]$id
  return(checkout_session_id)
}

get_product_id <- function(checkout_session_id, stripe_api_key){
  req <- request(
    paste0(
      "https://api.stripe.com/v1/checkout/sessions/",
      checkout_session_id,
      "/line_items"
    )
  ) |>
    req_auth_bearer_token(stripe_api_key)
  
  resp <- req_perform(req)
  
  line_items <- resp_body_json(resp)
  return(line_items$data[[1]]$price$product)
}

get_product_desc <- function(product_id, stripe_api_key){
  req <- request(
    paste0(
      "https://api.stripe.com/v1/products/",
      product_id
    )
  ) |>
    req_auth_bearer_token(stripe_api_key)
  
  resp <- req_perform(req)
  
  product <- resp_body_json(resp)
  return(product$description)
}
