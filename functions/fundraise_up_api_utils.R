api_key <- Sys.getenv("FUNDRAISE_UP_API_KEY")
livemode <- TRUE
stripe_api_key <- Sys.getenv("STRIPE_API_KEY")
  
get_donation <- function(donation_id, api_key, livemode){
  # Request donation details
  resp <- GET(
    url = paste0("https://api.fundraiseup.com/v1/donations/"),
    query = list(
      livemode = tolower(as.character(livemode)),
      # TODO: this is max, need to handle more
      limit = 100
    ),
    add_headers(
      Authorization = paste("Bearer", api_key),
      Accept = "application/json"
    )
  )
  
  # Parse the JSON response
  donations <- content(resp, as = "parsed", type = "application/json")
  
  # Look at single payment
  donation <- donations$data[[1]]
  # Payment ID
  charge_id <- donation$payment$id
}