fundaise_up_auth <- function(){
  # Base URL (replace with actual Fundraise Up endpoint)
  base_url <- "https://api.fundraiseup.com/v1/"
  
  # Example: test authentication with a GET request
  response <- GET(
    url = paste0(base_url, "donations"),  # example endpoint
    add_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    )
  )
  
  # Check status
  status_code(response)
  
  # Parse response
  content(response, as = "text", encoding = "UTF-8")
}