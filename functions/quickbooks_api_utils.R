form_auth_url <- function(client_id, redirect_uri){
  paste0(
    "https://appcenter.intuit.com/connect/oauth2?",
    "client_id=", client_id,
    "&response_type=code",
    "&scope=com.intuit.quickbooks.accounting",
    "&redirect_uri=", URLencode(redirect_uri),
    "&state=1234"
  )
}

get_access_token <- function(client_id, client_secret, token_url, auth_code, redirect_uri){
  auth_header <- paste(
    "Basic",
    base64_encode(charToRaw(paste0(client_id, ":", client_secret)))
  )
  
  response <- POST(
    url = token_url,
    add_headers(
      Authorization = auth_header,
      `Content-Type` = "application/x-www-form-urlencoded"
    ),
    body = list(
      grant_type = "authorization_code",
      code = auth_code,
      redirect_uri = redirect_uri
    ),
    encode = "form"
  )
  
  content(response, as = "parsed")
}

get_realmID <- function(session){
  query <- parseQueryString(session$clientData$url_search)
  query$realmId
}

get_quickbooks_accts <- function(access_token, realmID, intuit_url){
  response <- GET(
    paste0(intuit_url, realmID, "/query?query=", URLencode("select * from Account", reserved=T)),
    add_headers(Authorization = paste("Bearer", access_token))
  )
  content <- content(response, as = "text")
  json_data <- fromJSON(content)
  
  # Format into data frame
  accounts <- json_data$QueryResponse$Account
  return(accounts)
}

get_quickbooks_vendors <- function(access_token, realmID, intuit_url){
  response <- GET(
    paste0(intuit_url, realmID, "/query?query=", URLencode("select * from Vendor", reserved=T)),
    add_headers(Authorization = paste("Bearer", access_token))
  )
  content <- content(response, as = "text")
  json_data <- fromJSON(content)
  
  # Format into data frame
  vendors <- json_data$QueryResponse$Vendor
  return(vendors)
}

get_quickbooks_payment_methods <- function(access_token, realmID, intuit_url){
  response <- GET(
    paste0(intuit_url, realmID, "/query?query=", URLencode("select * from PaymentMethod", reserved=T)),
    add_headers(Authorization = paste("Bearer", access_token))
  )
  content <- content(response, as = "text")
  json_data <- fromJSON(content)
  
  # Format into data frame
  methods <- json_data$QueryResponse$PaymentMethod
  return(methods)
}

get_quickbooks_customers <- function(access_token, realmID, intuit_url, page_size=1000){
  all_customers <- list()
  start_position <- 1
  
  repeat{
    response <- GET(
      paste0(intuit_url, realmID, "/query?query=", 
             URLencode(paste0("select * from Customer STARTPOSITION ", start_position, " MAXRESULTS ", page_size), reserved=T)
             ),
      add_headers(Authorization = paste("Bearer", access_token))
    )
    
    content <- content(response, as = "text")
    json_data <- fromJSON(content)
    # Format into data frame
    customers <- json_data$QueryResponse$Customer
    
    # Stop if no customers returned
    if (is.null(customers) || nrow(customers) == 0) {
      break
    }
    
    all_customers <- rbind(all_customers, customers)
    
    # Stop when fewer than requested are returned
    if (nrow(customers) < page_size) {
      break
    }
    
    start_position <- start_position + page_size
  }


  return(all_customers)
}

get_quickbooks_items <- function(access_token, realmID, intuit_url){
  response <- GET(
    paste0(intuit_url, realmID, "/query?query=", URLencode("select * from Item", reserved=T)),
    add_headers(Authorization = paste("Bearer", access_token))
  )
  content <- content(response, as = "text")
  json_data <- fromJSON(content)
  
  # Format into data frame
  items <- json_data$QueryResponse$Item
  return(items)
}

print_purchase_result <- function(response){
  content <- content(response, as = "parsed")
  
  if (response$status_code == 200){
    print(paste("The following purchase was entered successfully:",
                content$Purchase$PaymentType, "for", content$Purchase$TotalAmt))
  } else{
    print(paste("Purchase was not entered successfully:", content))
  }
}

post_purchase <- function(access_token, realm_id, intuit_url, 
                             payment_date, acct_ref, payment_type, vendor_id,
                             payment_amt, description, category_ref, payment_method_id){
  # Form URL
  url <- paste0(
    intuit_url,
    realm_id,
    "/purchase"
  )
  
  # Request body
  body <- list(
    TxnDate = payment_date,
    AccountRef = list(
      value = acct_ref # Payment acct, need to look at accts
    ),
    PaymentMethodRef = list(
      value = payment_method_id
      # name = "Debit Card"
    ),
    PaymentType = payment_type, # Payment Type can be: Cash, Check, or CreditCard.
    EntityRef = list(
      value = vendor_id,
      type = "Vendor" 
    ),
    TotalAmt = payment_amt,
    Line = list(
      list(
        Amount = payment_amt,
        Description=description,
        DetailType = "AccountBasedExpenseLineDetail",
        AccountBasedExpenseLineDetail = list(
          AccountRef = list(
            value = category_ref # this needs to be the ref for the expense category
          )
        )
      )
    )
  )
  
  # Enter expense
  response <- POST(
    url = url,
    add_headers(
      Authorization = paste("Bearer", access_token),
      Accept = "application/json",
      `Content-Type` = "application/json",
      `Accept-Encoding` = "identity"
    ),
    body = toJSON(body, auto_unbox = TRUE)
    # encode = "raw"
  )
  
  # Return result
  return(response)
}

get_sales_result <- function(response, payment_method){
  # If Customer was not created successfully
  if (is.null(response)){
    return("Not entered in Quickbooks")
  }
  
  content <- content(response, as = "parsed")
  
  if (response$status_code == 200){
    print(paste("The following sale was entered successfully:",
                payment_method, "for", content$SalesReceipt$TotalAmt))
    return(content$SalesReceipt$DocNumber)
  } else{
    print(paste("Sale was not entered successfully:", content))
    return("Not entered in Quickbooks")
  }
}

post_sale <- function(access_token, realm_id, intuit_url, payment_date,
                      donor_id,
                      donor_email,
                      payment_method_id, deposit_account_id,
                      billing_address, shipping_date, 
                      amount_positive, description_positive, item_positive_id,
                      amount_negative, description_negative, item_negative_id){
  
  # If Customer could not be created
  if (is.null(donor_id)){
    return(NULL)
  }
  
  
  # Form URL
  url <- paste0(
    intuit_url,
    realm_id,
    "/salesreceipt"
  )
  
  # Request body
  body <- list(
    # Transaction date
    TxnDate = payment_date,
    # Customer (donor)
    CustomerRef = list(
      value = donor_id
    ),
    BillEmail = list(
      Address = donor_email
    ),
    # Payment method (must exist in QuickBooks)
    PaymentMethodRef = list(
      value = payment_method_id
    ),
    # Deposit account
    DepositToAccountRef = list(
      value = deposit_account_id
    ),
    # Billing address
    # BillAddr = billing_address,
    # Shipping info
    ShipDate = shipping_date,
    # Line items (1 positive, 1 negative)
    Line = list(
      # Positive line item: payment received
      list(
        Amount = amount_positive,
        DetailType = "SalesItemLineDetail",
        Description = description_positive,
        SalesItemLineDetail = list(
          ItemRef = list(
            value = item_positive_id
          )
        )
      ),
      # Negative line item (e.g., fee, discount)
      list(
        Amount = - abs(amount_negative),  # should be negative
        DetailType = "SalesItemLineDetail",
        Description = description_negative,
        SalesItemLineDetail = list(
          ItemRef = list(
            value = item_negative_id
          )
        )
      )
    )
  )
  
  # Post sales receipt
  res <- POST(
    url = url,
    add_headers(
      Authorization = paste("Bearer", access_token),
      Accept = "application/json",
      "Content-Type" = "application/json"
    ),
    body = toJSON(body, auto_unbox = TRUE)
  )
  
  # Return result
  return(res)
}

null_to_empty <- function(x) {
  if (is.null(x)) "" else x
}

post_customer <- function(access_token, realm_id, intuit_url,
                          customer_name, email, phone, 
                          line1, line2, city, state, postal_code, country){

  # -----------------------------
  # Customer Data
  # -----------------------------
  customer_data <- list(
    DisplayName = customer_name,
    PrimaryEmailAddr = list(
      Address = null_to_empty(email)
    ),
    PrimaryPhone = list(
      FreeFormNumber = null_to_empty(phone)
    ),
    BillAddr = list(
      Line1 = null_to_empty(line1),
      Line2 = null_to_empty(line2),
      City = null_to_empty(city),
      CountrySubDivisionCode = null_to_empty(state),
      PostalCode = null_to_empty(postal_code),
      Country = null_to_empty(country)
    )
  )
  
  # -----------------------------
  # QuickBooks Customer Endpoint
  # -----------------------------
  url <- paste0(
    intuit_url,
    realm_id,
    "/customer"
  )
  # -----------------------------
  # Create Customer
  # -----------------------------
  # Post sales receipt
  print(customer_data)
  response <- POST(
    url = url,
    add_headers(
      Authorization = paste("Bearer", access_token),
      Accept = "application/json",
      "Content-Type" = "application/json"
    ),
    body = toJSON(customer_data, auto_unbox = TRUE)
  )
  content <- content(response, as = "parsed")
  print(content)
  stop()
    
  # -----------------------------
  # Output Result
  # -----------------------------
  if (!is.null(data$Customer)) {
    cat("Customer created successfully.\n")
    
    # Print created customer info
    print(data$Customer$DisplayName)
    return(data$Customer$Id)
  } else {
    print("Failed to create customer.\n")
    print(data)
    return(NULL)
  }
}
