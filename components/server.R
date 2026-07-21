# Define server
server <- function(input, output, session) {
  
  # Initialize reactive values
  source("functions/initialize_server.R")
  
  # Show initial status
  initialStatus("stripe")
  initialStatus("fundraise_up")
  
  # Quickbooks authentication
  observe({
    query <- parseQueryString(session$clientData$url_search)
  
    if (!is.null(query$code)) {
      # Exchange code for access token
      auth_code <- query$code
      content_parsed <- get_access_token(client_id, client_secret, token_url, auth_code, redirect_uri)

      # Set access token
      if (!shiny::isRunning()){
        access_token <- make_value(Sys.getenv("ACCESS_TOKEN"))
        realmID <- make_value(Sys.getenv("REALM_ID"))
      } else{
        access_token(content_parsed$access_token)
        # print(get_value(access_token))
        # Retrieve realmID
        realmID(get_realmID(session))
      }
      
      # Retrieve needed information from QuickBooks
      if (!shiny::isRunning()){
        # Retrieve accts
        accts <- make_value(get_quickbooks_accts(get_value(access_token), get_value(realmID), intuit_url))
        # Retrieve vendors
        vendors <- make_value(get_quickbooks_vendors(get_value(access_token), get_value(realmID), intuit_url))
        # Retrieve payment methods
        payment_methods <- make_value(get_quickbooks_payment_methods(get_value(access_token), get_value(realmID), intuit_url))
        # Retrieve customers (donors)
        customers <- make_value(get_quickbooks_customers(get_value(access_token), get_value(realmID), intuit_url))
        # Retrieve items
        items <- make_value(get_quickbooks_items(get_value(access_token), get_value(realmID), intuit_url))
      } else{
        # Retrieve accts
        accts(get_quickbooks_accts(get_value(access_token), get_value(realmID), intuit_url))
        # Retrieve vendors
        vendors(get_quickbooks_vendors(get_value(access_token), get_value(realmID), intuit_url))
        # Retrieve payment methods
        payment_methods(get_quickbooks_payment_methods(get_value(access_token), get_value(realmID), intuit_url))
        # Retrieve customers (donors)
        customers(get_quickbooks_customers(get_value(access_token), get_value(realmID), intuit_url))
        # Retrieve items
        items(get_quickbooks_items(get_value(access_token), get_value(realmID), intuit_url))
      }

      # Display result
      auth_message <- ifelse(
        "error" %in% names(content_parsed),
        paste("Error:", content_parsed$error),
        "Authenticatation success!"
      )
      output$token_output <- renderPrint({
        auth_message
      })
    }
  })
  
  ## Sales transactions
  
  # Display Stripe payouts
  # Stripe payouts are 26th of every month (or next day if holiday or something), but dates are retrieved via API
  # Bank statement will just show payout total
  displayPayouts("stripe", stripe_api_key)
  
  # TODO: Retrieve and display Fundraise Up payouts
  # displayPayouts("fundraise_up", stripe_api_key)

  # Enter payments received from Stripe
  enterPayments("stripe", stripe_api_key)
  
  # TODO: Enter payments received from Fundraise Up dashboard
  # enterPayments("fundraise_up", stripe_api_key)
  
  
  ## Expenses
  # Enter check
  # observeEvent(input$check_entry, {
  #   
  #   # Flow:
  #   # user uploads spreadsheet with checks (amts, description, date)
  #   # API prompts to choose vendor for each (or add one if not present)
  #   # click button to enter the check expenses
  #   # Enter example expense
  #   response <- post_purchase(access_token(), realmID(), intuit_url, 
  #                             payment_date = "2026-04-15", acct_ref = "35", # ID for acct making payment - for a check the account type needs to be Bank
  #                             payment_type = "Check", vendor_id = "56", # ID for vendor receiving payment
  #                             payment_amt = 53.3, description = "blankets2", # TODO: add check number here from user
  #                             category_ref = "7", # ID for expense category
  #                             payment_method_id = "2" # Does not get used for check type, but no harm having it in the code
  #   )
  #   
  #   # Print result
  #   print_purchase_result(response)
  # })
  # 
  # # Enter debit payment
  # observeEvent(input$debit_entry, {
  #   
  #   # Flow:
  #   # user uploads spreadsheet with debits (amts, description, date)
  #   # API prompts to choose vendor for each (or add one if not present)
  #   # click button to enter the debit expenses
  #   # Enter example expense
  #   response <- post_purchase(access_token(), realmID(), intuit_url, 
  #                             payment_date = "2026-04-15", acct_ref = "35", # ID for acct making payment - for a debit card the account type needs to be Bank
  #                             payment_type = "Cash", vendor_id = "30", # ID for vendor receiving payment
  #                             payment_amt = 115, description = "quickbooks annual fee", # TODO: add check number here from user
  #                             category_ref = "8", # ID for expense category
  #                             payment_method_id = "1" # This is what will indicate debit
  #   )
  #   # Print result
  #   print_purchase_result(response)
  # })
  # 
  # # Enter EFT payment
  # observeEvent(input$eft_entry, {
  #   # Flow:
  #   # user uploads spreadsheet with debits (amts, description, date)
  #   # API prompts to choose vendor for each (or add one if not present)
  #   # click button to enter the debit expenses
  #   # Enter example expense
  #   response <- post_purchase(access_token(), realmID(), intuit_url,
  #                             payment_date = "2026-04-16", acct_ref = "35", # ID for acct making payment - for an EFT the account type needs to be Bank
  #                             payment_type = "Cash", vendor_id = "30", # ID for vendor receiving payment
  #                             payment_amt = 0.44, description = "", # TODO: add check number here from user
  #                             category_ref = "8", # ID for expense category
  #                             payment_method_id = "1" # This is what will indicate EFT
  #   )
  #   # Print result
  #   print_purchase_result(response)
  # })
  # 
  # # Enter zelle payment
  # # TODO: consider taking advantage of ref no field in quickbooks (only if makes matching better)
  # observeEvent(input$eft_entry, {
  #   # Flow:
  #   # user uploads spreadsheet with zelle (amts, description, date)
  #   # API prompts to choose vendor for each (or add one if not present)
  #   # click button to enter the expenses
  #   # Enter example expense
  #   response <- post_purchase(access_token(), realmID(), intuit_url,
  #                             payment_date = "2026-04-16", acct_ref = "35", # ID for acct making payment - for zelle the account type needs to be Bank
  #                             payment_type = "Cash", vendor_id = "30", # ID for vendor receiving payment
  #                             payment_amt = 420, description = "website revision fee",
  #                             category_ref = "8", # ID for expense category
  #                             payment_method_id = "1" # This is what will indicate zelle
  #   )
  #   # Print result
  #   print_purchase_result(response)
  # })
}