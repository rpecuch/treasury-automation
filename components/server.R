# Source utility fns
source("functions/quickbooks_api_utils.R")
source("functions/stripe_api_utils.R")
source("functions/ui_utils.R")

# Define server
server <- function(input, output, session) {
  
  # Initialize reactive values
  source("functions/initialize_server.R")
  output$stripe_result <- renderPrint({
    "Not clicked."
  })
  
  # Listen for returned authorization code
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
  # Retrieve Stripe payouts
  if (shiny::isRunning()){
    stripe_payouts <- reactive({
      req(input$stripe_date)
      
      # Retrieve payouts within bank statement dates
      get_payouts(stripe_api_key, input$stripe_date[1], input$stripe_date[2])
    })
  } else{
    stripe_payouts <- get_payouts(stripe_api_key, first_day_prev_month(), last_day_prev_month())
  }
  
  output$stripe_payouts_table <- renderDT({
    req(stripe_payouts())
    formatted_table(stripe_payouts())
  })
  
  # Enter payments received from Stripe
  observeEvent(input$stripe, {
    # Initialize entered payments
    if (!shiny::isRunning()){
      entered_stripe_payments <- make_value(data.frame())
    } else{
      entered_stripe_payments(data.frame())
    }
    
    # Stripe payouts are 26th of every month (or next day if holiday or something), but dates are retrieved via API
    # Bank statement will just show payout total
    
    # Loop through payouts
    req(get_value(stripe_payouts))
    
    for (i in seq_len(nrow(get_value(stripe_payouts))) ){
      # Retrieve payments
      payout_id <- get_value(stripe_payouts)$id[i]
      payments <- get_payout_charges(payout_id, stripe_api_key)
      
      # Loop through payments
      for (j in seq_len(nrow(payments)) ){
        payment_type <- payments$type[j]
        
        # Stripe cardholder updates fees - expenses
        if (payment_type == "stripe_fee"){
          # Enter expense
          response <- post_purchase(
            get_value(access_token), get_value(realmID), intuit_url,
            payment_date = get_value(stripe_payouts)$arrival_date[i],
            # Bank of America Checking account
            acct_ref = expense_config$stripe$acct_ref,
            payment_type = "Cash",
            # Stripe Fee as vendor
            vendor_id = expense_config$stripe$vendor_id,
            payment_amt = abs(payments$amount[j]),
            description = paste("Stripe-", payments$description[j], ". Accounted for in payout to bank account."),
            # Stripe Fees account (expense account)
            category_ref = expense_config$stripe$category_ref,
            # Cash payment method
            payment_method_id = expense_config$stripe$payment_method_id
          )
          
          print_purchase_result(response)
          
          # Append row to table of entered stripe payments
          status <- ifelse(response$status_code == 200, "Entered Successfully", "Not Entered - Automation Failure")
          entered_details <- entered_payment_list(payout.date = get_value(stripe_payouts)$arrival_date[i],
                                                  payment.date = payments$available_on[j],
                                                  status = status,
                                                  sales.receipt.number = "N/A - expense",
                                                  description = payments$description[j],
                                                  amt = payments$amount[j],
                                                  fee = payments$fee[j],
                                                  net = payments$net[j],
                                                  donor = "N/A - expense",
                                                  email = NA, address = NA)
          
          if (!shiny::isRunning()){
            entered_stripe_payments <- make_value(add_row_from_list(get_value(entered_stripe_payments), entered_details))
          } else{
            entered_stripe_payments(add_row_from_list(get_value(entered_stripe_payments), entered_details))
          }
          
        }
        
        # Stripe payments + associated processing fees - sales
        else if (payment_type == "charge"){
          # Retrieve details
          payment_id <- payments$source[j]
          stripe_fee <- payments$fee[j]
          payment_details <- get_charge_details(payment_id, stripe_api_key)

          # If no IMO metadata, check line items on checkout session
          if (length(payment_details$metadata) == 0){
            checkout_id <- get_checkout_session(payment_details$payment_intent, stripe_api_key)
            product_id <- get_product_id(checkout_id, stripe_api_key)
            payment_desc <- get_product_desc(product_id, stripe_api_key)
            payment_desc <- paste("Stripe -", payment_desc)
          } else{
            payment_desc <- paste("Stripe -", payment_details$metadata$`In Memory/Honor of`, payment_details$metadata$`Enter Name Here`)
          }
          
          # Categorize as fundraiser or memorial donation
          payment_cat <- ifelse(str_detect(tolower(payment_desc), memorial_pattern), "item_positive_memorial", "item_positive_fundraiser")

          # Check if customer (donor) exists by email
          email <- payment_details$billing_details$email
          customer_emails <- unlist(get_value(customers)$PrimaryEmailAddr)
          customer_row <- which(tolower(customer_emails) == tolower(email))
          donor_id <- get_value(customers)$Id[customer_row]
          # Check if customer exists by name
          if (length(customer_row) == 0){
            customer_name <- payment_details$billing_details$name
            customer_names <- unlist(get_value(customers)$DisplayName)
            customer_row <- which(tolower(customer_names) == tolower(customer_name))
            donor_id <- get_value(customers)$Id[customer_row]
            
            # Create new customer
            if (length(customer_row) == 0){
              donor_id <- post_customer(get_value(access_token), get_value(realmID), intuit_url,
                            customer_name = customer_name, email = email, phone = payment_details$billing_details$phone,
                            line1 = payment_details$billing_details$address$line1,
                            line2 = payment_details$billing_details$address$line2,
                            city = payment_details$billing_details$address$city, 
                            state = payment_details$billing_details$address$state, postal_code = payment_details$billing_details$address$postal_code, 
                            country = payment_details$billing_details$address$country)
              # Retrieve updated customer list
              if (!shiny::isRunning()){
                customers <- make_value(get_quickbooks_customers(get_value(access_token), get_value(realmID), intuit_url))
              } else{
                customers(get_quickbooks_customers(get_value(access_token), get_value(realmID), intuit_url))
              }
            }
          }
          
          # Form billing address
          billing_address <- paste0(
            payment_details$billing_details$address$line1, payment_details$billing_details$address$line2,
            ", ", payment_details$billing_details$address$city, ", ", payment_details$billing_details$address$state,
            ", ", payment_details$billing_details$address$country, ", ", payment_details$billing_details$address$postal_code
          )

          # Enter payment
          # Payment ID shown in Stripe UI corresponds to payment_intent field from API
          response <- post_sale(get_value(access_token), get_value(realmID), intuit_url,
                                payment_date = get_value(stripe_payouts)$arrival_date[i], # Date of payout
                                donor_id = donor_id, # comes from Customers
                                donor_email = email,
                                # Stripe payment method
                                payment_method_id = payment_config$stripe$payment_method_id, # comes from Payment Methods
                                # payment_method = "Stripe", # must exist in Quickbooks
                                # Bank of America checking
                                deposit_account_id = payment_config$stripe$deposit_account_id, # comes from Accounts, must be of type Bank
                                billing_address = billing_address,
                                shipping_date = payment_details$created,
                                amount_positive = payment_details$amount,
                                description_positive = payment_desc,
                                # Honor/memorial gift
                                item_positive_id = payment_config$stripe[[payment_cat]], # comes from Items, should indicate memorial/honoratum gift
                                amount_negative = stripe_fee,
                                description_negative = "Stripe Processing Fee",
                                # Stripe processing charge
                                item_negative_id = payment_config$stripe$item_negative_id # comes from Items, should inidcate process change from stripe
          )
          # Print result
          sale_no <- get_sales_result(response, "Stripe")
          # Append row to table of entered stripe payments
          status <- ifelse(response$status_code == 200, "Entered Successfully", "Not Entered - Automation Failure")
          entered_details <- entered_payment_list(payout.date = get_value(stripe_payouts)$arrival_date[i],
                                                  payment.date = payment_details$created,
                                                  status = status,
                                                  sales.receipt.number = sale_no,
                                                  description = payment_desc,
                                                  amt = payment_details$amount,
                                                  fee = stripe_fee,
                                                  net = payment_details$amount - stripe_fee,
                                                  donor = payment_details$billing_details$name,
                                                  email = email, address = billing_address)

          if (!shiny::isRunning()){
            entered_stripe_payments <- make_value(add_row_from_list(get_value(entered_stripe_payments), entered_details))
          } else{
            entered_stripe_payments(add_row_from_list(get_value(entered_stripe_payments), entered_details))
          }

        }
        
        else{
          # Handling for unknown payment types
          entered_details <- entered_payment_list(payout.date = get_value(stripe_payouts)$arrival_date[i],
                                                  payment.date = payments$available_on[j],
                                                  status = "Not Entered - Unknown Type",
                                                  sales.receipt.number = NA,
                                                  description = paste("Unknown payment type:", payment_type),
                                                  amt = payments$amount[j],
                                                  fee = payments$fee[j],
                                                  net = payments$net[j],
                                                  donor = NA,
                                                  email = NA, address = NA)

          if (!shiny::isRunning()){
            entered_stripe_payments <- make_value(add_row_from_list(get_value(entered_stripe_payments), entered_details))
          } else{
            entered_stripe_payments(add_row_from_list(get_value(entered_stripe_payments), entered_details))
          }
        }
        
      }
    }
    
    # Create result message
    all_successful <- all(get_value(entered_stripe_payments)$QuickBooks.Status == "Entered Successfully")
    stripe_message <- ifelse(all_successful,
                             "All Stripe transactions from displayed payouts entered in QuickBooks successfully! Download file to see details.",
                             "Some Stripe transactions from displayed payouts were NOT entered in QuickBooks successfully. Download file to see details.")
    
    # Show in UI when completed
    output$stripe_result <- renderPrint({
      # Sys.sleep(1)
      
      stripe_message
    })
  })
  
  # Download entered Stripe payments
  output$download_stripe <- downloadHandler(
    # Filename when user downloads
    filename = function() {
      paste0("entered_stripe_payments_", Sys.Date(), ".xlsx")
    },

    # File content
    content = function(file) {
      req(get_value(entered_stripe_payments))

      write.xlsx(get_value(entered_stripe_payments), file, row.names = FALSE)
    }
  )
  
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