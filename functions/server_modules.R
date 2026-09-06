# Initial status message
initialStatus <- function(id){
  moduleServer(id, function(input, output, session){
    output$result <- renderPrint({
      "Not clicked."
    })
  })
}

# Display payouts
displayPayouts <- function(id, api_key){
  moduleServer(id, function(input, output, session){
    output$payouts_table <- renderDT({
      get_payouts(api_key, input$dates[1], input$dates[2])
    })
  })
}

# Enter payments
enterPayments <- function(id, api_key){
  moduleServer(id, function(input, output, session){
    observeEvent(input$enter, {
      # Initialize entered payments
      payments_to_enter <- data.frame()
      unexpected_payments <- data.frame()
      # if (!shiny::isRunning()){
      #   entered_payments <- make_value(data.frame())
      # } else{
      #   entered_payments(data.frame())
      # }
      
      # Loop through payouts
      if (!shiny::isRunning()){
        payout_data <- get_payouts(stripe_api_key, "2026-07-01", "2026-07-31")
      } else{
        payout_data <- get_payouts(api_key, input$dates[1], input$dates[2])
      }
      
      # Check that authetntication has been done
      if (nrow(payout_data) == 0){
        message <- "No payouts to enter"
      }
      else if(is.null(get_value(access_token))){
        message <- "Authenticate to Intuit first"
      } else{
        # Loop through payouts
        for (i in seq_len(nrow(payout_data))){
          # Retrieve payments
          payout_id <- payout_data$id[i]
          payments <- get_payout_charges(payout_id, api_key)
          
          # Store cardholder update fees
          cau_fees <- data.frame()
          
          # Loop through payments
          for (j in seq_len(nrow(payments)) ){
            payment_type <- payments$type[j]
            
            # Stripe cardholder updates fees - expenses
            if (payment_type == "stripe_fee"){
              # Extract needed info
              cau_info <- list(
                payment_amt = abs(payments$amount[j]),
                description = payments$description[j]
              )
              
              # Add info to cau_fees
              cau_fees <- add_row_from_list(cau_fees, cau_info)
            }
            
                # Stripe payments + associated processing fees - sales
                else if (payment_type == "charge"){
                  # Retrieve details
                  payment_id <- payments$source[j]
                  stripe_fee <- payments$fee[j]
                  payment_details <- get_charge_details(payment_id, api_key)

                  # If no IMO metadata, check line items on checkout session
                  if (length(payment_details$metadata) == 0){
                    checkout_id <- get_checkout_session(payment_details$payment_intent, api_key)
                    product_id <- get_product_id(checkout_id, api_key)
                    payment_desc <- get_product_desc(product_id, api_key)
                    payment_desc <- paste("Stripe -", payment_desc)
                  } else{
                    if (str_detect(payment_details$description, "ecurring donation")){
                      payment_desc <- paste("FRU - Recurring donor")
                    } else if ("Campaign Name" %in% names(payment_details$metadata)){
                      payment_desc <- paste("FRU -", payment_details$metadata$`Campaign Name`)
                    } else{
                      payment_desc <- paste("Stripe -", payment_details$metadata$`In Memory/Honor of`, payment_details$metadata$`Enter Name Here`)
                    }
                  }

                  # Categorize as fundraiser or memorial donation
                  # TODO - add for item_positive_individual (recurring donors)
                  payment_cat <- ifelse(str_detect(tolower(payment_desc), memorial_pattern), "item_positive_memorial", "item_positive_fundraiser")

                  # Check if customer (donor) exists by email
                  email <- ifelse(id == "fundraise_up", payment_details$metadata$`Supporter Email`, payment_details$billing_details$email)
                  customer_name <- ifelse(id == "fundraise_up", paste(payment_details$metadata$`Supporter First Name`, payment_details$metadata$`Supporter Last Name`), 
                                          payment_details$billing_details$name)
                 
                  customer_emails <- unlist(get_value(customers)$PrimaryEmailAddr)
                  customer_row <- which(tolower(customer_emails) == tolower(email))
                  donor_id <- get_value(customers)$Id[customer_row]
                  # Check if customer exists by name
                  if (length(customer_row) != 1){
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
                  
                  details_to_enter <- list(
                    payment_date = get_value(payout_data)$arrival_date[i],
                    donor_id = donor_id,
                    donor_email = email,
                    payment_method_id = payment_config[[id]]$payment_method_id,
                    deposit_account_id = payment_config[[id]]$deposit_account_id,
                    billing_address = billing_address,
                    shipping_date = payment_details$created,
                    amount_positive = payment_details$amount,
                    description_positive = payment_desc,
                    item_positive_id = payment_config[[id]][[payment_cat]],
                    amount_negative = stripe_fee,
                    description_negative = "Stripe Processing Fee",
                    item_negative_id = payment_config[[id]]$item_negative_id
                  )
                  payments_to_enter <- add_row_from_list(payments_to_enter, details_to_enter)

                }
            
                else{
                  # Handling for unknown payment types
                  unexpected_payment <- entered_payment_list(payout.date = get_value(payout_data)$arrival_date[i],
                                                          payment.date = payments$available_on[j],
                                                          status = "Not Entered - Unknown Type",
                                                          sales.receipt.number = NA,
                                                          description = paste("Unknown payment type:", payment_type),
                                                          amt = payments$amount[j],
                                                          fee = payments$fee[j],
                                                          net = payments$net[j],
                                                          donor = NA,
                                                          email = NA, address = NA)
                  unexpected_payments <- add_row_from_list(unexpected_payments, unexpected_payment)

                }
            
            # entered_details <- entered_payment_list(payout.date = get_value(payout_data)$arrival_date[i],
            #                                         payment.date = payment_details$created,
            #                                         status = status,
            #                                         sales.receipt.number = sale_no,
            #                                         description = payment_desc,
            #                                         amt = payment_details$amount,
            #                                         fee = stripe_fee,
            #                                         net = payment_details$amount - stripe_fee,
            #                                         donor = customer_name,
            #                                         email = email, address = billing_address)
            
            # Add payment to payments table
            # if (payment_type != "stripe_fee"){
            #   if (!shiny::isRunning()){
            #     entered_payments <- make_value(add_row_from_list(get_value(entered_payments), entered_details))
            #   } else{
            #     entered_payments(add_row_from_list(get_value(entered_payments), entered_details))
            #   }
            # }
            
            # Enter expense
            # response <- post_purchase(
            #   get_value(access_token), get_value(realmID), intuit_url,
            #   payment_date = payout_data$arrival_date[i],
            #   # Bank of America Checking account
            #   acct_ref = expense_config[[id]]$acct_ref,
            #   payment_type = "Cash",
            #   # Stripe Fee as vendor
            #   vendor_id = expense_config[[id]]$vendor_id,
            #   payment_amt = abs(payments$amount[j]),
            #   description = paste("Stripe-", payments$description[j], ". Accounted for in payout to bank account."),
            #   # Stripe Fees account (expense account)
            #   category_ref = expense_config[[id]]$category_ref,
            #   # Stripe payment method
            #   payment_method_id = expense_config[[id]]$payment_method_id
            # )
            # 
            # print_purchase_result(response)
            # 
            # # Append row to table of entered stripe payments
            # status <- ifelse(response$status_code == 200, "Entered Successfully", "Not Entered - Automation Failure")
            # entered_details <- entered_payment_list(payout.date = payout_data$arrival_date[i],
            #                                         payment.date = payments$available_on[j],
            #                                         status = status,
            #                                         sales.receipt.number = "N/A - expense",
            #                                         description = payments$description[j],
            #                                         amt = payments$amount[j],
            #                                         fee = payments$fee[j],
            #                                         net = payments$net[j],
            #                                         donor = "N/A - expense",
            #                                         email = NA, address = NA)

          }
        }
        
        # Identify payment to include CAU update fees a part of
        if (nrow(cau_fees) > 1){
          # Get highest payment
          max_amt <- max(payments_to_enter$amount_positive)
          matches <- which(payments_to_enter$amount_positive == max_amt)
          
          # Row number of the first match
          first_match <- matches[1]
        } else{
          first_match <- 0
        }
        
        # TODO: enter payments, and add all the cau_fees as additional negative charges on the payment of greatest amt
        for (k in seq_len(nrow(payments_to_enter)) ){
          # Enter payment
          response <- post_sale(get_value(access_token), get_value(realmID), intuit_url,
                                payment_date = get_value(payout_data)$arrival_date[i], # Date of payout
                                donor_id = donor_id, # comes from Customers
                                donor_email = email,
                                # Stripe payment method
                                payment_method_id = payment_config[[id]]$payment_method_id, # comes from Payment Methods
                                # Bank of America checking
                                deposit_account_id = payment_config[[id]]$deposit_account_id, # comes from Accounts, must be of type Bank
                                billing_address = billing_address,
                                shipping_date = payment_details$created,
                                amount_positive = payment_details$amount,
                                description_positive = payment_desc,
                                # Honor/memorial gift
                                item_positive_id = payment_config[[id]][[payment_cat]], # comes from Items, should indicate memorial/honoratum gift
                                amount_negative = stripe_fee,
                                description_negative = "Stripe Processing Fee",
                                # Stripe processing charge
                                item_negative_id = payment_config[[id]]$item_negative_id # comes from Items, should inidcate process change from stripe
          )
          
          # Print result
          # sale_no <- get_sales_result(response, id)
          # 
          # # Append row to table of entered stripe payments
          # status <- "Not Entered - Automation Failure"
          # if (!is.null(response)){
          #   if (response$status_code == 200) status <- "Entered Successfully"
          # }
        }
        
        # TODO: create data frame for output, and indicate where cau_fees may be found
        
        # Create result message
        all_successful <- all(get_value(entered_payments)$QuickBooks.Status == "Entered Successfully")
        message <- ifelse(all_successful,
                                 paste(
                                   "All", id, "from displayed payouts entered in QuickBooks successfully! Download file to see details."
                                 ),
                                 paste(
                                   "Some", id, "from displayed payouts were NOT entered in QuickBooks successfully. Download file to see details."
                                 )
                               )

      }
      
      # Show in UI when completed
      output$result <- renderPrint({
        message
      })
      
      # Download entered payments
      output$download <- downloadHandler(
        # Filename when user downloads
        filename = function() {
          paste0("entered_",id, "_payments_", Sys.Date(), ".xlsx")
        },
        
        # File content
        content = function(file) {
          req(get_value(entered_payments))
          
          write.xlsx(get_value(entered_payments), file, row.names = FALSE)
        }
      )
    })
  })
}