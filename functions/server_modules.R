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
      # Initialize payments to enter
      payments_to_enter <- data.frame()
      unexpected_payments <- data.frame()
      
      # Initialize entered payments
      if (!shiny::isRunning()){
        entered_payments <- make_value(data.frame())
      } else{
        entered_payments(data.frame())
      }
      
      # Loop through payouts
      if (!shiny::isRunning()){
        id <- "fundraise_up"
        api_key <- fru_stripe_api_key
        payout_data <- get_payouts(api_key, "2026-07-01", "2026-07-31")
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
          cau_fees <- list()
          
          # Loop through payments
          for (j in seq_len(nrow(payments)) ){
            payment_type <- payments$type[j]
            
            # Stripe cardholder updates fees - expenses
            if (payment_type == "stripe_fee"){
              # Extract needed info
              cau_info <- list(
                Amount = - abs(payments$amount[j]),
                DetailType = "SalesItemLineDetail",
                Description = payments$description[j],
                SalesItemLineDetail = list(
                  ItemRef = list(
                    value = payment_config[[id]]$item_negative_id
                  )
                ),
                payment_date = payments$created[j]
              )
              
              # Add info to cau_fees
              cau_fees <- append(cau_fees, list(cau_info))
            }
            
                # Stripe payments + associated processing fees - sales
                else if (payment_type == "charge"){
                  # Retrieve details
                  payment_id <- payments$source[j]
                  stripe_fee <- payments$fee[j]
                  payment_details <- get_charge_details(payment_id, api_key)

                  # If no IMO metadata, check line items on checkout session
                  desc_label <- ifelse(id == "fundraise_up", "FRU", "Stripe")
                  if (length(payment_details$metadata) == 0){
                    checkout_id <- get_checkout_session(payment_details$payment_intent, api_key)
                    product_id <- get_product_id(checkout_id, api_key)
                    payment_desc <- get_product_desc(product_id, api_key)
                    # payment_desc <- paste("Stripe -", payment_desc)
                  } else{
                    if (str_detect(payment_details$description, "ecurring donation")){
                      payment_desc <- paste("Recurring donor")
                    } else if ("Campaign Name" %in% names(payment_details$metadata)){
                      payment_desc <- ifelse(str_detect(payment_details$metadata$`Campaign Name`, "Donate - Top Right"),
                                             payment_details$description, payment_details$metadata$`Campaign Name`)
                    } else{
                      payment_desc <- paste(payment_details$metadata$`In Memory/Honor of`, payment_details$metadata$`Enter Name Here`)
                    }
                  }
                  payment_desc <- paste(desc_label, payment_desc, sep = " - ")

                  # Categorize as fundraiser or memorial donation
                  payment_cat <- case_when(
                    str_detect(tolower(payment_desc), memorial_pattern) ~ "item_positive_memorial",
                    str_detect(tolower(payment_desc), indv_pattern) ~ "item_positive_individual",
                    .default = "item_positive_fundraiser"
                  )

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
                    item_negative_id = payment_config[[id]]$item_negative_id,
                    customer_name = customer_name
                  )
                  payments_to_enter <- add_row_from_list(payments_to_enter, details_to_enter)

                }
            
                else{
                  # Handling for unknown payment types
                  unexpected_payment <- entered_payment_list(payout.date = get_value(payout_data)$arrival_date[i],
                                                          payment.date = payments$created[j],
                                                          status = "Not Entered - Unknown Type",
                                                          sales.receipt.number = NA,
                                                          description = paste("Unknown payment type:", payment_type),
                                                          amt = payments$amount[j],
                                                          fee = payments$fee[j],
                                                          net = payments$net[j],
                                                          donor = NA,
                                                          email = NA, address = NA)
                  entered_payments <- add_row_from_list(entered_payments, unexpected_payment)

                }

          }
        }
        
        # Identify payment to include CAU update fees a part of
        if (length(cau_fees) > 1){
          # Get highest payment
          max_amt <- max(payments_to_enter$amount_positive)
          matches <- which(payments_to_enter$amount_positive == max_amt)
          
          # Row number of the first match
          first_match <- matches[1]
        } else{
          first_match <- 0
        }
        
        # Enter payments
        for (k in seq_len(nrow(payments_to_enter)) ){
          payment_row <- as.list(payments_to_enter[k, ])
          
          # Include CAU fees if applicable
          cau_update_fees <- NULL
          if (k == first_match){
            cau_update_fees <- cau_fees
            cau_update_fees <- lapply(cau_fees, function(x) {
              x[["payment_date"]] <- NULL
              x
            })
            
          }
          
          # Enter payment
          response <- post_sale(get_value(access_token), get_value(realmID), intuit_url,
                                payment_date = payment_row$payment_date, # Date of payout to bank account
                                donor_id = payment_row$donor_id, # ID for customer in Quickbooks
                                donor_email = payment_row$donor_email,
                                payment_method_id = payment_row$payment_method_id, # ID for payment method in Quickbooks
                                deposit_account_id = payment_row$deposit_account_id, # ID for Bank of America checking account in Quickbooks, must be of type Bank
                                billing_address = payment_row$billing_address,
                                shipping_date = payment_row$shipping_date, # Date payment was made
                                amount_positive = payment_row$amount_positive, # Gross amount of payment
                                description_positive = payment_row$description_positive, # Reason for payment
                                item_positive_id = payment_row$item_positive_id, # ID for Item in Quickbooks that is category for the reason for payment
                                amount_negative = payment_row$amount_negative, # Fee amount
                                description_negative = payment_row$description_negative, # Reason for fee
                                item_negative_id = payment_row$item_negative_id, # ID for Item in Quickbooks that is category for the reason for fee
                                cau_update_fees = cau_update_fees
          )
          
          # Print result
          sale_no <- get_sales_result(response, id)

          # Get status
          status <- "Not Entered - Automation Failure"
          if (!is.null(response)){
            if (response$status_code == 200) status <- "Entered Successfully"
          }
          
          # Compile details for output
          entered_details <- entered_payment_list(payout.date = payment_row$payment_date,
                                                  payment.date = payment_row$shipping_date,
                                                  status = status,
                                                  sales.receipt.number = sale_no,
                                                  description = payment_row$description_positive,
                                                  amt = payment_row$amount_positive,
                                                  fee = payment_row$amount_negative,
                                                  net = payment_row$amount_positive - payment_row$amount_negative,
                                                  donor = payment_row$customer_name,
                                                  email = payment_row$donor_email, address = payment_row$billing_address)
          
          # Add payment to payments table
          if (!shiny::isRunning()){
            entered_payments <- make_value(add_row_from_list(get_value(entered_payments), entered_details))
          } else{
            entered_payments(add_row_from_list(get_value(entered_payments), entered_details))
          }
          
          # Add CAU fees if applicable
          if (k == first_match){
            for (cau_fee in cau_fees){
              entered_fees <- entered_payment_list(payout.date = payout_data$arrival_date[i],
                                                      payment.date = cau_fee$payment_date,
                                                      status = status,
                                                      sales.receipt.number = sale_no,
                                                      description = cau_fee$Description,
                                                      amt = cau_fee$Amount,
                                                      fee = NA,
                                                      net = cau_fee$Amount,
                                                      donor = NA,
                                                      email = NA, address = NA)
              
              
              # Add to payments table
              if (!shiny::isRunning()){
                entered_payments <- make_value(add_row_from_list(get_value(entered_payments), entered_fees))
              } else{
                entered_payments(add_row_from_list(get_value(entered_payments), entered_fees))
              }
            }
            
          }
        }
        
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