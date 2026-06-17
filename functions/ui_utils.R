# Format/display data
formatted_table <- function(df, pageLength=5){
  datatable(
    df,
    options = list(
      scrollX = TRUE,     # enables horizontal scrolling
      autoWidth = FALSE,   # prevents DT from resizing columns automatically
      pageLength = pageLength
    ),
    rownames = FALSE
  )
}

add_row_from_list <- function(df, row_list) {
  if (ncol(df) == 0) {
    return(as.data.frame(row_list, stringsAsFactors = FALSE))
  }
  
  # Create an empty row with NA values
  new_row <- as.list(rep(NA, ncol(df)))
  names(new_row) <- names(df)
  
  # Fill in values from the input list
  new_row[names(row_list)] <- row_list
  
  # Convert to data frame with matching column types
  new_row_df <- as.data.frame(new_row, stringsAsFactors = FALSE)
  
  # Append the row
  rbind(df, new_row_df)
}

entered_payment_list <- function(payout.date, payment.date, 
                                 status, sales.receipt.number,
                                 description, amt, fee, net, donor, email, address){
  list(
    "Payout.Date" = payout.date,
    "Payment.Date" = payment.date,
    "QuickBooks.Status" = status,
    "Sales.Receipt.Number" = sales.receipt.number,
    "Description" = description,
    "Gross" = amt,
    "Fee" = fee,
    "Net" = net,
    "Donor" = donor,
    "Email" = email,
    "Billing.Address" = address
  )
}

# Date functions
first_day_prev_month <- function(date = Sys.Date()) {
  d <- as.Date(date)
  first_this_month <- as.Date(format(d, "%Y-%m-01"))
  seq(first_this_month, length = 2, by = "-1 month")[2]
}

last_day_prev_month <- function(date = Sys.Date()) {
  d <- as.Date(date)
  first_this_month <- as.Date(format(d, "%Y-%m-01"))
  first_this_month - 1
}