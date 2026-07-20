# Stripe payments
downloadServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Download entered payments
    output$download <- downloadHandler(
      # Filename when user downloads
      filename = function() {
        paste0("entered_",id, "_payments_", Sys.Date(), ".xlsx")
      },
      
      # File content
      content = function(file) {
        # data <- get_value(entered_stripe_payments)
        
        # Handle blank value
        content_to_write <- ifelse(is.null(data), data.frame(), data)
        
        write.xlsx(content_to_write, file, row.names = FALSE)
      }
    )
  })
}