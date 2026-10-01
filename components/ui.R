ui <- fluidPage(
  titlePanel("NLMSF Treasury Automation"),
  
  sidebarLayout(
    sidebarPanel(
      tags$a(
        href = form_auth_url(client_id, redirect_uri),
        "Authenticate with Intuit",
        target = "_self"
      ),
      verbatimTextOutput("token_output")
    ),
    
    mainPanel(
      tabsetPanel(
        makePaymentTab(title = "Stripe", id="stripe"),
        makePaymentTab(title = "Fundraise Up", id="fundraise_up")
        
        # tabPanel("Check Expense",
        #          actionButton("check_entry", "Enter Check Expense")
        # ),
        # 
        # tabPanel("Debit Expense",
        #          actionButton("debit_entry", "Enter Debit Expense")
        # ),
        # 
        # tabPanel("EFT Expense",
        #          actionButton("eft_entry", "Enter EFT Expense")
        # ),
        # 
        # tabPanel("Zelle Expense",
        #          actionButton("zelle_entry", "Enter Zelle Expense")
        # ),
        
        # Optional future tab:
        # tabPanel("Check Payment",
        #   actionButton("check_payment", "Enter Check Payment")
        # )
        
      )
    )
  )
)