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
        tabPanel("Stripe",
                 # Allow user to choose when to look for payments
                 dateRangeInput(
                   inputId = "stripe_date",
                   label = "Bank Statement Dates:",
                   start = first_day_prev_month(),
                   end = last_day_prev_month()
                 ),
                 # Display payouts that will be entered
                 div(style = "width: 700px;",
                     h3("Stripe Payouts"),
                     DTOutput("stripe_payouts_table")
                 ),
                 # Allow entering payments on demand
                 actionButton("stripe", "Enter Stripe Payments"),
                 withSpinner(verbatimTextOutput("stripe_result")),
                 # Download entered payments
                 downloadButton(
                   outputId = "download_stripe",
                   label = "Download Entered Stripe Payments"
                 )
        )
        
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