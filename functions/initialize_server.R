# Initialize reactive values
make_value <- function(initial = NULL) {

  # Running inside Shiny
  if (shiny::isRunning()) {
    return(shiny::reactiveVal(initial))
  }

  # Running outside Shiny
  return(initial)
}

# Retrieve reactive values
get_value <- function(x) {
  if (shiny::is.reactive(x) || shiny::is.reactivevalues(x)) {
    x()
  } else {
    x
  }
}

# Initialize reactive values
if (!shiny::isRunning()){
  access_token <- make_value(NULL)
  realmID <- make_value(NULL)
  accts <- make_value(NULL)
  vendors <- make_value(NULL)
  payment_methods <- make_value(NULL)
  stripe_payouts <- make_value(NULL)
  customers <- make_value(NULL)
  items <- make_value(NULL)
  entered_stripe_payments <- make_value(NULL)
} else{
  access_token <- reactiveVal(NULL)
  realmID <- reactiveVal(NULL)
  accts <- reactiveVal(NULL)
  vendors <- reactiveVal(NULL)
  payment_methods <- reactiveVal(NULL)
  stripe_payouts <- reactiveVal(NULL)
  customers <- reactiveVal(NULL)
  items <- reactiveVal(NULL)
  entered_stripe_payments <- reactiveVal(NULL)
}
