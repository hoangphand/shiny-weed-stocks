ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Weed stocks"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    checkboxGroupInput("symbols", "Choose stocks:", 
                       choiceNames = symbol_data$symbol, 
                       choiceValues = symbol_data$id
    ),
    selectInput("duration", "Choose duration:",
                c("Last 1 week" = "7",
                  "Last 2 week" = "14",
                  "Last 3 week" = "21",
                  "Last 1 month" = "30",
                  "Last 2 month" = "60",
                  "Last 3 month" = "90"))
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    plotOutput("corr"),
    verbatimTextOutput("value")
  )
)