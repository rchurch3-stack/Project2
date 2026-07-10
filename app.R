library(shiny)
library(readr)
library(dplyr)
library(ggplot2)

# ---------------------------------
# Load and prepare the data
# ---------------------------------

bank_train <- read_delim(
  "data/train.csv",
  delim = ";",
  show_col_types = FALSE
)

bank_test <- read_delim(
  "data/test.csv",
  delim = ";",
  show_col_types = FALSE
)

bank <- bind_rows(bank_train, bank_test)

bank <- bank |>
  mutate(
    across(
      c(
        job,
        marital,
        education,
        default,
        housing,
        loan,
        contact,
        month,
        poutcome,
        y
      ),
      as.factor
    )
  )

# -----------------------------
# User Interface
# -----------------------------

ui <- fluidPage(
  
  titlePanel("ST 558 Project 2"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Filters"),
      
      checkboxGroupInput(
        inputId = "marital",
        label = "Marital Status",
        choices = levels(bank$marital),
        selected = levels(bank$marital)
      ),
      
      checkboxGroupInput(
        inputId = "housing",
        label = "Housing Loan",
        choices = levels(bank$housing),
        selected = levels(bank$housing)
      ),
      
      # First numeric variable
      selectInput(
        "num_var1",
        "Numeric Variable 1",
        choices = c(
          "age",
          "balance",
          "duration",
          "campaign",
          "pdays",
          "previous"
        )
      ),
      
      uiOutput("slider1"),
      
      # Second numeric variable
      selectInput(
        "num_var2",
        "Numeric Variable 2",
        choices = c(
          "age",
          "balance",
          "duration",
          "campaign",
          "pdays",
          "previous"
        ),
        selected = "balance"
      ),
      
      uiOutput("slider2"),
      
      actionButton(
        inputId = "apply_filters",
        label = "Apply Filters"
      )
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          "About",
          
          h3("Bank Marketing Explorer"),
          
          p(
            "This application allows users to explore the Bank Marketing dataset by applying filters and viewing summaries of the data."
          ),
          
          h4("About the Data"),
          
          p(
            "The data comes from the UCI Machine Learning Repository and contains information collected from a Portuguese bank's direct marketing campaigns."
          ),
          
          tags$figure(
            class = "centerFigure",
            tags$img(
              src = "bank.jpg",
              alt = "Bank building representing the Bank Marketing dataset.",
              width = 500
            ),
            tags$figcaption(
              "Representative image of a bank used for the Bank Marketing dataset."
            )
          ),
          
          h4("How to Use This App"),
          
          
          br(),
          
        ),
        
        tabPanel(
          "Preview",
          
          tableOutput("preview")
          
        )
        
      )
      
    )
    
  )
  
)

# -----------------------------
# Server
# -----------------------------

server <- function(input, output) {
  
  output$slider1 <- renderUI({
    
    x <- bank[[input$num_var1]]
    
    sliderInput(
      inputId = "range1",
      label = paste("Range for", input$num_var1),
      min = min(x),
      max = max(x),
      value = c(min(x), max(x))
    )
    
  })
  
  output$slider2 <- renderUI({
    
    x <- bank[[input$num_var2]]
    
    sliderInput(
      inputId = "range2",
      label = paste("Range for", input$num_var2),
      min = min(x),
      max = max(x),
      value = c(min(x), max(x))
    )
    
  })
  
  filtered_bank <- eventReactive(
    input$apply_filters,
    {
      
      bank |>
        filter(
          marital %in% input$marital,
          housing %in% input$housing,
          .data[[input$num_var1]] >= input$range1[1],
          .data[[input$num_var1]] <= input$range1[2],
          .data[[input$num_var2]] >= input$range2[1],
          .data[[input$num_var2]] <= input$range2[2]
        )
      
    }
  )
  
  output$preview <- renderTable({
    
    head(filtered_bank())
    
  })
  
}

# -----------------------------
# Run the App
# -----------------------------

shinyApp(
  ui = ui,
  server = server
)