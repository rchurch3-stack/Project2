library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)

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
          
          h4("Data Source"),
          
          p(
            "This application allows users to explore the Bank Marketing dataset through interactive filters, summary statistics, tables, and visualizations."
          ),
          
          p(
            "More information about the dataset is available here:"
          ),
          
          tags$a(
            href = "https://www.kaggle.com/datasets/prakharrathi25/banking-dataset-marketing-targets",
            "Kaggle: Banking Dataset - Marketing Targets",
            target = "_blank"
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
          
          tags$ul(
            tags$li("Use the sidebar to select categorical and numeric filters, then click 'Apply Filters' to update the results."),
            tags$li("The About tab provides an overview of the application and the dataset."),
            tags$li("The Summary tab displays summary statistics for the filtered data."),
            tags$li("The Tables tab displays contingency tables for the filtered data."),
            tags$li("The Plots tab displays visualizations based on the filtered data.")
          ),
          
          
          br(),
          
        ),
        
        tabPanel(
          
          "Data Exploration",
          
          radioButtons(
            inputId = "summary_type",
            label = "Select Summary Type",
            choices = c("Categorical", "Numeric"),
            selected = "Categorical",
            inline = TRUE
          ),
          
          conditionalPanel(
            condition = "input.summary_type == 'Numeric'",
            
            selectInput(
              "num_summary",
              "Numeric Summary",
              choices = c(
                "Summary Statistics",
                "Histogram",
                "Scatter Plot"
              )
            ),
            
            conditionalPanel(
              condition = "input.num_summary == 'Summary Statistics'",
              tableOutput("summary_stats")
            ),
            
            selectInput(
              "hist_var",
              "Histogram Variable",
              choices = c(
                "age",
                "balance",
                "duration",
                "campaign",
                "pdays",
                "previous"
              )
            ),
            
            conditionalPanel(
              condition = "input.num_summary == 'Histogram'",
              plotOutput("plot3")
            ),
            
            conditionalPanel(
              condition = "input.num_summary == 'Scatter Plot'",
              plotOutput("plot4")
            )
            
          ),
          
          conditionalPanel(
            condition = "input.summary_type == 'Categorical'",
            
            selectInput(
              "cat_summary",
              "Categorical Summary",
              choices = c(
                "One-way Table",
                "Two-way Table",
                "Bar Chart"
              )
            ),
            
            conditionalPanel(
              condition = "input.cat_summary == 'One-way Table'",
              tableOutput("one_way")
            ),
            
            conditionalPanel(
              condition = "input.cat_summary == 'Two-way Table'",
              tableOutput("two_way")
            ),
            
            conditionalPanel(
              condition = "input.cat_summary == 'Bar Chart'",
              plotOutput("plot1")
            )
            
          ),
          
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
  
  output$summary_stats <- renderTable({
    
    filtered_bank() |>
      group_by(y) |>
      summarize(
        mean_age = mean(age),
        median_age = median(age),
        sd_age = sd(age),
        mean_balance = mean(balance),
        median_balance = median(balance),
        sd_balance = sd(balance),
        mean_duration = mean(duration),
        .groups = "drop"
      )
    
  })
  
  output$one_way <- renderTable({
    
    table(filtered_bank()$y)
    
  })
  
  output$bank_table <- DT::renderDataTable({
    
    filtered_bank()
    
  })
  
  output$two_way <- renderTable({
    
    table(
      filtered_bank()$marital,
      filtered_bank()$y
    )
    
  })
  
  output$plot1 <- renderPlot({
    
    ggplot(filtered_bank(), aes(x = y)) +
      geom_bar() +
      labs(
        title = "Term Deposit Subscription Counts",
        x = "Subscribed",
        y = "Count"
      )
    
  })
  
  output$plot2 <- renderPlot({
    
    ggplot(filtered_bank(),
           aes(x = housing, fill = y)) +
      geom_bar(position = "dodge") +
      labs(
        title = "Subscription by Housing Loan"
      )
    
  })
  
  output$plot3 <- renderPlot({
    
    ggplot(
      filtered_bank(),
      aes(x = .data[[input$hist_var]], fill = y)
    ) +
      geom_histogram(
        binwidth = 5,
        color = "white"
      ) +
      labs(
        title = paste("Histogram of", input$hist_var),
        x = input$hist_var,
        fill = "Subscribed"
      )
    
  })
  
  output$plot4 <- renderPlot({
    
    ggplot(
      filtered_bank(),
      aes(
        x = age,
        y = balance,
        color = y
      )
    ) +
      geom_point(alpha = 0.4) +
      labs(
        title = "Age vs. Account Balance by Subscription",
        x = "Age",
        y = "Account Balance",
        color = "Subscribed"
      )
    
  })
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      "filtered_bank_data.csv"
    },
    
    content = function(file) {
      
      write.csv(
        filtered_bank(),
        file,
        row.names = FALSE
      )
      
    }
    
  )
  
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