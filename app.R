library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)
library(shinycssloaders)

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
          
          fluidRow(
            
            column(
              width = 5,
              
              tags$figure(
                class = "centerFigure",
                tags$img(
                  src = "bank.jpg",
                  alt = "Bank building representing the Bank Marketing dataset.",
                  width = "350px",
                  height = "250px"
                ),
                tags$figcaption(
                  "Representative image of a bank used for the Bank Marketing dataset."
                )
              )
            ),
            
            column(
              width = 5,
              
              h4("How to Use This App"),
              
              tags$ul(
                tags$li("Sidebar: Select categorical and numeric filters, then click 'Apply Filters' to update all results."),
                tags$li("About: Learn about the application, dataset, and how to use the app."),
                tags$li("Summary: View summary statistics for the filtered data."),
                tags$li("Tables: View contingency tables for the filtered data."),
                tags$li("Plots: Explore interactive visualizations of the filtered data.")
              )
            )
            
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
            
            conditionalPanel(
              condition = "input.num_summary == 'Histogram'",
              
              selectInput(
                "hist_var",
                "Select Variable for Histogram",
                choices = c(
                  "age",
                  "balance",
                  "duration",
                  "campaign",
                  "pdays",
                  "previous"
                )
              ),
              
              withSpinner(plotOutput("plot3"))
            ),
            
            conditionalPanel(
              condition = "input.num_summary == 'Scatter Plot'",
              
              selectInput(
                "scatter_x",
                "Select X-axis Variable",
                choices = c(
                  "age",
                  "balance",
                  "duration",
                  "campaign",
                  "pdays",
                  "previous"
                ),
                selected = "age"
              ),
              
              selectInput(
                "scatter_y",
                "Select Y-axis Variable",
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
              
              withSpinner(plotOutput("plot4"))
              
            ),
            
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
              
              selectInput(
                "bar_var",
                "Select Variable for Bar Chart",
                choices = c(
                  "marital",
                  "housing",
                  "job",
                  "education",
                  "loan",
                  "contact"
                )
              ),
              
              withSpinner(plotOutput("plot1"))
            )
            
          ),
          
        ),
        
        tabPanel(
          
          "Data Download",
          
          p(
            "View and download the filtered dataset."
          ),
          
          downloadButton(
            "download_data",
            "Download Filtered Data"
          ),
          
          br(),
          br(),
          
          withSpinner(DT::dataTableOutput("bank_table"))
          
        ),
        
        tabPanel(
          "Preview",
          
          withSpinner(tableOutput("preview"))
          
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
    
    req(filtered_bank())
    
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
    
    req(filtered_bank())
    
    table(filtered_bank()$y)
    
  })
  
  output$bank_table <- DT::renderDataTable({
    
    req(filtered_bank())
    
    filtered_bank()
    
  })
  
  output$two_way <- renderTable({
    
    req(filtered_bank())
    
    table(
      filtered_bank()$marital,
      filtered_bank()$y
    )
    
  })
  
  output$plot1 <- renderPlot({
    
    req(filtered_bank())
    
    ggplot(
      filtered_bank(),
      aes(
        x = .data[[input$bar_var]],
        fill = y
      )
    ) +
      geom_bar(position = "dodge") +
      labs(
        title = paste("Subscription by", input$bar_var),
        x = input$bar_var,
        fill = "Subscribed"
      )
    
  })
  
  output$plot2 <- renderPlot({
    
    req(filtered_bank())
    
    ggplot(filtered_bank(),
           aes(x = housing, fill = y)) +
      geom_bar(position = "dodge") +
      labs(
        title = "Subscription by Housing Loan"
      )
    
  })
  
  output$plot3 <- renderPlot({
    
    req(filtered_bank())
    
    ggplot(
      filtered_bank(),
      aes(
        x = .data[[input$hist_var]],
        fill = y
      )
    ) +
      geom_histogram(
        binwidth = 5,
        color = "white"
      ) +
      labs(
        title = paste("Histogram of", input$hist_var),
        x = input$hist_var,
        y = "Count",
        fill = "Subscribed"
      )
    
  })
  
  output$plot4 <- renderPlot({
    
    req(filtered_bank())
    
    ggplot(
      filtered_bank(),
      aes(
        x = .data[[input$scatter_x]],
        y = .data[[input$scatter_y]],
        color = y
      )
    ) +
      geom_point(alpha = 0.4) +
      labs(
        title = paste(input$scatter_y, "vs.", input$scatter_x),
        x = input$scatter_x,
        y = input$scatter_y,
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
    
    req(filtered_bank())
    
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