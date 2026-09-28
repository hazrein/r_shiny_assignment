library(shiny)
library(readxl)

ui <- fluidPage(
  
  titlePanel("Claims Paid"),
  
  fluidRow(
    style = "display: flex; align-items: stretch;",
    
    # LEFT SIDE - User Inputs
    column(
      width = 4,
      
      h3("Upload Claims Data"),
      
      p("If you have a claims data file, upload it here."),
      
      p(
        style = "text-align: justify;",
        tags$b("Note:"),
        " Please upload incremental claims data with loss years arranged in rows and development years arranged in columns. The application will automatically calculate the cumulative claims."
      ),
      
      fileInput(
        "claims_file",
        "Select a CSV or Excel file:",
        accept = c(".csv", ".xlsx", ".xls")
      ),
      
      hr(),
      
      h3("Enter Claims Manually"),
      
      helpText(
        "Enter the claims paid amounts below if no file is uploaded."
      ),
      
      numericInput(
        "claim_2017_1",
        "2017 - Development Year 1:",
        value = 524792
      ),
      
      numericInput(
        "claim_2017_2",
        "2017 - Development Year 2:",
        value = 218265
      ),
      
      numericInput(
        "claim_2017_3",
        "2017 - Development Year 3:",
        value = 2225
      ),
      
      numericInput(
        "claim_2018_1",
        "2018 - Development Year 1:",
        value = 798502
      ),
      
      numericInput(
        "claim_2018_2",
        "2018 - Development Year 2:",
        value = 197157
      ),
      
      numericInput(
        "claim_2019_1",
        "2019 - Development Year 1:",
        value = 917636
      ),
      
      h3("Parameter"),
      
      numericInput(
        "tail_factor",
        "Tail Factor:",
        value = 1.10,
        step = 0.01
      )
    ),
    
    # RIGHT SIDE - Results
    column(
      width = 8,
      style = "border-left: 1px solid #cccccc; padding-left: 30px;",
      
      h3("Cumulative Paid Claims ($)"),
      
      tableOutput("claims_table"),
      
      br(),
      
      plotOutput(
        "claims_plot",
        height = "500px"
      )
    )
  )
)

server <- function(input, output) {
  
  claims_input <- reactive({
    
    # If no file is uploaded, use the manual inputs
    if (is.null(input$claims_file)) {
      
      return(data.frame(
        LossYear = c(2017, 2018, 2019),
        Dev1 = c(
          input$claim_2017_1,
          input$claim_2018_1,
          input$claim_2019_1
        ),
        Dev2 = c(
          input$claim_2017_2,
          input$claim_2018_2,
          NA
        ),
        Dev3 = c(
          input$claim_2017_3,
          NA,
          NA
        )
      ))
    }
    
    # Check the type of uploaded file
    file_extension <- tools::file_ext(input$claims_file$name)
    
    # Read CSV file
    if (tolower(file_extension) == "csv") {
      
      claims <- read.csv(
        input$claims_file$datapath,
        check.names = FALSE
      )
      
      # Read Excel file
    } else if (tolower(file_extension) %in% c("xlsx", "xls")) {
      
      claims <- readxl::read_excel(
        input$claims_file$datapath
      )
      
    } else {
      stop("Please upload a CSV or Excel file.")
    }
    
    return(claims)
  })
  
  cumulative_claims <- reactive({
    
    # Get claims from uploaded file OR manual inputs
    claims <- claims_input()
    
    # 2017 cumulative claims
    dev1_2017 <- as.numeric(claims[1, 2])
    dev2_2017 <- dev1_2017 + as.numeric(claims[1, 3])
    dev3_2017 <- dev2_2017 + as.numeric(claims[1, 4])
    
    # 2018 cumulative claims
    dev1_2018 <- as.numeric(claims[2, 2])
    dev2_2018 <- dev1_2018 + as.numeric(claims[2, 3])
    
    # 2019 cumulative claims
    dev1_2019 <- as.numeric(claims[3, 2])
    
    # Calculate development factors
    factor_1_to_2 <- (dev2_2017 + dev2_2018) /
      (dev1_2017 + dev1_2018)
    
    factor_2_to_3 <- dev3_2017 / dev2_2017
    
    # Project missing cumulative claims
    dev2_2019 <- dev1_2019 * factor_1_to_2
    
    dev3_2018 <- dev2_2018 * factor_2_to_3
    dev3_2019 <- dev2_2019 * factor_2_to_3
    
    # Apply tail factor for Development Year 4
    dev4_2017 <- dev3_2017 * input$tail_factor
    dev4_2018 <- dev3_2018 * input$tail_factor
    dev4_2019 <- dev3_2019 * input$tail_factor
    
    data.frame(
      `Loss Year` = c("2017", "2018", "2019"),
      `Dev 1` = c(dev1_2017, dev1_2018, dev1_2019),
      `Dev 2` = c(dev2_2017, dev2_2018, dev2_2019),
      `Dev 3` = c(dev3_2017, dev3_2018, dev3_2019),
      `Dev 4` = c(dev4_2017, dev4_2018, dev4_2019),
      check.names = FALSE
    )
  })
  
  # Display cumulative claims table without decimal places
  output$claims_table <- renderTable({
    cumulative_claims()
  }, digits = 0)
  
  output$claims_plot <- renderPlot({
    
    claims <- cumulative_claims()
    
    # Draw the line graph
    matplot(
      x = 1:4,
      y = t(as.matrix(claims[, 2:5])),
      type = "o",
      pch = 16,
      lty = 1,
      xaxt = "n",
      xlim = c(0.9, 4.5),
      ylim = c(
        min(as.matrix(claims[, 2:5])) * 0.95,
        max(as.matrix(claims[, 2:5])) * 1.15
      ),
      xlab = "Development Year",
      ylab = "Cumulative Paid Claims ($)",
      main = "Cumulative Paid Claims ($)"
    )
    
    axis(1, at = 1:4, labels = 1:4)
    
    # Labels for 2019 projected values
    text(
      x = 2:4,
      y = as.numeric(claims[3, 3:5]),
      labels = format(
        round(as.numeric(claims[3, 3:5])),
        big.mark = ","
      ),
      pos = 3
    )
    
    # Labels for 2018 projected values
    text(
      x = 3:4,
      y = as.numeric(claims[2, 4:5]),
      labels = format(
        round(as.numeric(claims[2, 4:5])),
        big.mark = ","
      ),
      pos = 3
    )
    
    # Label for 2017 projected value
    text(
      x = 4,
      y = claims[1, 5],
      labels = format(
        round(as.numeric(claims[1, 5])),
        big.mark = ","
      ),
      pos = 3
    )
    
    # Legend
    legend(
      "bottomright",
      legend = claims$`Loss Year`,
      col = 1:3,
      lty = 1,
      pch = 16
    )
  })
}

shinyApp(ui = ui, server = server)