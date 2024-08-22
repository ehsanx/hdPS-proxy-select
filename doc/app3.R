setwd("/Users/jidianlewei/Desktop/WL_MachineLearning/hdPS/hdPS_ProxySelect/doc")
library(shiny)
library(ggplot2)
library(rsimsum)
library(knitr)

# Define UI
ui <- fluidPage(
  titlePanel("Select a Scenario and Performance Metric"),
  sidebarLayout(
    sidebarPanel(
      selectInput("scenario_select", "Choose a scenario:",
                  choices = list("Frequent Outcome and Exposure" = "scenario",
                                 "Rare Exposure" = "scenarioER",
                                 "Rare Outcome" = "scenarioOR")
      ),
      selectInput("metric_select", "Choose a performance metric:",
                  choices = list("Bias" = "bias", 
                                 "Coverage" = "cover", 
                                 "Empirical SE" = "empse", 
                                 "Model SE" = "modelse", 
                                 "MSE" = "mse",
                                 "Relative Precision" = "relprec",
                                 "Relative Error" = "relerror",
                                 "Power" = "power",
                                 "Bias-eliminated Coverage" = "becover")
      )
    ),
    mainPanel(
      plotOutput("selected_plot"),
      verbatimTextOutput("numerical_values_output") 
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Function to load and process the data
  load_scenario_data <- function(scenario) {
    base_path <- file.path("results", scenario)
    
    # List all .RData files in the directory
    files <- list.files(base_path, pattern = "\\.RData$", full.names = TRUE)
    
    # Initialize an empty list to hold the loaded data
    est_list <- list()
    
    # Loop over each file and load the corresponding data
    for (file_path in files) {
      load(file_path)
      
      # Extract the model name from the file name
      model <- tools::file_path_sans_ext(basename(file_path))
      
      # Try different naming conventions
      object_name <- paste0("RD_", model)
      object_name_er <- paste0("RD_", model, "_ER")
      object_name_dot_er <- paste0("RD_", model, ".ER")
      object_name_or <- paste0("RD_", model, "_OR")
      object_name_dot_or <- paste0("RD_", model, ".OR")
      
      # Check which object exists and rename it to a standard name
      if (exists(object_name_er)) {
        assign(object_name, get(object_name_er))
      } else if (exists(object_name_dot_er)) {
        assign(object_name, get(object_name_dot_er))
      } else if (exists(object_name_or)) {
        assign(object_name, get(object_name_or))
      } else if (exists(object_name_dot_or)) {
        assign(object_name, get(object_name_dot_or))
      } else if (exists(object_name)) {
        assign(object_name, get(object_name))
      } else {
        warning(paste("Object", object_name, "not found in", file_path))
      }
      
      # Add the object to the list if it exists
      if (exists(object_name)) {
        est_list[[model]] <- get(object_name)
      }
    }
    
    # Combine data into one data frame
    est <- do.call(rbind, lapply(names(est_list), function(model) {
      cbind(est_list[[model]], model = model)
    }))
    
    # Ensure that `est` is a data frame and contains the necessary columns
    est <- as.data.frame(est)
    
    # Rename columns if necessary
    if (!"RD" %in% names(est)) {
      names(est)[names(est) == "Estimate"] <- "RD"
    }
    if (!"SE" %in% names(est)) {
      names(est)[names(est) == "StandardError"] <- "SE"
    }
    
    if (!all(c("RD", "SE") %in% names(est))) {
      stop("The data frame does not contain the required columns 'RD' and 'SE'.")
    }
    
    est$model <- as.factor(est$model)
    return(est)
  }
  
  # Reactive expression to load and process data based on the selected scenario
  data <- reactive({
    scenario <- input$scenario_select
    load_scenario_data(scenario)
  })
  
  # Reactive expression to perform simulation analysis
  analysis <- reactive({
    est <- data()
    
    if (!is.data.frame(est)) {
      stop("Data is not a data frame.")
    }
    
    if (!all(c("RD", "SE", "model") %in% names(est))) {
      stop("Columns 'RD', 'SE', or 'model' are missing.")
    }
    
    theta <- 0
    
    # Perform the simsum analysis
    simsum(data = est, estvarname = "RD", true = theta, se = "SE", 
           methodvar = "model", x = TRUE)
  })
  
  # Plotting functions using the reactive data
  output$selected_plot <- renderPlot({
    s1 <- analysis()
    
    custom_theme <- theme_minimal() + 
      theme(
        panel.background = element_rect(fill = "white"),  
        panel.grid.major = element_blank(),              
        panel.grid.minor = element_blank(),              
        legend.position = "bottom"                      
      )
    
    metric <- input$metric_select
    
    # Plot using autoplot with the selected metric
    autoplot(s1, type = "lolly", stats = metric) + 
      custom_theme + 
      labs(x = metric)
  })
  
  # Display the numerical values used in the plot
  output$numerical_values_output <- renderPrint({
    s1 <- analysis()
    
    # Extract the relevant metric data from the summary object
    metric_data <- s1$summ[s1$summ$stat == input$metric_select, ]
    
    # Print the metric data without row numbers using knitr::kable()
    knitr::kable(metric_data, row.names = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
