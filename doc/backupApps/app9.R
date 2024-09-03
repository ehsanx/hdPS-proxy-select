library(shiny)
library(ggplot2)
library(dplyr)
library(rsimsum)

# Define the models you want to work with
model_names <- c(
  kitchen = "Kitchen sink",
  bross = "Bross-based hdPS",
  hybrid = "Hybrid hdPS",
  ga = "Genetic Algorithm",
  xgboost = "XGBoost",
  rf = "Random Forest",
  forward = "Forward Selection",
  backward = "Backward Elimination",
  lasso = "LASSO",  
  enet = "Elasticnet"
)

# Define metric names
metric_names <- c(
  bias = "Bias",
  cover = "Coverage",
  becover = "Bias-eliminated Coverage",
  relerror = "Relative Error",
  empse = "Empirical SE",
  modelse = "Model SE",
  mse = "MSE",
  relprec = "Relative Precision",
  power = "Power",
  nsim = "# of simulations"
)

# Define UI
ui <- fluidPage(
  titlePanel("Exploring the Simulation Results"),
  sidebarLayout(
    sidebarPanel(
      selectInput("scenario_select", "Choose a scenario:",
                  choices = list("Frequent Outcome and Exposure" = "scenario",
                                 "Rare Exposure" = "scenarioER",
                                 "Rare Outcome" = "scenarioOR")
      ),
      selectInput("metric_select", "Choose a performance metric:",
                  choices = metric_names
      ),
      sliderInput("num_indices", "Select number of simulations:",
                  min = 100, max = 1000, value = 500, step = 50)
    ),
    mainPanel(
      plotOutput("selected_plot")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  load_all_scenarios_data <- function() {
    base_path <- "C:/Users/Ehsan/Documents/GitHub/hdPS-proxy-select/doc/results"
    combined_list <- readRDS(file.path(base_path, "combined_list_filtered.rds"))
    return(combined_list)
  }
  
  load_scenario_data <- function(combined_list, scenario, num_indices) {
    selected_scenario <- combined_list[[scenario]]
    
    # Subset the data to the requested number of simulations
    if (num_indices > nrow(selected_scenario)) {
      showNotification(
        paste("Warning: Requested number of simulations (", num_indices, 
              ") exceeds available simulations (", nrow(selected_scenario), "). Showing available data.", sep = ""),
        type = "warning",
        duration = 5
      )
      num_indices <- nrow(selected_scenario)
    }
    
    selected_scenario <- selected_scenario[1:num_indices, ]
    
    # Ensure the 'model' column exists and is consistent
    if (!"model" %in% colnames(selected_scenario)) {
      selected_scenario$model <- NA
    }
    
    selected_scenario$model <- factor(selected_scenario$model, levels = names(model_names))
    
    return(selected_scenario)
  }
  
  # Load all scenarios data once
  all_data <- load_all_scenarios_data()
  
  # Reactive expression to load and process data based on the selected scenario and number of simulations
  data <- reactive({
    scenario <- input$scenario_select
    num_indices <- input$num_indices
    load_scenario_data(all_data, scenario, num_indices)
  })
  
  # Plotting functions using the reactive data
  output$selected_plot <- renderPlot({
    est <- data()
    metric <- input$metric_select
    internal_metric <- names(metric_names)[metric_names == metric]  # Fetch the internal metric name
    
    if (is.null(est) || is.na(internal_metric)) {
      showNotification("Invalid data or metric selection.", type = "error")
      return(NULL)
    }
    
    theta <- 0 # for RD or ATE; this would be 1 for OR
    s1 <- simsum(data = est, estvarname = "RD", true = theta, se = "SE", 
                 methodvar = "model", x = TRUE)
    
    p <- autoplot(s1, type = "forest", stats = internal_metric) + 
      theme_minimal() +
      labs(title = paste(metric_names[internal_metric], "for", input$scenario_select),
           x = metric_names[internal_metric])
    
    print(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
