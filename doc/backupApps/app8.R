library(shiny)
library(ggplot2)
library(dplyr)
library(rsimsum)

# Load the combined list of scenarios
base_path <- "C:/Users/Ehsan/Documents/GitHub/hdPS-proxy-select/doc/results"
combined_list <- readRDS(file.path(base_path, "combined_list_filtered.rds"))

# Define UI
library(shiny)

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

ui <- fluidPage(
  titlePanel("Simulation Analysis by Performance Measure"),
  sidebarLayout(
    sidebarPanel(
      selectInput("scenario", "Choose a Scenario:", choices = c("scenario", "scenarioER", "scenarioOR")),
      selectInput("metric_select", "Choose a Performance Measure:", choices = metric_names),
      selectInput("plot_type", "Choose a plot type:",
                  choices = c("Lolly" = "lolly",
                              "Forest" = "forest",
                              "Zipper" = "zip",
                              "Heat" = "heat")),
      checkboxInput("show_all_measures", "Show All Measures", value = FALSE),
      actionButton("runAnalysis", "Run Analysis")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary Table", tableOutput("summaryTable")),
        tabPanel("Plots", plotOutput("selected_plot")),
        tabPanel("Numerical Values", uiOutput("numerical_values_output")),
        tabPanel("Simulation Info", textOutput("num_indices_output"))
      )
    )
  )
)


server <- function(input, output) {
  base_path <- "C:/Users/Ehsan/Documents/GitHub/hdPS-proxy-select/doc/results"
  combined_list <- readRDS(file.path(base_path, "combined_list_filtered.rds"))
  
  analysis <- eventReactive(input$runAnalysis, {
    selected_scenario <- combined_list[[input$scenario]]
    print("Selected Scenario Loaded")  # Debugging line
    print(head(selected_scenario$summ))  # Debugging line to check data
    return(selected_scenario)
  })
  
  effective_num_indices <- reactive({
    selected_scenario <- analysis()
    return(nrow(selected_scenario$summ))  # Debugging line: nrow instead of nrow(selected_scenario)
  })
  
  # Plotting functions using the reactive data
  output$selected_plot <- renderPlot({
    s1 <- analysis()
    if (is.null(s1)) {
      showNotification("Analysis data is missing.", type = "error")
      return(NULL)
    }
    
    custom_theme <- theme_minimal() + 
      theme(
        panel.background = element_rect(fill = "white"),  
        panel.grid.major = element_blank(),              
        panel.grid.minor = element_blank(),              
        legend.position = "bottom"                      
      )
    
    plot_type <- input$plot_type
    print(paste("Selected Plot Type:", plot_type))  # Debugging line
    
    metric <- input$metric_select
    print(paste("Selected Metric:", metric))  # Debugging line
    
    internal_metric <- names(metric_names)[metric_names == metric]  # Fetch the internal metric name
    print(paste("Internal Metric Name:", internal_metric))  # Debugging line
    
    if (is.na(internal_metric) || !internal_metric %in% names(metric_names)) {
      showNotification("Invalid metric selected, please choose a valid metric.", type = "error")
      return(NULL)
    }
    
    if (plot_type == "zip") {
      p <- autoplot(s1, type = plot_type, stats = internal_metric) + 
        custom_theme
    } else if (plot_type == "forest") {
      p <- autoplot(summary(s1), type = plot_type, stats = internal_metric) + 
        custom_theme + 
        labs(x = metric_names[internal_metric])  # Use the correct human-readable name
    } else if (plot_type == "heat") {
      p <- ggplot(s1$summ, aes(x = model, y = stat, fill = est)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "blue") +
        custom_theme +
        labs(title = paste("Heatmap of", metric_names[internal_metric]), x = "Model", y = metric_names[internal_metric])
    } else {
      p <- autoplot(s1, type = plot_type, stats = internal_metric) + 
        custom_theme + 
        labs(x = metric_names[internal_metric])  # Use the correct human-readable name
    }
    
    print(p)  # Debugging line to check if the plot object is created
  })
  
  output$numerical_values_output <- renderUI({
    s1 <- analysis()
    if (is.null(s1)) {
      showNotification("Analysis data is missing.", type = "error")
      return(NULL)
    }
    
    metric <- input$metric_select
    internal_metric <- names(metric_names)[metric_names == metric]
    print(paste("Internal Metric for Table:", internal_metric))  # Debugging line
    
    metric_data <- s1$summ[s1$summ$stat == internal_metric, c("model", "est", "mcse")]
    print(head(metric_data))  # Debugging line to check the filtered data
    
    if (nrow(metric_data) == 0) {
      showNotification("No data available for the selected metric.", type = "warning")
      return(NULL)
    }
    
    metric_data$est <- sprintf("%.4f", metric_data$est)
    metric_data$mcse <- sprintf("%.4f", metric_data$mcse)
    metric_data$formatted <- paste0(metric_data$est, " (", metric_data$mcse, ")")
    
    chosen_metric_table <- data.frame(
      Model = model_names[metric_data$model],
      `Estimate.MCSE` = metric_data$formatted
    )
    
    table_html <- knitr::kable(
      chosen_metric_table, 
      format = "html", 
      digits = 4, 
      row.names = FALSE,
      caption = paste("Metric:", metric_names[internal_metric])
    ) %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
    
    div(HTML(table_html))
  })
  
  output$num_indices_output <- renderText({
    num_sims <- effective_num_indices()
    if (!is.null(num_sims)) {
      paste("The results from", num_sims, "simulations.")
    } else {
      "The results from simulations."
    }
  })
}

shinyApp(ui = ui, server = server)
