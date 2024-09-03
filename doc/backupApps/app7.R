library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(cowplot)
library(rsimsum)
library(knitr)
library(reshape2)
library(tidyr)

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
  useShinyjs(),  # Add this line to activate shinyjs
  titlePanel("Exploring the Simulation Results"),
  sidebarLayout(
    sidebarPanel(
      selectInput("metric_select", "Choose a performance metric:",
                  choices = metric_names
      ),
      sliderInput("num_indices", "Select number of simulations:",
                  min = 100, max = 500, value = 500, step = 50),
      checkboxInput("show_all_scenarios", "Show all scenarios side by side", value = FALSE),
      selectInput("scenario_select", "Choose a scenario:",
                  choices = list("Frequent Outcome and Exposure" = "scenario",
                                 "Rare Exposure" = "scenarioER",
                                 "Rare Outcome" = "scenarioOR")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Chosen metric", 
                 tags$p("Numerical values correspond to the selected scenario and the metric."),
                 uiOutput("numerical_values_output")
        ),
        # Add the "All Scenarios" tab in the UI
        tabPanel("All Scenarios",
                 tags$p("Comparison of the selected metric across all scenarios (irrespective of the chosen scenario)."),
                 checkboxInput("black_and_white", "Convert to black and white", value = FALSE),
                 downloadButton("download_metric_comp_png", "Download Metric PNG"),
                 downloadButton("download_metric_comp_pdf", "Download Metric PDF"),
                 downloadButton("download_metric_comp_jpg", "Download Metric JPG"),
                 plotOutput("metric_comparison_plot"),
                 downloadButton("download_combined_summary", "Download Combined Summary Data")
        )
      )
    )
  )
)



# Server logic
server <- function(input, output, session) {
  effective_num_indices <- reactiveVal(0)
  
  # Observe the checkbox and enable/disable the scenario selection accordingly
  observe({
    if (input$show_all_scenarios) {
      shinyjs::disable("scenario_select")
    } else {
      shinyjs::enable("scenario_select")
    }
  })
  
  load_scenario_data <- function(scenario, num_indices) {
    base_path <- file.path("results", scenario)
    files <- list.files(base_path, pattern = "\\.RData$", full.names = TRUE)
    
    est_list <- list()
    valid_indices_list <- list()
    
    for (file_path in files) {
      load(file_path)
      
      model <- tools::file_path_sans_ext(basename(file_path))
      
      object_name <- paste0("RD_", model)
      object_name_er <- paste0("RD_", model, "_ER")
      object_name_dot_er <- paste0("RD_", model, ".ER")
      object_name_or <- paste0("RD_", model, "_OR")
      object_name_dot_or <- paste0("RD_", model, ".OR")
      
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
        next
      }
      
      est_model <- get(object_name)
      valid_indices <- which(!is.na(est_model[, "RD"]))
      
      if (length(valid_indices) == 0) {
        warning(paste("Model", model, "contains no valid simulations in", scenario))
        next
      }
      
      valid_indices_list[[model]] <- valid_indices
      est_model$model <- model
      est_list[[model]] <- est_model
    }
    
    bross_indices <- valid_indices_list[["bross"]]
    
    if (!is.null(bross_indices)) {
      if (num_indices > length(bross_indices)) {
        showNotification(
          paste("Warning: Requested number of simulations (", num_indices, 
                ") exceeds available simulations (", length(bross_indices), "). Showing available data.", sep = ""),
          type = "warning",
          duration = 5
        )
        num_indices <- length(bross_indices)
      }
      
      bross_indices <- bross_indices[1:num_indices]
      
      est_list <- lapply(est_list, function(est_model) {
        est_model <- est_model[bross_indices, ]
        return(est_model)
      })
    }
    
    est <- do.call(rbind, lapply(names(est_list), function(model) {
      cbind(est_list[[model]], model = model)
    }))
    
    est <- as.data.frame(est)
    
    required_columns <- c("RD", "SE", "model")
    for (col in required_columns) {
      if (!col %in% names(est)) {
        est[[col]] <- NA
      }
    }
    
    est <- est[, required_columns]
    est$model <- model_names[as.character(est$model)]
    est$model <- factor(est$model, levels = model_names)
    est$Scenario <- scenario
    
    return(est)
  }
  
  
  # Reactive expression to load and process data based on the selected scenario and number of simulations
  data <- reactive({
    all_data <- all_scenarios_data()
    
    # Check if the data is loading
    if (is.null(all_data)) {
      return(NULL)
    }
    
    selected_scenario <- input$scenario_select
    scenario_data <- all_data[all_data$Scenario == selected_scenario, ]
    
    # Check if the scenario data is correctly filtered
    print(head(scenario_data))
    
    effective_num_indices(nrow(scenario_data))
    
    return(scenario_data)
  })
  
  
  # Reactive expression to load all scenarios data
  all_scenarios_data <- reactive({
    num_indices <- input$num_indices
    
    if (input$show_all_scenarios) {
      # Combine data from all scenarios if the checkbox is checked
      scenarios <- c("scenario", "scenarioER", "scenarioOR")
      combined_data <- do.call(rbind, lapply(scenarios, function(scenario) {
        load_scenario_data(scenario, num_indices)
      }))
    } else {
      # Load data only for the selected scenario
      selected_scenario <- input$scenario_select
      combined_data <- load_scenario_data(selected_scenario, num_indices)
    }
    
    if (is.null(combined_data)) {
      return(NULL)
    }
    
    # Perform the simsum analysis for each scenario or combined data
    scenario_summaries <- lapply(split(combined_data, combined_data$Scenario), function(est) {
      theta <- 0 # for RD or ATE; this would be 1 for OR
      s1 <- simsum(data = est, estvarname = "RD", true = theta, se = "SE", 
                   methodvar = "model", x = TRUE, ref = "Kitchen sink")
      summary(s1)
    })
    
    # Combine the summaries into a single data frame
    combined_summary <- do.call(rbind, lapply(names(scenario_summaries), function(scenario) {
      scenario_summary <- scenario_summaries[[scenario]]$summ
      scenario_summary$Scenario <- scenario
      scenario_summary
    }))
    
    return(combined_summary)
  })
  
  output$metric_comparison_plot <- renderPlot({
    combined_summary <- all_scenarios_data()
    
    if (is.null(combined_summary)) {
      showNotification("No data available for the selected metric across the scenarios.", type = "error")
      return(NULL)
    }
    
    # Map the selected metric to the internal name
    metric <- input$metric_select
    internal_metric <- names(metric_names)[metric_names == metric]
    
    # Filter by the selected metric
    filtered_data <- combined_summary[combined_summary$stat == internal_metric, ]
    
    if (nrow(filtered_data) == 0) {
      showNotification("No data available for the selected metric across the scenarios.", type = "error")
      return(NULL)
    } else {
      # Update scenario labels based on actual scenario values
      filtered_data$Scenario <- factor(filtered_data$Scenario, 
                                       levels = c("scenario", "scenarioER", "scenarioOR"),
                                       labels = c("Frequent Outcome and Exposure", 
                                                  "Rare Exposure", 
                                                  "Rare Outcome"))
      
      p <- ggplot(filtered_data, aes(x = model, y = est, color = Scenario, linetype = Scenario)) +
        geom_point(position = position_dodge(width = 0.3)) +
        geom_errorbar(aes(ymin = est - mcse, ymax = est + mcse), width = 0.2, 
                      position = position_dodge(width = 0.3)) +
        geom_line(aes(group = Scenario), 
                  position = position_dodge(width = 0.3)) +
        theme_minimal() +
        theme(legend.position = "bottom") +
        labs(title = paste("Comparison of", metric_names[internal_metric], "Across Scenarios."),
             x = "Model", y = metric_names[internal_metric]) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      if (input$black_and_white) {
        p <- p + 
          scale_linetype_manual(
            values = c("solid", "dashed", "dotted"),
            labels = c("Frequent Outcome and Exposure", "Rare Exposure", "Rare Outcome")
          ) +
          scale_color_manual(
            values = c("black", "black", "black"),
            labels = c("Frequent Outcome and Exposure", "Rare Exposure", "Rare Outcome")
          ) +
          guides(
            linetype = guide_legend(title = "Scenario", 
                                    override.aes = list(color = c("black", "black", "black"))),
            color = "none"
          )
      } else {
        p <- p + 
          scale_linetype_manual(
            values = c("solid", "solid", "solid"),
            labels = c("Frequent Outcome and Exposure", "Rare Exposure", "Rare Outcome")
          ) +
          scale_color_manual(
            values = c("red", "blue", "green"),
            labels = c("Frequent Outcome and Exposure", "Rare Exposure", "Rare Outcome")
          ) +
          guides(linetype = "none")
      }
      
      print(p)
    }
  })
  
  
  
  output$numerical_values_output <- renderUI({
    combined_summary <- all_scenarios_data()
    
    if (is.null(combined_summary)) {
      showNotification("No data available for the selected metric.", type = "error")
      return(NULL)
    }
    
    metric <- input$metric_select
    internal_metric <- names(metric_names)[metric_names == metric]
    
    metric_data <- combined_summary[combined_summary$stat == internal_metric, c("model", "est", "mcse", "Scenario")]
    
    metric_data$est <- sprintf("%.4f", metric_data$est)
    metric_data$mcse <- sprintf("%.4f", metric_data$mcse)
    metric_data$formatted <- paste0(metric_data$est, " (", metric_data$mcse, ")")
    
    if (input$show_all_scenarios) {
      metric_table <- metric_data %>%
        tidyr::pivot_wider(
          names_from = Scenario,
          values_from = formatted,
          id_cols = model,
          values_fill = list(formatted = "NA")
        ) %>%
        dplyr::rename(
          "Frequent" = scenario,
          "Rare Exposure" = scenarioER,
          "Rare Outcome" = scenarioOR
        )
    } else {
      metric_table <- data.frame(
        Model = metric_data$model,
        `Estimate.MCSE` = metric_data$formatted
      )
    }
    
    table_html <- knitr::kable(
      metric_table,
      format = "html",
      digits = 8,
      row.names = FALSE,
      caption = paste("Metric:", metric_names[internal_metric])
    ) %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
    
    div(HTML(table_html))
  })
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)
