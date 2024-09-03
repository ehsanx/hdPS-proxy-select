library(shiny)
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
      selectInput("plot_type", "Choose a plot type:",
                  choices = c("Lolly" = "lolly",
                              "Forest" = "forest",
                              "Zipper" = "zip",
                              "Heat" = "heat")),
      sliderInput("num_indices", "Select number of simulations:",
                  min = 100, max = 1000, value = 500, step = 50)
    ),
    mainPanel(
      # Display the number of simulations if the checkbox is checked
      conditionalPanel(
        condition = "input.show_num_indices == true",
        textOutput("num_indices_output")  # Text output for num_indices
      ),
      tabsetPanel(
        tabPanel("Plot", 
                 tags$p("The plot represents the selected scenario and the metric."),
                 downloadButton("download_png", "Download PNG"),
                 downloadButton("download_pdf", "Download PDF"),
                 downloadButton("download_jpg", "Download JPG"),
                 plotOutput("selected_plot")
        ),
        tabPanel("Chosen metric", 
                 tags$p("Numerical values correspond to the selected scenario and the metric."),
                 checkboxInput("show_all_scenarios", "Show all scenarios side by side", value = FALSE),  # New checkbox
                 uiOutput("numerical_values_output")
        ),
        tabPanel("All metrics", 
                 tags$p("Summary of all metrics across different models for a selected scenario (irrespective of the chosen metric)."),
                 uiOutput("summary_output"),
                 downloadButton("download_est_rds", "Download data object")
        ),
        tabPanel("SEs",
                 tags$p("Comparison of empirical and model-based standard errors across models for a selected scenario (irrespective of the chosen metric)."),
                 checkboxInput("gray_scale", "Convert to gray scale", value = FALSE),
                 downloadButton("download_secomp_png", "Download SE PNG"),
                 downloadButton("download_secomp_pdf", "Download SE PDF"),
                 downloadButton("download_secomp_jpg", "Download SE JPG"),
                 plotOutput("se_comparison_plot")
                 
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
  
  load_scenario_data <- function(scenario, num_indices) {
    base_path <- file.path("results", scenario)
    files <- list.files(base_path, pattern = "\\.RData$", full.names = TRUE)
    
    est_list <- list()
    valid_indices_list <- list()
    
    for (file_path in files) {
      load(file_path)
      
      model <- tools::file_path_sans_ext(basename(file_path))
      
      # Try different naming conventions for the object
      object_name <- paste0("RD_", model)
      object_name_er <- paste0("RD_", model, "_ER")
      object_name_dot_er <- paste0("RD_", model, ".ER")
      object_name_or <- paste0("RD_", model, "_OR")
      object_name_dot_or <- paste0("RD_", model, ".OR")
      
      # Check which object exists and assign it to the standard object name
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
    
    # Combine filtered data into one data frame
    est <- do.call(rbind, lapply(names(est_list), function(model) {
      cbind(est_list[[model]], model = model)
    }))
    
    est <- as.data.frame(est)
    
    # Ensure consistency in column names
    required_columns <- c("RD", "SE", "model")
    for (col in required_columns) {
      if (!col %in% names(est)) {
        est[[col]] <- NA
      }
    }
    
    est <- est[, required_columns]  # Reorder or subset columns to ensure consistency
    
    est$model <- model_names[as.character(est$model)]
    est$model <- factor(est$model, levels = model_names)
    
    # Set the effective number of simulations after processing all models
    effective_num_indices(length(bross_indices))
    
    # Print the final number of rows after all models are processed
    # print(paste("Final number of rows after filtering:", nrow(est)))
    print(paste("Final effective number of simulations set to:", effective_num_indices()))
    
    return(est)
  }
  
  
  
  
  # Reactive expression to load and process data based on the selected scenario and number of simulations
  data <- reactive({
    scenario <- input$scenario_select
    num_indices <- input$num_indices
    load_scenario_data(scenario, num_indices)
  })
  
  perform_simsum_analysis <- function(est) {
    theta <- 0  # for RD or ATE; this would be 1 for OR
    simsum(data = est, estvarname = "RD", true = theta, se = "SE", 
           methodvar = "model", x = TRUE, ref = "Kitchen sink")
  }
  
  
  # Reactive expression to perform simulation analysis
  # Reactive expression to perform simulation analysis for the selected scenario
  analysis <- reactive({
    est <- data()
    perform_simsum_analysis(est)
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
    
    # Get the selected plot type
    plot_type <- input$plot_type
    metric <- input$metric_select
    internal_metric <- names(metric_names)[metric_names == metric]  # Fetch the internal metric name
    
    # Check if internal_metric is valid
    if (is.na(internal_metric) || !internal_metric %in% names(metric_names)) {
      showNotification("Invalid metric selected, please choose a valid metric.", type = "error")
      return(NULL)
    }
    
    # Conditionally set the autoplot parameters based on the plot type
    if (plot_type == "zip") {
      p <- autoplot(s1, type = plot_type, stats = internal_metric) + 
        custom_theme
    } else if (plot_type == "forest") {
      p <- autoplot(summary(s1), type = plot_type, stats = internal_metric) + 
        custom_theme + 
        labs(x = metric_names[internal_metric])  # Use the correct human-readable name
    } else {
      p <- autoplot(s1, type = plot_type, stats = internal_metric) + 
        custom_theme + 
        labs(x = metric_names[internal_metric])  # Use the correct human-readable name
    }
    
    print(p)
  })
  
  
  
  
  output$numerical_values_output <- renderUI({
    combined_summary <- all_scenarios_data()
    
    if (is.null(combined_summary)) {
      showNotification("No data available for the selected metric.", type = "error")
      return(NULL)
    }
    
    metric <- input$metric_select
    internal_metric <- names(metric_names)[metric_names == metric]
    
    # Filter based on scenario if the checkbox is not checked
    if (!input$show_all_scenarios) {
      selected_scenario <- input$scenario_select
      combined_summary <- combined_summary[combined_summary$Scenario == selected_scenario, ]
    }
    
    metric_data <- combined_summary[combined_summary$stat == internal_metric, c("model", "est", "mcse", "Scenario")]
    
    # Increase the number of decimal places
    metric_data$est <- sprintf("%.4f", metric_data$est)
    metric_data$mcse <- sprintf("%.4f", metric_data$mcse)
    metric_data$formatted <- paste0(metric_data$est, " (", metric_data$mcse, ")")
    
    # Pivot wider only if showing all scenarios
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
          "Rare Outcome" = scenarioOR,
          "Model" = model
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
      digits = 8,  # Adjust digits to match desired precision
      row.names = FALSE,
      caption = paste("Metric:", metric_names[internal_metric])
    ) %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
    
    div(HTML(table_html))
  })
  
  

  
  
  
  
  
  output$download_png <- downloadHandler(
    filename = function() {
      paste("plot_", input$metric_select, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", width = 8, height = 6)
    }
  )
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste("plot_", input$metric_select, ".pdf", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "pdf", width = 8, height = 6)
    }
  )
  
  output$download_jpg <- downloadHandler(
    filename = function() {
      paste("plot_", input$metric_select, ".jpg", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "jpg", width = 8, height = 6)
    }
  )
  
  output$download_est_rds <- downloadHandler(
    filename = function() {
      paste("est_object_", input$scenario_select, ".rds", sep = "")
    },
    content = function(file) {
      est <- data()
      saveRDS(est, file)
    }
  )
  
  output$summary_output <- renderUI({
    s1 <- analysis()
    
    metrics <- c("bias", "empse", "mse", "modelse", "cover", "becover")
    
    extract_metric <- function(metric) {
      #print("Analysis Data in Plot Tab:")
      #print(s1$summ)
      metric_data <- s1$summ[s1$summ$stat == metric, c("model", "est", "mcse")]
      metric_data$est <- sprintf("%.4f", metric_data$est)
      metric_data$mcse <- sprintf("%.4f", metric_data$mcse)
      metric_data$formatted <- paste0(metric_data$est, " (", metric_data$mcse, ")")
      return(setNames(metric_data$formatted, metric_data$model))
    }
    
    performance_measures <- do.call(cbind, lapply(metrics, function(metric) {
      extract_metric(metric)
    }))
    
    performance_measures <- data.frame(
      Measure = c("Bias", "Empirical SE", "MSE", "Model-based SE", "Coverage", "Bias-eliminated Coverage"),
      t(performance_measures)
    )
    
    colnames(performance_measures) <- c("Measure", names(model_names))
    
    table_html <- knitr::kable(performance_measures, format = "html", digits = 4, row.names = FALSE) %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
    
    div(HTML(table_html))
  })
  
  output$se_comparison_plot <- renderPlot({
    s1 <- analysis()
    
    data <- s1$summ %>%
      filter(stat %in% c("empse", "modelse")) %>%
      select(model, stat, est) %>%
      pivot_wider(names_from = stat, values_from = est)
    
    melted_data <- data %>%
      pivot_longer(cols = -model, names_to = "variable", values_to = "value")
    
    p <- ggplot(melted_data, aes(x = model, y = value, fill = factor(variable, levels = c("empse", "modelse")))) +
      geom_bar(stat = "identity", position = position_dodge()) +
      coord_flip() + 
      labs(y = "SE", x = "Method", fill = "SE") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    if (input$gray_scale) {
      p <- p + scale_fill_manual(
        values = c("empse" = "lightgray", "modelse" = "darkgray"), 
        labels = c("empse" = "Empirical SE", "modelse" = "Model SE")
      )
    } else {
      p <- p + scale_fill_manual(
        values = c("empse" = "skyblue", "modelse" = "orange"), 
        labels = c("empse" = "Empirical SE", "modelse" = "Model SE")
      )
    }
    
    print(p)
  })
  
  output$download_secomp_png <- downloadHandler(
    filename = function() {
      paste("se_comparison_plot", ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", width = 8, height = 6)
    }
  )
  
  output$download_secomp_pdf <- downloadHandler(
    filename = function() {
      paste("se_comparison_plot", ".pdf", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "pdf", width = 8, height = 6)
    }
  )
  
  output$download_secomp_jpg <- downloadHandler(
    filename = function() {
      paste("se_comparison_plot", ".jpg", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "jpg", width = 8, height = 6)
    }
  )
  
  load_all_scenarios_data <- function(num_indices) {
    scenarios <- c("scenario", "scenarioER", "scenarioOR")
    all_data <- list()
    
    for (scenario in scenarios) {
      # Use the existing load_scenario_data function
      scenario_data <- load_scenario_data(scenario, num_indices)
      
      if (!is.null(scenario_data)) {
        scenario_data$Scenario <- scenario
        all_data[[scenario]] <- scenario_data
      }
    }
    
    combined_data <- do.call(rbind, all_data)
    
    return(combined_data)
  }
  
  
  all_scenarios_data <- reactive({
    num_indices <- input$num_indices
    all_data <- load_all_scenarios_data(num_indices)
    
    if (is.null(all_data)) {
      return(NULL)
    }
    
    # Perform the simsum analysis for each scenario
    scenario_summaries <- lapply(split(all_data, all_data$Scenario), function(est) {
      s1 <- perform_simsum_analysis(est)
      summary(s1)
    })
    
    # Combine the summaries into a single data frame
    combined_summary <- do.call(rbind, lapply(names(scenario_summaries), function(scenario) {
      scenario_summary <- scenario_summaries[[scenario]]$summ
      scenario_summary$Scenario <- scenario
      scenario_summary
    }))
    
    #print("Combined Summary in Chosen Metric Tab:")
    #print(combined_summary)
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
    combined_summary <- combined_summary[combined_summary$stat == internal_metric, ]
    
    if (nrow(combined_summary) == 0) {
      showNotification("No data available for the selected metric across the scenarios.", type = "error")
    } else {
      p <- ggplot(combined_summary, aes(x = model, y = est, color = Scenario, linetype = Scenario)) +
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
  
  
  output$download_metric_comp_png <- downloadHandler(
    filename = function() {
      paste("metric_comparison_", input$metric_select, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", width = 8, height = 6)
    }
  )
  
  output$download_metric_comp_pdf <- downloadHandler(
    filename = function() {
      paste("metric_comparison_", input$metric_select, ".pdf", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "pdf", width = 8, height = 6)
    }
  )
  
  output$download_metric_comp_jpg <- downloadHandler(
    filename = function() {
      paste("metric_comparison_", input$metric_select, ".jpg", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "jpg", width = 8, height = 6)
    }
  )
  
  output$download_combined_summary <- downloadHandler(
    filename = function() {
      paste("combined_summary_", input$metric_select, ".rds", sep = "")
    },
    content = function(file) {
      combined_summary <- all_scenarios_data()
      
      if (is.null(combined_summary)) {
        showNotification("No data available for the selected metric across the scenarios.", type = "error")
        return(NULL)
      }
      
      metric <- input$metric_select
      internal_metric <- names(metric_names)[metric_names == metric]
      
      combined_summary <- combined_summary[combined_summary$stat == internal_metric, ]
      
      saveRDS(combined_summary, file)
    }
  )
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
