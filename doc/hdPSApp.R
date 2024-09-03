library(shiny)
library(ggplot2)
library(dplyr)
library(cowplot)
library(rsimsum)
library(knitr)
library(reshape2)
library(tidyr)

# Create a mapping from scenario codes to descriptive names
scenario_names <- c(
  scenario = "Frequent Outcome and Exposure",
  scenarioER = "Rare Exposure",
  scenarioOR = "Rare Outcome"
)

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

# Define a custom theme for ggplot2
custom_theme <- theme_minimal() + 
  theme(
    panel.background = element_rect(fill = "white"),  
    panel.grid.major = element_blank(),              
    panel.grid.minor = element_blank(),              
    legend.position = "bottom"
  )

# Helper function to map metric names
get_internal_metric <- function(metric) {
  names(metric_names)[metric_names == metric]
}

# Helper function for download handlers
create_download_handler <- function(extension) {
  downloadHandler(
    filename = function() {
      paste("plot_", input$metric_select, ".", extension, sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = extension, width = 8, height = 6)
    }
  )
}

# Define UI
ui <- fluidPage(
  titlePanel("Plasmode Simulation Results Comparing hdPS Alternatives"),
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
      conditionalPanel(
        condition = "input.show_num_indices == true",
        textOutput("num_indices_output")
      ),
      tabsetPanel(
        tabPanel("Plot", 
                 tags$p("The plot represents the selected scenario and the metric."),
                 downloadButton("download_png", "Download PNG"),
                 downloadButton("download_pdf", "Download PDF"),
                 downloadButton("download_jpg", "Download JPG"),
                 selectInput("plot_type", "Choose a plot type:",
                             choices = c("Lolly" = "lolly",
                                         "Forest" = "forest",
                                         "Zipper" = "zip",
                                         "Heat" = "heat")),
                 plotOutput("selected_plot")
        ),
        tabPanel("Chosen metric", 
                 tags$p("Numerical values correspond to the selected scenario and the metric."),
                 checkboxInput("show_all_scenarios", "Show all scenarios side by side", value = FALSE),
                 uiOutput("numerical_values_output")
        ),
        tabPanel("All metrics", 
                 tags$p("Summary of all metrics across different models for a selected scenario (irrespective of the chosen metric)."),
                 uiOutput("summary_output"),
                 downloadButton("download_latex", "Download LaTeX Table"),
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
      
      object_name <- paste0("RD_", model)
      object_name_variants <- c(object_name, paste0(object_name, "_ER"), paste0(object_name, ".ER"),
                                paste0(object_name, "_OR"), paste0(object_name, ".OR"))
      
      est_model <- NULL
      for (obj in object_name_variants) {
        if (exists(obj)) {
          est_model <- get(obj)
          break
        }
      }
      
      if (is.null(est_model)) {
        warning(paste("Object", object_name, "not found in", file_path))
        next
      }
      
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
        warning_message <- paste("Warning: In scenario '", scenario_names[[scenario]], 
                                 "', requested number of simulations (", num_indices, 
                                 ") exceeds available simulations (", length(bross_indices), 
                                 "). Showing available data.", sep = "")
        
        # Print the warning in the console
        message(warning_message)
        
        # Show the warning in the Shiny UI
        showNotification(
          warning_message,
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
    est <- est[, required_columns, drop = FALSE]
    est$model <- factor(model_names[as.character(est$model)], levels = model_names)
    
    # Update reactive value without causing unnecessary reactivity
    isolate({
      effective_num_indices(length(bross_indices))
    })
    
    return(est)
  }
  
  
  
  
  
  data <- reactive({
    scenario <- input$scenario_select
    num_indices <- input$num_indices
    
    # Isolate to prevent unnecessary reactivity
    isolate({
      load_scenario_data(scenario, num_indices)
    })
  })
  
  observeEvent(input$num_indices, {
    # Isolate to ensure this doesn't trigger other reactive elements unnecessarily
    isolate({
      num_indices <- input$num_indices
      # Update reactive value once
      effective_num_indices(num_indices)
    })
  })
  
  
  perform_simsum_analysis <- function(est) {
    theta <- 0  # for RD or ATE; this would be 1 for OR
    simsum(data = est, estvarname = "RD", true = theta, se = "SE", 
           methodvar = "model", x = TRUE, ref = "Kitchen sink")
  }
  
  analysis <- reactive({
    perform_simsum_analysis(data())
  })
  
  generate_performance_measures <- function(s1, metrics, model_names) {
    performance_measures <- sapply(metrics, function(metric) {
      metric_data <- s1$summ[s1$summ$stat == metric, c("model", "est", "mcse")]
      metric_data$est <- sprintf("%.4f", metric_data$est)
      metric_data$mcse <- sprintf("%.4f", metric_data$mcse)
      paste0(metric_data$est, " (", metric_data$mcse, ")")
    })
    
    performance_measures <- data.frame(
      Measure = c("Bias", "Empirical SE", "MSE", "Model-based SE", "Coverage", "Bias-eliminated Coverage"),
      t(performance_measures)
    )
    
    colnames(performance_measures) <- c("Measure", model_names)
    
    performance_measures <- as.data.frame(t(performance_measures))
    colnames(performance_measures) <- performance_measures[1, ]  # Set first row as column names
    performance_measures <- performance_measures[-1, ]  # Remove the first row
    
    return(performance_measures)
  }
  
  
  selected_plot <- reactive({
    s1 <- analysis()
    plot_type <- input$plot_type
    internal_metric <- get_internal_metric(input$metric_select)
    
    if (is.na(internal_metric)) {
      showNotification("Invalid metric selected, please choose a valid metric.", type = "error")
      return(NULL)
    }
    
    if (plot_type == "zip" || plot_type == "lolly") {
      p <- autoplot(s1, type = plot_type, stats = internal_metric) + custom_theme
    } else if (plot_type == "forest") {
      p <- autoplot(summary(s1), type = plot_type, stats = internal_metric) + 
        custom_theme + labs(x = metric_names[internal_metric])
    } else {
      p <- autoplot(s1, type = plot_type, stats = internal_metric) + 
        custom_theme + labs(x = metric_names[internal_metric])
    }
    
    return(p)
  })
  
  
  
  output$selected_plot <- renderPlot({
    selected_plot()  # Call the reactive plot function
  })
  
  output$download_png <- downloadHandler(
    filename = function() {
      "selected_plot.png"
    },
    content = function(file) {
      ggsave(file, plot = selected_plot(), device = "png", width = 8, height = 6)
    }
  )
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      "selected_plot.pdf"
    },
    content = function(file) {
      ggsave(file, plot = selected_plot(), device = "pdf", width = 8, height = 6)
    }
  )
  
  output$download_jpg <- downloadHandler(
    filename = function() {
      "selected_plot.jpg"
    },
    content = function(file) {
      ggsave(file, plot = selected_plot(), device = "jpg", width = 8, height = 6)
    }
  )
  
  
  
  
  # Observer to change the metric to "Coverage" when "Zipper" is selected
  observeEvent(input$plot_type, {
    if (input$plot_type == "zip") {
      updateSelectInput(session, "metric_select", selected = "Coverage")
    }
  })
  
  output$numerical_values_output <- renderUI({
    combined_summary <- all_scenarios_data()
    
    if (is.null(combined_summary)) {
      showNotification("No data available for the selected metric.", type = "error")
      return(NULL)
    }
    
    internal_metric <- get_internal_metric(input$metric_select)
    
    if (!input$show_all_scenarios) {
      combined_summary <- combined_summary[combined_summary$Scenario == input$scenario_select, ]
    }
    
    metric_data <- combined_summary[combined_summary$stat == internal_metric, c("model", "est", "mcse", "Scenario")]
    
    metric_data$est <- sprintf("%.4f", metric_data$est)
    metric_data$mcse <- sprintf("%.4f", metric_data$mcse)
    metric_data$formatted <- paste0(metric_data$est, " (", metric_data$mcse, ")")
    
    metric_table <- if (input$show_all_scenarios) {
      metric_data %>%
        pivot_wider(names_from = Scenario, values_from = formatted, id_cols = model, values_fill = list(formatted = "NA")) %>%
        rename("Frequent" = scenario, "Rare Exposure" = scenarioER, "Rare Outcome" = scenarioOR, "Model" = model)
    } else {
      data.frame(Model = metric_data$model, `Estimate.MCSE` = metric_data$formatted)
    }
    
    table_html <- knitr::kable(metric_table, format = "html", digits = 8, row.names = FALSE, 
                               caption = paste("Metric:", metric_names[internal_metric])) %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
    
    div(HTML(table_html))
  })
  
  output$download_est_rds <- downloadHandler(
    filename = function() {
      paste("est_object_", input$scenario_select, ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(data(), file)
    }
  )
  
  output$summary_output <- renderUI({
    performance_measures <- performance_measures_reactive()
    
    table_html <- knitr::kable(performance_measures, format = "html", digits = 4, row.names = TRUE) %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
    
    div(HTML(table_html))
  })
  
  
  performance_measures_reactive <- reactive({
    s1 <- analysis()
    metrics <- c("bias", "empse", "mse", "modelse", "cover", "becover")
    
    generate_performance_measures(s1, metrics, model_names)
  })
  
  
  output$download_latex <- downloadHandler(
    filename = function() {
      "performance_measures_table.tex"
    },
    content = function(file) {
      performance_measures <- performance_measures_reactive()
      latex_table <- knitr::kable(performance_measures, format = "latex", digits = 4, row.names = TRUE) %>%
        kableExtra::kable_styling(latex_options = c("striped", "hold_position"))
      
      writeLines(latex_table, con = file)
    }
  )
  
  
  output$summary_output <- renderUI({
    performance_measures <- performance_measures_reactive()
    
    table_html <- knitr::kable(performance_measures, format = "html", digits = 4, row.names = TRUE) %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
    
    div(HTML(table_html))
  })
  
  
  se_comparison_plot <- reactive({
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
      custom_theme +
      scale_fill_manual(
        values = if (input$gray_scale) c("empse" = "lightgray", "modelse" = "darkgray") 
        else c("empse" = "skyblue", "modelse" = "orange"),
        labels = c("empse" = "Empirical SE", "modelse" = "Model SE")
      )
    
    return(p)
  })
  
  
  create_plot_download_handler <- function(filename, plot_function, extension) {
    downloadHandler(
      filename = function() { paste0(filename, ".", extension) },
      content = function(file) {
        ggsave(file, plot = plot_function(), device = extension, width = 8, height = 6)
      }
    )
  }
  
  output$download_png <- create_plot_download_handler("selected_plot", selected_plot, "png")
  output$download_pdf <- create_plot_download_handler("selected_plot", selected_plot, "pdf")
  output$download_jpg <- create_plot_download_handler("selected_plot", selected_plot, "jpg")
  
  output$download_secomp_png <- create_plot_download_handler("se_comparison_plot", se_comparison_plot, "png")
  output$download_secomp_pdf <- create_plot_download_handler("se_comparison_plot", se_comparison_plot, "pdf")
  output$download_secomp_jpg <- create_plot_download_handler("se_comparison_plot", se_comparison_plot, "jpg")
  
  output$download_metric_comp_png <- create_plot_download_handler("metric_comparison_plot", metric_comparison_plot, "png")
  output$download_metric_comp_pdf <- create_plot_download_handler("metric_comparison_plot", metric_comparison_plot, "pdf")
  output$download_metric_comp_jpg <- create_plot_download_handler("metric_comparison_plot", metric_comparison_plot, "jpg")
  
  
  
  output$se_comparison_plot <- renderPlot({
    se_comparison_plot()  # Call the reactive plot function
  })
  
  
  output$download_secomp_png <- create_download_handler("png")
  output$download_secomp_pdf <- create_download_handler("pdf")
  output$download_secomp_jpg <- create_download_handler("jpg")
  
  all_scenarios_data <- reactive({
    num_indices <- input$num_indices
    all_data <- do.call(rbind, lapply(c("scenario", "scenarioER", "scenarioOR"), function(scenario) {
      scenario_data <- load_scenario_data(scenario, num_indices)
      if (!is.null(scenario_data)) scenario_data$Scenario <- scenario
      scenario_data
    }))
    
    if (is.null(all_data)) return(NULL)
    
    scenario_summaries <- lapply(split(all_data, all_data$Scenario), function(est) {
      summary(perform_simsum_analysis(est))
    })
    
    combined_summary <- do.call(rbind, lapply(names(scenario_summaries), function(scenario) {
      scenario_summary <- scenario_summaries[[scenario]]$summ
      scenario_summary$Scenario <- scenario
      scenario_summary
    }))
    
    #print("Combined Summary in Chosen Metric Tab:")
    #print(combined_summary)
    return(combined_summary)
  })
  
  metric_comparison_plot <- reactive({
    combined_summary <- all_scenarios_data()
    
    if (is.null(combined_summary)) {
      showNotification("No data available for the selected metric across the scenarios.", type = "error")
      return(NULL)
    }
    
    internal_metric <- get_internal_metric(input$metric_select)
    combined_summary <- combined_summary[combined_summary$stat == internal_metric, ]
    
    if (nrow(combined_summary) == 0) {
      showNotification("No data available for the selected metric across the scenarios.", type = "error")
      return(NULL)
    } else {
      # Apply scenario names mapping for the legend
      combined_summary$Scenario <- factor(combined_summary$Scenario, levels = names(scenario_names), labels = scenario_names)
      
      p <- ggplot(combined_summary, aes(x = model, y = est, color = Scenario, linetype = Scenario)) +
        geom_point(position = position_dodge(width = 0.3)) +
        geom_errorbar(aes(ymin = est - mcse, ymax = est + mcse), width = 0.2, position = position_dodge(width = 0.3)) +
        geom_line(aes(group = Scenario), position = position_dodge(width = 0.3)) +
        custom_theme +
        labs(title = paste("Comparison of", metric_names[internal_metric], "Across Scenarios."),
             x = "Model", y = metric_names[internal_metric]) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
        scale_color_manual(values = if (input$black_and_white) c("black", "black", "black") 
                           else c("red", "blue", "green"))
      
      return(p)
    }
  })
  
  
  output$metric_comparison_plot <- renderPlot({
    metric_comparison_plot()  # Call the reactive plot function
  })
  
 
  output$download_combined_summary <- downloadHandler(
    filename = function() {
      paste("combined_summary_", input$metric_select, ".rds", sep = "")
    },
    content = function(file) {
      combined_summary <- all_scenarios_data()
      if (!is.null(combined_summary)) {
        internal_metric <- get_internal_metric(input$metric_select)
        combined_summary <- combined_summary[combined_summary$stat == internal_metric, ]
        saveRDS(combined_summary, file)
      }
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
