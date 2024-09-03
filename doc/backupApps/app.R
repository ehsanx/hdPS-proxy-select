setwd("/Users/jidianlewei/Desktop/WL_MachineLearning/hdPS/hdPS_ProxySelect/doc")
library(shiny)
# Define UI
ui <- fluidPage(
  titlePanel("Select a Scenario and Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput("scenario_select", "Choose a scenario:", 
                  choices = list("Frequent Outcome and Exposure" = "scenario",
                                 "Rare Exposure" = "scenarioER",
                                 "Rare Outcome" = "scenarioOR")
      ),
      selectInput("plot_select", "Choose a plot:", 
                  choices = list("Bias" = "bias.png", 
                                 "Coverage" = "cover.png", 
                                 "Empirical SE" = "empse.png", 
                                 "Model SE" = "modelse.png", 
                                 "Comparison" = "secompare.png", 
                                 "Zip" = "zip.png",
                                 "Bias Coverage" = "becover.png")
      )
    ),
    mainPanel(
      imageOutput("selected_plot")
    )
  )
)

# Server logic
server <- function(input, output) {
  output$selected_plot <- renderImage({
    # Get the selected scenario and plot file name
    scenario <- input$scenario_select
    plot_file <- input$plot_select
    
    # Construct the full path to the image based on the scenario
    full_path <- file.path("images", scenario, plot_file)
    
    # Return a list containing the filename
    list(src = full_path,
         contentType = 'image/png',
         alt = "Selected plot")
  }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
