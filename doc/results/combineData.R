# Load necessary libraries
library(dplyr)

# Define the base path where the data is stored
base_path <- "C:/Users/Ehsan/Documents/GitHub/hdPS-proxy-select/doc/results"

# Scenario 1: "scenario"
scenario <- "scenario"
scenario_path <- file.path(base_path, scenario)
files <- list.files(scenario_path, pattern = "\\.RData$", full.names = TRUE)
print(paste("Processing scenario:", scenario))
print(paste("Files found:", files))

scenario_data_list <- list()

for (file_path in files) {
  load(file_path)
  model <- tools::file_path_sans_ext(basename(file_path))
  print(paste("Processing file:", file_path))
  print(paste("Model identified:", model))
  
  object_name <- paste0("RD_", model)
  
  if (exists(object_name)) {
    print(paste("Found object:", object_name))
    assign("current_data", get(object_name))
  } else {
    warning(paste("Object", object_name, "not found in", file_path))
    next
  }
  
  current_data$model <- model
  print(head(current_data))
  scenario_data_list[[model]] <- current_data
}

combined_scenario_data <- bind_rows(scenario_data_list)
print(paste("Combined data for", scenario, ":", nrow(combined_scenario_data), "rows"))
saveRDS(combined_scenario_data, file.path(base_path, paste0(scenario, "_combined.rds")))
message(paste("Saved combined data for", scenario, "to", paste0(scenario, "_combined.rds")))

# Scenario 2: "scenarioER"
scenarioER <- "scenarioER"
scenario_path_ER <- file.path(base_path, scenarioER)
files_ER <- list.files(scenario_path_ER, pattern = "\\.RData$", full.names = TRUE)
print(paste("Processing scenario:", scenarioER))
print(paste("Files found:", files_ER))

scenarioER_data_list <- list()

for (file_path in files_ER) {
  load(file_path)
  model <- tools::file_path_sans_ext(basename(file_path))
  print(paste("Processing file:", file_path))
  print(paste("Model identified:", model))
  
  object_name <- paste0("RD_", model)
  object_name_er <- paste0("RD_", model, "_ER")
  object_name_dot_er <- paste0("RD_", model, ".ER")
  
  if (exists(object_name_er)) {
    print(paste("Found object:", object_name_er))
    assign("current_data", get(object_name_er))
  } else if (exists(object_name_dot_er)) {
    print(paste("Found object:", object_name_dot_er))
    assign("current_data", get(object_name_dot_er))
  } else if (exists(object_name)) {
    print(paste("Found object:", object_name))
    assign("current_data", get(object_name))
  } else {
    warning(paste("Object", object_name_er, "not found in", file_path))
    next
  }
  
  current_data$model <- model
  print(head(current_data))
  scenarioER_data_list[[model]] <- current_data
}

combined_scenarioER_data <- bind_rows(scenarioER_data_list)
print(paste("Combined data for", scenarioER, ":", nrow(combined_scenarioER_data), "rows"))
saveRDS(combined_scenarioER_data, file.path(base_path, paste0(scenarioER, "_combined.rds")))
message(paste("Saved combined data for", scenarioER, "to", paste0(scenarioER, "_combined.rds")))

# Scenario 3: "scenarioOR"
scenarioOR <- "scenarioOR"
scenario_path_OR <- file.path(base_path, scenarioOR)
files_OR <- list.files(scenario_path_OR, pattern = "\\.RData$", full.names = TRUE)
print(paste("Processing scenario:", scenarioOR))
print(paste("Files found:", files_OR))

scenarioOR_data_list <- list()

for (file_path in files_OR) {
  load(file_path)
  model <- tools::file_path_sans_ext(basename(file_path))
  print(paste("Processing file:", file_path))
  print(paste("Model identified:", model))
  
  object_name <- paste0("RD_", model)
  object_name_or <- paste0("RD_", model, "_OR")
  object_name_dot_or <- paste0("RD_", model, ".OR")
  
  if (exists(object_name_or)) {
    print(paste("Found object:", object_name_or))
    assign("current_data", get(object_name_or))
  } else if (exists(object_name_dot_or)) {
    print(paste("Found object:", object_name_dot_or))
    assign("current_data", get(object_name_dot_or))
  } else if (exists(object_name)) {
    print(paste("Found object:", object_name))
    assign("current_data", get(object_name))
  } else {
    warning(paste("Object", object_name_or, "not found in", file_path))
    next
  }
  
  current_data$model <- model
  print(head(current_data))
  scenarioOR_data_list[[model]] <- current_data
}

combined_scenarioOR_data <- bind_rows(scenarioOR_data_list)
print(paste("Combined data for", scenarioOR, ":", nrow(combined_scenarioOR_data), "rows"))
saveRDS(combined_scenarioOR_data, file.path(base_path, paste0(scenarioOR, "_combined.rds")))
message(paste("Saved combined data for", scenarioOR, "to", paste0(scenarioOR, "_combined.rds")))
  
# Load the three RDS files
scenario_data <- readRDS(file.path(base_path, "scenario_combined.rds"))
scenarioER_data <- readRDS(file.path(base_path, "scenarioER_combined.rds"))
scenarioOR_data <- readRDS(file.path(base_path, "scenarioOR_combined.rds"))

# Combine them into a list
combined_data_list <- list(
  scenario = scenario_data,
  scenarioER = scenarioER_data,
  scenarioOR = scenarioOR_data
)

# Now `combined_data_list` contains all three datasets
saveRDS(combined_data_list, file.path(base_path, paste0("combined_list.rds")))


######################

# Identify rows with NA values in the 'RD' or 'SE' columns for the "bross" method in each scenario
failing_rows_scenario <- rownames(combined_data_list$scenario)[combined_data_list$scenario$model == "bross" & 
                                                                 (is.na(combined_data_list$scenario$RD))]

failing_rows_scenarioER <- rownames(combined_data_list$scenarioER)[combined_data_list$scenarioER$model == "bross" & 
                                                                     (is.na(combined_data_list$scenarioER$RD))]

failing_rows_scenarioOR <- rownames(combined_data_list$scenarioOR)[combined_data_list$scenarioOR$model == "bross" & 
                                                                     (is.na(combined_data_list$scenarioOR$RD))]



# Extract data index numbers for 'scenario'
index_scenario <- gsub("^data\\.(\\d+)_bross$", "\\1", failing_rows_scenario)

# Extract data index numbers for 'scenarioER'
index_scenarioER <- gsub("^data\\.ER\\.(\\d+)_bross$", "\\1", failing_rows_scenarioER)

# Extract data index numbers for 'scenarioOR'
index_scenarioOR <- gsub("^data\\.OR\\.(\\d+)_bross$", "\\1", failing_rows_scenarioOR)

# Convert to numeric
index_scenario <- as.numeric(index_scenario)
length(index_scenario)
index_scenarioER <- as.numeric(index_scenarioER)
length(index_scenarioER)
index_scenarioOR <- as.numeric(index_scenarioOR)
length(index_scenarioOR)

# Print the extracted indices
print("Extracted data index numbers for scenario:")
print(index_scenario)

print("Extracted data index numbers for scenarioER:")
print(index_scenarioER)

print("Extracted data index numbers for scenarioOR:")
print(index_scenarioOR)


# Define a function to filter the data based on specific indices
filter_scenario_data <- function(data, index.fail) {
  # Extract the numeric part of the rownames for comparison
  row_indices <- as.numeric(gsub(".*\\.(\\d+).*", "\\1", rownames(data)))
  
  # Keep only the rows whose index is not in index.fail
  filtered_data <- data[!row_indices %in% index.fail, ]
  
  return(filtered_data)
}

# Apply the filter to each dataset in the combined_data_list
filtered_scenario_data <- filter_scenario_data(combined_data_list$scenario, index_scenario)
filtered_scenarioER_data <- filter_scenario_data(combined_data_list$scenarioER, index_scenarioER)
filtered_scenarioOR_data <- filter_scenario_data(combined_data_list$scenarioOR, index_scenarioOR)

# Save the filtered datasets
saveRDS(filtered_scenario_data, file.path(base_path, "scenario_filtered_individual.rds"))
saveRDS(filtered_scenarioER_data, file.path(base_path, "scenarioER_filtered_individual.rds"))
saveRDS(filtered_scenarioOR_data, file.path(base_path, "scenarioOR_filtered_individual.rds"))

# Save the entire filtered list
filtered_combined_data_list <- list(
  scenario = filtered_scenario_data,
  scenarioER = filtered_scenarioER_data,
  scenarioOR = filtered_scenarioOR_data
)

saveRDS(filtered_combined_data_list, file.path(base_path, "combined_list_filtered_individual.rds"))

# Print a summary of the filtering process
print("Filtering completed.")
print(paste("Number of rows in filtered scenario:", nrow(filtered_scenario_data)))
print(paste("Number of rows in filtered scenarioER:", nrow(filtered_scenarioER_data)))
print(paste("Number of rows in filtered scenarioOR:", nrow(filtered_scenarioOR_data)))




index.fail <- sort(unique(c(index_scenario, index_scenarioER, index_scenarioOR)))
length(index.fail)

# Filter combined_data_list based on data that does not belong to index.fail

# Define a function to filter the data
filter_data <- function(data, index.fail) {
  # Extract the numeric part of the rownames for comparison
  row_indices <- as.numeric(gsub(".*\\.(\\d+).*", "\\1", rownames(data)))
  
  # Keep only the rows whose index is not in index.fail
  filtered_data <- data[!row_indices %in% index.fail, ]
  
  return(filtered_data)
}

# Apply the filter to each dataset in the combined_data_list
combined_data_list_filtered <- lapply(combined_data_list, filter_data, index.fail = index.fail)

# Save the filtered datasets
# saveRDS(combined_data_list_filtered$scenario, file.path(base_path, "scenario_filtered.rds"))
# saveRDS(combined_data_list_filtered$scenarioER, file.path(base_path, "scenarioER_filtered.rds"))
# saveRDS(combined_data_list_filtered$scenarioOR, file.path(base_path, "scenarioOR_filtered.rds"))

# Save the entire filtered list
saveRDS(combined_data_list_filtered, file.path(base_path, "combined_list_filtered.rds"))

# Print a summary of the filtering process
print("Filtering completed.")
print(paste("Number of rows in filtered scenario:", nrow(combined_data_list_filtered$scenario)))
print(paste("Number of rows in filtered scenarioER:", nrow(combined_data_list_filtered$scenarioER)))
print(paste("Number of rows in filtered scenarioOR:", nrow(combined_data_list_filtered$scenarioOR)))

