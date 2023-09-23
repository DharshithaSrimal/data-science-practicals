### Task a: Provide detailed description of each datasets, their properties and relationships
# Install and load package
install.packages("readxl")
library(readxl)

### Explore planning data

# Get a list of all planning files in the folder
plan_files <- list.files(path = "Plan/", pattern = "^[^~$].*\\.xlsx$", all.files=FALSE, full.names = TRUE)
plan_files

#Read and explore the structure of a sample planning data file
sample_plan_data <- read_xlsx(plan_files[1])
str(sample_plan_data)

#Examine data properties, such as column names and data types
colnames(sample_plan_data)




### Explore production data

# Get a list of all planning files in the folder
production_files <- list.files(path = "Production Quantities/", pattern = "^[^~$].*\\.xlsx$", all.files=FALSE, full.names = TRUE)
production_files

#Read and explore the structure of a sample planning data file
sample_production_data <- read_xlsx(production_files[1])
str(sample_production_data)

#Examine data properties, such as column names and data types
colnames(sample_plan_data)




### Task b: Read data from CSV files into the R environment for processing 

install.packages("dplyr")
library(dplyr)

#Read all planning data files and production data files into separate data frames using a loop
plan_data_list <- lapply(plan_files, read_excel)
plan_data_list

production_data_list <- lapply(production_files, read_excel)
production_data_list


### Task c: Clean any outliers and exceptional values from the datasets

#Identify the data frame(s) with a character "Date" column
problematic_data_frames <- lapply(plan_data_list, function(data) {
  if (is.character(data$Date)) {
    return(data)
  } else {
    return(NULL)
  }
})


# Function to remove rows before the headers
remove_rows_before_headers <- function(df) {
  # Define a condition to match the expected header values
  header_condition <- df$...1 == "Module" &
    df$...2 == "Material"
  
  # Identify the row where the headers are located based on the condition
  header_row <- which(header_condition)
  
  # Remove rows before the header row
  if (!is.null(header_row) && length(header_row) > 0) {
    df <- df[(header_row[1] + 1):nrow(df), ]
  }
  
  return(df)
}

# Apply the function to each data frame in plan_data_list
plan_data_list <- lapply(plan_data_list, remove_rows_before_headers)

plan_data_list






#Identify the data frame(s) with a character "Material" column
problematic_data_frames <- lapply(plan_data_list, function(data) {
  if (is.character(data$Material)) {
    return(data)
  } else {
    return(NULL)
  }
})
#Convert character "Material" columns to double
for (i in seq_along(problematic_data_frames)) {
  if (!is.null(problematic_data_frames[[i]])) {
    problematic_data_frames[[i]]$Material <- as.numeric(problematic_data_frames[[i]]$Material)
  }
}

#Remove the problematic data frames from plan_data_list
plan_dataa_list <- plan_data_list[-which(!sapply(problematic_data_frames, is.null))]
#bind_rows to combine the data frames
combined_plan_data <- bind_rows(plan_dataa_list)
combined_plan_data
for (i in 1:length(plan_data_list)) {
  cat("Data Frame", i, ":\n")
  print(head(plan_data_list[[i]], 100))
}

install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

# Create a function to identify outliers using z-scores
identify_outliers <- function(data, column) {
  if (is.null(data)) {
    # Return NULL if the data frame is empty
    return(NULL)
  }
  if (!column %in% colnames(data)) {
    # Return NULL if the specified column is not present in the data frame
    return(NULL)
  }
  z_scores <- abs(scale(data[[column]]))
  threshold <- 3  # Adjust the threshold as needed (e.g., 2 or 3)
  return(data[z_scores <= threshold, ])
}

# Apply the function to each data frame in plan_data_list
cleaned_plan_data_list <- lapply(plan_data_list, function(data) {
  data %>% identify_outliers("SMV")
})
cleaned_plan_data_list




identify_and_visualize_outliers <- function(data, column) {
  if (is.null(data) || !column %in% colnames(data)) {
    return(NULL)
  }
  
  # Create a box plot to visualize the distribution and outliers
  ggplot(data, aes(x = "", y = .data[[column]])) +
    geom_boxplot() +
    labs(title = paste("Box Plot of", column)) +
    theme_minimal()
}


# Apply the function to each data frame in plan_data_list
outlier_plots <- lapply(plan_data_list, function(data) {
  identify_and_visualize_outliers(data, "SMV")
})
outlier_plots

# Display the box plots for each section
for (i in seq_along(outlier_plots)) {
  print(outlier_plots[[i]])
}
