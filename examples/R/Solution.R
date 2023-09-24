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
#################################################################



##### Task b: Read data from CSV files into the R environment for processing #####

install.packages("dplyr")
library(dplyr)

#Read all planning data files and production data files into separate data frames using a loop
plan_data_list <- lapply(plan_files, read_excel)
plan_data_list

# Function to remove rows before the headers
remove_rows_before_headers <- function(df) {
  # Define a condition to match the expected header values
  header_row <- which(
    df$...1 == "Module" & df$...2 == "Material"
  )
  
  # If headers are found, keep the header row and remove rows above it
  if (!is.null(header_row) && length(header_row) > 0) {
    df <- df[header_row[1]:nrow(df), ]
  }
  
  # Remove rows with empty values in specific columns
  columns_to_check <- c("SMV", "EFF.%", "Qty.", "Standard Hours.", "Work Hours.")
  for (column in columns_to_check) {
    if (any(!is.na(df[[column]]) & df[[column]] != "")) {
      df <- df[!is.na(df[[column]]) & df[[column]] != "", ]
    }
  }
  
  return(df)
}

# Apply the function to each data frame in plan_data_list
plan_data_list <- lapply(plan_data_list, remove_rows_before_headers)
plan_data_list

production_data_list <- lapply(production_files, read_excel)
production_data_list


### Task c: Clean any outliers and exceptional values from the datasets


## Removing outliers in planning data
# Function to remove rows with empty values in specific columns
remove_rows_with_empty_values <- function(data, columns_to_check) {
  for (col_name in columns_to_check) {
    if (!col_name %in% colnames(data)) {
      cat("Column", col_name, "not found in the data frame.\n")
      next
    }
    
    # Remove rows with empty values, NULL, or N/A in the specified column
    data <- data[!is.na(data[[col_name]]) & data[[col_name]] != "" & data[[col_name]] != "NULL", ]
  }
  
  return(data)
}

################################################################
# Specify the columns in  planning to check for empty values
columns_to_check <- c("SMV", "Eff. %", "Qty.", "S/O", "L/I")

# Apply the function to each data frame in plan_data_list
plan_data_list_cleaned <- lapply(plan_data_list, function(df) {
  df <- remove_rows_with_empty_values(df, columns_to_check)
  return(df)
})

# Display the cleaned data frames
for (i in 1:length(plan_data_list_cleaned)) {
  cat("Data Frame", i, "after removing rows with empty values:\n")
  print(plan_data_list_cleaned[[i]])
  cat("\n")
}
###############################################################
# Specify the columns in  planning to check for empty values
columns_to_check <- c("SMV", "Efficiency", "Qty.", "S/O", "L/I")

# Apply the function to each data frame in plan_data_list
prod_data_list_cleaned <- lapply(production_data_list, function(df) {
  df <- remove_rows_with_empty_values(df, columns_to_check)
  return(df)
})

# Display the cleaned data frames
for (i in 1:length(prod_data_list_cleaned)) {
  cat("Data Frame", i, "after removing rows with empty values:\n")
  print(prod_data_list_cleaned[[i]])
  cat("\n")
}
###############################################################

#Function to identify outliers
identify_outliers <- function(data, column) {
  # Calculate the lower and upper quartiles
  Q1 <- quantile(data[[column]], 0.25)
  Q3 <- quantile(data[[column]], 0.75)
  
  # Calculate the interquartile range (IQR)
  IQR <- Q3 - Q1
  
  # Define the lower and upper bounds for outliers as scalar values
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Identify outliers, excluding the first row (header)
  outliers <- data[-1, ] %>%
    filter(data[[column]] < lower_bound | data[[column]] > upper_bound)
  
  return(outliers)
}

## Apply the function to each data frame in plan_data_list ##

#SMV planning outliers
outliers_SMV_planning_list <- lapply(plan_data_list_cleaned, function(df) {
  outliers <- tryCatch(
    identify_outliers(df, "SMV"),  
    error = function(e) NULL  # Handle the case when no outliers are found
  )
  return(outliers)
})

# Display the list of outliers for each data frame
for (i in 1:length(outliers_SMV_planning_list)) {
  cat("Outliers in SMV", i, " ")
  print(outliers_SMV_planning_list[[i]])
  cat("\n")
}

# Remove outliers from each data frame in plan_data_list
# column_name <- "SMV"
# cleaned_plan_data_list <- lapply(plan_data_list_cleaned, function(df) {
#   # Check if the specified column exists in the data frame
#   if (!column_name %in% colnames(df)) {
#     return(df)  # Skip processing if the column doesn't exist
#   }
#   
#   # Check if the specified column contains valid numeric data
#   if (!all(is.numeric(df[[column_name]]))) {
#     return(df)  # Skip processing if the column is not numeric
#   }
#   
#   # Identify outliers, excluding the header row
#   outliers <- identify_outliers(df, column = column_name)
#   if (nrow(outliers) > 0) {
#     # Remove the rows for outliers while keeping the header row
#     df <- df[-as.numeric(rownames(outliers)) + 1, ]
#   }
#   return(df)
# })

# Display the modified data frames without outliers
# cleaned_plan_data_list

#Efficency planning outliers
outliers_EFF_planning_list <- lapply(plan_data_list_cleaned, function(df) {
  outliers <- tryCatch(
    identify_outliers(df, "Eff. %"),  
    error = function(e) NULL  # Handle the case when no outliers are found
  )
  return(outliers)
})

# Display the list of outliers for each data frame
for (i in 1:length(outliers_EFF_planning_list)) {
  cat("Outliers in Eff. %", i, " ")
  print(outliers_EFF_planning_list[[i]])
  cat("\n")
}
plan_data_list_cleaned



## Apply the function to each data frame in prod_data_list ##

#SMV planning outliers
outliers_SMV_prod_list <- lapply(prod_data_list_cleaned, function(df) {
  outliers <- tryCatch(
    identify_outliers(df, "SMV"),  
    error = function(e) NULL  # Handle the case when no outliers are found
  )
  return(outliers)
})

# Display the list of outliers for each data frame
for (i in 1:length(outliers_SMV_prod_list)) {
  cat("Outliers in SMV", i, " ")
  print(outliers_SMV_prod_list[[i]])
  cat("\n")
}

#Efficiency planning outliers
outliers_Eff_prod_list <- lapply(prod_data_list_cleaned, function(df) {
  outliers <- tryCatch(
    identify_outliers(df, "Efficiency"),  
    error = function(e) NULL  # Handle the case when no outliers are found
  )
  return(outliers)
})

# Display the list of outliers for each data frame
for (i in 1:length(outliers_Eff_prod_list)) {
  cat("Outliers in Efficiency", i, " ")
  print(outliers_SMV_prod_list[[i]])
  cat("\n")
}







# # Apply the function to each data frame in production_data_list
# 
# #SMV production outliers
# outliers_production_list <- lapply(production_data_list, function(df) {
#   outliers <- tryCatch(
#     identify_outliers(df, "SMV"),  
#     error = function(e) NULL  # Handle the case when no outliers are found
#   )
#   return(outliers)
# })
# outliers_production_list
# 
# # Apply the function to each data frame in plan_data_list
# outliers_production_list <- lapply(production_data_list, function(df) {
#   identify_outliers(df, "SMV")  # Replace "SMV" with the column you want to analyze for outliers
# })
# 
# 
# # Filter out NULL elements from the production list
# outliers_production_list <- outliers_list[sapply(outliers_production_list, function(x) !is.null(x))]
# outliers_production_list


# #Identify the data frame(s) with a character "Material" column
# problematic_data_frames <- lapply(plan_data_list, function(data) {
#   if (is.character(data$Material)) {
#     return(data)
#   } else {
#     return(NULL)
#   }
# })
# #Convert character "Material" columns to double
# for (i in seq_along(problematic_data_frames)) {
#   if (!is.null(problematic_data_frames[[i]])) {
#     problematic_data_frames[[i]]$Material <- as.numeric(problematic_data_frames[[i]]$Material)
#   }
# }
# 
# # Function to convert date columns to character
# convert_date_column <- function(df) {
#   # Find the columns that are of POSIXct or POSIXt data type
#   #date_columns <- names(df)[sapply(df, is.POSIXct) | sapply(df, is.POSIXt)]
#   
#   # Check if the column name is null and rename it to "Date"
#   date_columns <- ifelse(is.null(date_columns), "Date", date_columns)
#   
#   # Convert each date column to character
#   for (col in date_columns) {
#     df[[col]] <- as.character(df[[col]])
#   }
#   
#   # Rename the columns to "Date"
#   names(df) <- ifelse(names(df) %in% date_columns, "Date", names(df))
#   
#   return(df)
# }
# 
# # Apply the function to each data frame in plan_data_list
# plan_data_list <- lapply(plan_data_list, convert_date_column)
# plan_data_list
# 
# #bind_rows to combine the data frames
# combined_plan_data <- bind_rows(plan_dataa_list)
# combined_plan_data
# for (i in 1:length(plan_data_list)) {
#   cat("Data Frame", i, ":\n")
#   print(head(plan_data_list[[i]], 100))
# }


###########################################


##### Task d: Normalizations, Scaling #####

# Define the list of columns to check for empty values
columns_to_check <- c("SMV", "Qty.", "Standard Hours.", "Work Hours.")

# Define the range you want to normalize to
min_range <- 0
max_range <- 1

# Function to normalize data to a custom range
normalize_to_range <- function(df, columns_to_normalize, min_val, max_val) {
  # Check if the header row exists
  if (nrow(df) >= 1) {
    header_row <- df[1, ]
    
    # Find the indices of columns to normalize based on header names
    selected_columns <- names(header_row) %in% columns_to_normalize
    
    # Apply normalization to selected columns
    for (col in names(df[, selected_columns])) {
      df[, col] <- (df[, col] - min(df[, col])) / (max(df[, col]) - min(df[, col])) * (max_val - min_val) + min_val
    }
  }
  
  return(df)
}

## Planning Data ##

# Apply the function to each data frame in plan_data_list_cleaned
plan_normalized_data_list <- lapply(plan_data_list_cleaned, normalize_to_range, columns_to_normalize = columns_to_check, min_val = min_range, max_val = max_range)
plan_normalized_data_list


## Production Data ##

# Apply the function to each data frame in prod_data_list_cleaned
prod_normalized_data_list <- lapply(prod_data_list_cleaned, normalize_to_range, columns_to_normalize = columns_to_check, min_val = min_range, max_val = max_range)
prod_normalized_data_list

###########################################

###### Task e: Merge the datasets #######

# Combine all data frames in the list into a single data frame
plan_combinedd <- bind_rows(plan_normalized_data_list)

# Check the structure of the combined data frame
str(plan_combined)

# Repeat the same process for the production data
prod_combined <- bind_rows(prod_normalized_data_list)

# Check the structure of the combined production data frame
str(prod_combined)



install.packages("lubridate")
library(lubridate)  # For date/time manipulation

# Function to convert "Date" column to datetime format, handling missing cases
convert_date_column <- function(df) {
  if ("Date" %in% colnames(df)) {
    df$Date <- as_datetime(df$Date)
  }
  return(df)
}

# Function to ensure "Material" column exists, and convert if needed
convert_material_column <- function(df) {
  if ("Material" %in% colnames(df)) {
    df$Material <- as.character(df$Material)
  }
  return(df)
}

# Function to ensure "S/O" column exists, and convert if needed
convert_SO_column <- function(df) {
  if ("S/O" %in% colnames(df)) {
    df$S/O <- as.character(df$S/O)
  }
  return(df)
}

# Apply the function to each data frame in the list
plan_normalized_data_list <- lapply(plan_normalized_data_list, convert_date_column)
prod_normalized_data_list <- lapply(prod_normalized_data_list, convert_date_column)

# Apply the material column conversion function to each data frame in the list
plan_normalized_data_list <- lapply(plan_normalized_data_list, convert_material_column)
prod_normalized_data_list <- lapply(prod_normalized_data_list, convert_material_column)

# Function to ensure a column exists and convert if needed
convert_column <- function(df, column_name, target_data_type) {
  if (column_name %in% colnames(df)) {
    current_data_type <- typeof(df[[column_name]])
    if (current_data_type != target_data_type) {
      df[[column_name]] <- as(target_data_type, df[[column_name]])
    }
  }
  return(df)
}
# Apply the column conversion function to each data frame in the list
column_name <- "S/O"  # Replace with the actual column name
target_data_type <- "character"  # Replace with the desired data type

plan_normalized_data_list <- lapply(plan_normalized_data_list, function(df) {
  convert_column(df, column_name, target_data_type)
})

prod_normalized_data_list <- lapply(prod_normalized_data_list, function(df) {
  convert_column(df, column_name, target_data_type)
})

# Combine all data frames in the list into a single data frame
plan_combined <- bind_rows(plan_normalized_data_list)
prod_combined <- bind_rows(prod_normalized_data_list)












# Combine all data frames in the list into a single data frame
plan_combined <- bind_rows(plan_normalized_data_list)
prod_combined <- bind_rows(prod_normalized_data_list)

# Check the structure of the combined data frame
str(plan_combined)
str(prod_combined)



