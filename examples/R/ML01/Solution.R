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



######## Task b: Read data from CSV files into the R environment for processing #####

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

#SMV production outliers
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

install.packages("lubridate")
library(lubridate)  # For date/time manipulation

# Function to convert "Date" column to datetime format, handling missing cases
convert_date_column <- function(df) {
  if ("Date" %in% colnames(df)) {
    df$Date <- as.character(df$Date)
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
    # Use backticks to reference the column with a special character
    df$`S/O` <- as.character(df$`S/O`)
  }
  return(df)
}

# Apply the function to each data frame in the list
plan_normalized_data_list <- lapply(plan_normalized_data_list, convert_date_column)
prod_normalized_data_list <- lapply(prod_normalized_data_list, convert_date_column)

# Apply the material column conversion function to each data frame in the list
plan_normalized_data_list <- lapply(plan_normalized_data_list, convert_material_column)
prod_normalized_data_list <- lapply(prod_normalized_data_list, convert_material_column)

# Apply the SO column conversion function to each data frame in the list
plan_normalized_data_list <- lapply(plan_normalized_data_list, convert_SO_column)
prod_normalized_data_list <- lapply(prod_normalized_data_list, convert_SO_column)

# Combine all data frames in the list into a single data frame
plan_combined <- bind_rows(plan_normalized_data_list)
prod_combined <- bind_rows(prod_normalized_data_list)

# Check the structure of the combined data frame
str(plan_combined)
plan_combined
str(prod_combined)
prod_combined


# Set the first row as column names and remove it for plan_combined
new_colnames <- plan_combined[1, ]
plan_combined <- plan_combined[-1, ]
# Replace column names with non-empty, non-NULL, and non-NA values
colnames(plan_combined) <- ifelse(!is.na(new_colnames) & new_colnames != "" & !is.null(new_colnames), new_colnames, colnames(plan_combined))
# Set the first row as column names and remove it for prod_combined
new_colnames <- prod_combined[1, ]
prod_combined <- prod_combined[-1, ]
# Replace column names with non-empty, non-NULL, and non-NA values
colnames(prod_combined) <- ifelse(!is.na(new_colnames) & new_colnames != "" & !is.null(new_colnames), new_colnames, colnames(prod_combined))

# Display the modified data frames
print(plan_combined)
print(prod_combined)

# Merge the data frames based on the unique key
# Find the column names "SO" and "LI" in prod_combined and rename them
if ("SO" %in% colnames(prod_combined)) {
  colnames(prod_combined)[colnames(prod_combined) == "SO"] <- "S/O"
}

if ("LI" %in% colnames(prod_combined)) {
  colnames(prod_combined)[colnames(prod_combined) == "LI"] <- "L/I"
}

if ("Eff. %" %in% colnames(plan_combined)) {
  colnames(plan_combined)[colnames(plan_combined) == "Eff. %"] <- "Efficiency"
}

if ("Standard Hours." %in% colnames(plan_combined)) {
  colnames(plan_combined)[colnames(plan_combined) == "Standard Hours."] <- "Standard Hours"
}

if ("Work Hours." %in% colnames(plan_combined)) {
  colnames(plan_combined)[colnames(plan_combined) == "Work Hours."] <- "Worked Hours"
}

# Select the desired columns from plan_combined
plancombined <- select(plan_combined, "S/O", "L/I", "Efficiency", "Standard Hours", "Worked Hours", "SMV")
plancombined

# Select the desired columns from prod_combined
prodcombined <- select(prod_combined, "S/O", "L/I", "Efficiency", "Standard Hours", "Worked Hours", "SMV")
prodcombined

# Now, perform the outer join
merged_data <- inner_join(plancombined, prodcombined, by = c("S/O" = "S/O", "L/I" = "L/I"))
merged_data
#########################################################################


############ Task f: Create training and test datasets #################

# For data splitting and modeling
install.packages("caret") 
library(caret)

set.seed(123)  # Set a seed for reproducibility
split_ratio <- 0.7  # 70% for training, 30% for testing

# Calculate the number of rows for the training set
num_training <- round(nrow(merged_data) * split_ratio)

# Randomly sample rows for the training set
training_indexes <- sample(seq_len(nrow(merged_data)), size = num_training)

# Create the training and testing datasets
training_data <- merged_data[training_indexes, ]
training_data
testing_data <- merged_data[-training_indexes, ]
testing_data


#Check the Result
dim(training_data)
dim(testing_data)

#######################################################################

###### Task g&h: Training a model on the data | Apply different Machine Learning approaches and discuss#######################

# For data visualization
# install.packages("ggplot2") 
library(ggplot2)  


##############Clustering##########################

# Assuming you have the necessary libraries loaded (e.g., 'cluster' for k-means)
library(cluster)

# Select the columns for clustering
cluster_data <- select(merged_data, "Efficiency.x", "Standard Hours.x", "Worked Hours.x", "SMV.y")

# Remove rows with missing values in the selected columns
cluster_data <- cluster_data[complete.cases(cluster_data), ]

# Check data types again
str(cluster_data)

# Check for missing values
summary(cluster_data)
anyNA(cluster_data)

# Convert character columns to numeric
cluster_data$Efficiency.x <- as.numeric(cluster_data$Efficiency.x)
cluster_data$`Standard Hours.x` <- as.numeric(cluster_data$`Standard Hours.x`)
cluster_data$`Worked Hours.x` <- as.numeric(cluster_data$`Worked Hours.x`)
cluster_data$SMV.y <- as.numeric(cluster_data$SMV.y)


########Average efficiency total dataset ###########
average_efficiency <- mean(cluster_data$Efficiency.x)
average_efficiency
#######################################################


# Now, scale the numeric columns
normalized_data <- scale(cluster_data)

# Specify the number of clusters (k)
k <- 3  # You can adjust the number of clusters as needed

# Perform k-means clustering
kmeans_result <- kmeans(normalized_data, centers = k)
kmeans_result

# Extract cluster assignments
cluster_assignments <- kmeans_result$cluster
cluster_assignments

# Add cluster assignments to the original data
merged_data$Cluster <- cluster_assignments
merged_data$Cluster

# View the cluster assignments
table(cluster_assignments)


# Assuming you have a numeric dataset (e.g., normalized_data)
library(ggplot2)

# Assuming 'cluster_assignments' contains cluster assignments
merged_data$Cluster <- cluster_assignments

############## Task i: Evaluation ###########
silhouette_score <- silhouette(cluster_assignments, dist(normalized_data))
mean(silhouette_score)
############################################

########## Task k: Task k: Patterns identified and their visualizations #############################
# Create a scatterplot for Efficiency vs. Standard Hours within each cluster
ggplot(merged_data, aes(x = Efficiency.x, y = `Standard Hours.x`, color = as.factor(Cluster))) +
  geom_point() +
  labs(title = "Relationship between Efficiency and Standard Hours by Cluster")

#######################################################################################################




##############Linear regression##########################

# Assuming you have a training dataset (training_data) and a testing dataset (testing_data)

# Convert training_data character columns to numeric
training_data$Efficiency.x <- as.numeric(training_data$Efficiency.x)
training_data$`Standard Hours.x` <- as.numeric(training_data$`Standard Hours.x`)
training_data$`Worked Hours.x` <- as.numeric(training_data$`Worked Hours.x`)
training_data$SMV.y <- as.numeric(training_data$SMV.y)

# Convert testing_data character columns to numeric
testing_data$Efficiency.x <- as.numeric(testing_data$Efficiency.x)
testing_data$`Standard Hours.x` <- as.numeric(testing_data$`Standard Hours.x`)
testing_data$`Worked Hours.x` <- as.numeric(testing_data$`Worked Hours.x`)
testing_data$SMV.y <- as.numeric(testing_data$SMV.y)

################ Task g: Train a model on the data ###################################

# Train the chosen model using the training dataset.
model <- lm(Efficiency.x ~ ., data = training_data)  # Replace with your model of choice

######################################################################################



#######################Task I: Accuracy of each different models ####################

# Make predictions on the testing data
predictions <- predict(model, newdata = testing_data)

# Calculate the evaluation metric (e.g., Mean Absolute Error for regression)
mae <- mean(abs(predictions - testing_data$Efficiency))

# Print the Mean Absolute Error (or use other relevant metrics)
print(paste("Mean Absolute Error:", mae))

############################################################################


#####################Task k: Patterns identified and their visualizations ##################################

plot(testing_data$Efficiency, predictions, main = "Model Evaluation", 
     xlab = "Actual Efficiency", ylab = "Predicted Efficiency")
########################################################################################################



##################################################################
