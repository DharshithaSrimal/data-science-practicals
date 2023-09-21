# Install and load package
install.packages("readxl")
library(readxl)

# Get a list of all planning files in the folder
plan_files <- list.files(path = "Plan/", pattern = "*.xlsx", full.names = TRUE)
plan_files

# Read all the planning files into a list of data frames
planning_frames_list <- lapply(plan_files, read_excel)

# Now, planning_frames_list contains planning data frames, one for each Excel file
# You can access individual data frames like data_frames_list[[1]], data_frames_list[[2]], etc.
planning_frames_list[[10]]