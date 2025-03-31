# --------------------------------------------------------------------
# SECTION 1: COURSE AND ASSIGNMENT DETAILS
# --------------------------------------------------------------------
# Course      : LIS-4317 Introduction to Visual Analytics
# Assignment  : Assignment 10
# URL         : https://usflearn.instructure.com/courses/1926966/assignments/17693285
# Filename    : LIS-4317-Mod-10-Code.R
# Purpose     : This script uses ggplot2 to visualize hot dog contest and economics data.
# Author      : Steven Barden
# Email       : StevenBarden@usf.edu
# Created     : 2025-03-31-0000 (EST)
# Updated     : 2025-03-31-1200 (EST)
# License     : UltraFree
# Description : This script uses ggplot2 to visualize hot dog contest and economics data.

# --------------------------------------------------------------------
# SECTION 2: ENVIRONMENT SETUP
# --------------------------------------------------------------------

# Set Working Directory (Modify as Needed)
tryCatch({
  baseDir <- r"(C:\Users\Steve\OneDrive\College\_____DESKTOP ICONS\2025-USF-Spring\Classes\4317\Mod10)"
  dir.exists(baseDir)  # Checks if the directory exists
  setwd(baseDir)
}, error = function(e) {
  stop("Error setting the working directory: ", e$message)
})


# Load Required Libraries
tryCatch({
  library(dplyr)
  library(ggplot2)
  library(readr)  # For reading CSV from URL
}, error = function(e) {
  stop("Error loading libraries: ", e$message)
})

# --------------------------------------------------------------------
# SECTION 5: DATA LOADING
# --------------------------------------------------------------------

# Load Hot Dog Data
hotdogs <- read_csv("http://datasets.flowingdata.com/hot-dog-contest-winners.csv")

# Load Economics Data (built into ggplot2)
data(economics, package = "ggplot2")

# --------------------------------------------------------------------
# SECTION 6: DATA PROCESSING
# --------------------------------------------------------------------

# Clean Hot Dog Data
clean_hotdogs <- function(data) {
  tryCatch({
    # Remove duplicates
    data <- distinct(data)
    # Handle missing values
    data <- na.omit(data)
    cat("Hot dog data cleaned successfully.\n")
    return(data)
  }, error = function(e) {
    stop("Error cleaning hot dog data: ", e$message)
  })
}

# Clean and Transform Economics Data
process_economics <- function(data) {
  tryCatch({
    # Add year column
    data$year <- as.POSIXlt(data$date)$year + 1900
    # Remove duplicates
    data <- distinct(data)
    # Handle missing values
    data <- na.omit(data)
    cat("Economics data processed successfully.\n")
    return(data)
  }, error = function(e) {
    stop("Error processing economics data: ", e$message)
  })
}

# Apply Cleaning Functions
hotdogs_clean <- clean_hotdogs(hotdogs)
economics_clean <- process_economics(economics)

# --------------------------------------------------------------------
# SECTION 8: VISUALIZATION FUNCTIONS
# --------------------------------------------------------------------

# Hot Dog Visualization
visualize_hotdogs <- function(data) {
  tryCatch({
    ggplot(data) + 
      geom_bar(aes(x = Year, y = `Dogs eaten`, fill = factor(`New record`)), 
               stat = "identity") + 
      labs(title = "Nathan's Hot Dog Contest Wins, 1980-2010", 
           x = "Year", y = "Hot Dogs Eaten", fill = "New Record") +
      scale_fill_manual(values = c("grey", "purple")) +  # My creative touch!
      theme_minimal()
    cat("Hot dog visualization created.\n")
  }, error = function(e) {
    stop("Error visualizing hot dogs: ", e$message)
  })
}

# Economics Visualization
visualize_economics <- function(data) {
  tryCatch({
    ggplot(data, aes(x = date, y = unemploy / pop)) +
      geom_line(color = "green") +  # My creative touch!
      labs(title = "Unemployment Rate Over Time", 
           x = "Date", y = "Unemployment/Population") +
      theme_minimal()
    cat("Economics visualization created.\n")
  }, error = function(e) {
    stop("Error visualizing economics: ", e$message)
  })
}

# --------------------------------------------------------------------
# SECTION 9: MAIN EXECUTION BLOCK
# --------------------------------------------------------------------

main <- function() {
  tryCatch({
    cat("Starting script execution...\n")
    # Step 1: Load and Process Data
    cat("Step 1: Processing data...\n")
    hotdogs_processed <- clean_hotdogs(hotdogs)
    economics_processed <- process_economics(economics)
    # Step 2: Visualize Data
    cat("Step 2: Visualizing results...\n")
    visualize_hotdogs(hotdogs_processed)
    visualize_economics(economics_processed)
    cat("Script execution completed successfully.\n")
  }, error = function(e) {
    stop("Script execution failed: ", e$message)
  })
}

# Run the script
main()