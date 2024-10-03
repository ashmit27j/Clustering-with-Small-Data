# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)  # Ensure this library is loaded to use read_csv()

# Load the dataset
data <- read_csv('StudentPerformanceFactors.csv')

# Step 1: Save the original data before cleaning
write_csv(data, 'before_cleaning.csv')

# Check the column names to see what we can plot
print(colnames(data))

# Step 2: Data Cleaning (Assuming numeric columns for outlier removal)
numeric_columns <- sapply(data, is.numeric)

# Optionally handle outliers using IQR method
Q1 <- apply(data[, numeric_columns], 2, quantile, 0.25, na.rm = TRUE)
Q3 <- apply(data[, numeric_columns], 2, quantile, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Print bounds for inspection
print(lower_bound)
print(upper_bound)

# Filter out outliers and keep NA rows intact
data_cleaned <- data

print("idhar")
print(data_cleaned)

data_cleaned <- data
for (i in which(numeric_columns)) {
  initial_rows <- nrow(data_cleaned)
  
  # Get the column name from its index
  column_name <- colnames(data_cleaned)[i]
  
  # Apply the filter using the column name dynamically
  data_cleaned <- data_cleaned %>%
    filter((.data[[column_name]] >= lower_bound[i] & .data[[column_name]] <= upper_bound[i]) | is.na(.data[[column_name]]))
  
  filtered_rows <- nrow(data_cleaned)
  cat("Filtered column:", column_name, "- Rows before:", initial_rows, "- Rows after:", filtered_rows, "\n")
}


# Step 3: Save the cleaned data
write_csv(data_cleaned, 'after_cleaning.csv')

# Check cleaned data dimensions
print(dim(data_cleaned))

# Step 4: Scatter Plot
if ("Attendance" %in% colnames(data_cleaned) & "Previous_Scores" %in% colnames(data_cleaned)) {
  ggplot(data_cleaned, aes(x = Attendance, y = Previous_Scores)) + 
    geom_point() +
    labs(title = "Scatter plot of Attendance vs Previous Scores",
         x = "Attendance", y = "Previous Scores") +
    theme_minimal()
} else {
  cat("Columns 'Attendance' and/or 'Previous_Scores' do not exist in the dataset.\n")
}
