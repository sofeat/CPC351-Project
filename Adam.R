# Package to read xls
library(dplyr)
library("readxl")
library(e1071)
library(caret)
library(ggplot2)

# set working directory
setwd("C:/Users/Acer/Dropbox/PC/Desktop/CPC351/project")

# load dataset
sales <- read_excel("Electronic-store-sales-details.xls")

# Converting the attributes to categorical
sales$`Order ID` <- factor(sales$`Order ID`)
sales$`Ship Mode` <- factor(sales$`Ship Mode`)
sales$`Customer ID` <- factor(sales$`Customer ID`)
sales$`Customer Name` <- factor(sales$`Customer Name`)
sales$Segment <- factor(sales$Segment)
sales$Country <- factor(sales$Country)
sales$City <- factor(sales$City)
sales$State <- factor(sales$State)
sales$Region <- factor(sales$Region)
sales$`Product ID` <- factor(sales$`Product ID`)
sales$Category <- factor(sales$Category)
sales$`Sub-Category` <- factor(sales$`Sub-Category`)
sales$`Product Name` <- factor(sales$`Product Name`)

# getting the values in attribute segment
summary(sales$Segment)

# Data preparation

# Check for null values
which(is.na(sales)) # no null values

# Check for duplicate rows
duplicate_indices <- which(duplicated(sales))

# View duplicate rows
duplicate_rows <- sales[duplicate_indices, ] # no duplicate rows

# Remove Row ID attribute because Order ID is already unique
clean_sales <- sales[, !(names(sales) %in% c("Row ID"))]

# Performing data normalisation for scale consistency
# Using range normalization
clean_sales$Sales <- (clean_sales$Sales - min(clean_sales$Sales)) / (max(clean_sales$Sales) - min(clean_sales$Sales))
clean_sales$Quantity <- (clean_sales$Quantity - min(clean_sales$Quantity)) / (max(clean_sales$Quantity) - min(clean_sales$Quantity))
clean_sales$Profit <- (clean_sales$Profit - min(clean_sales$Profit)) / (max(clean_sales$Profit) - min(clean_sales$Profit))

# Perform feature selection using chi-square test
chi_sq_results <- sapply(clean_sales[, -which(names(clean_sales) == "Segment")], function(x) {
  chisq.test(table(x, clean_sales$Segment))$p.value
})

# Select features based on a threshold (e.g., p-value < 0.05)
selected_features <- names(chi_sq_results)[chi_sq_results < 0.05]

# Keep selected features along with the target variable
selected_sales <- clean_sales[, c(selected_features, "Segment")]

# Splitting data set train-test
sample <- sample(c(TRUE, FALSE), nrow(selected_sales), replace = TRUE, prob = c(0.7, 0.3))
train_sales <- selected_sales[sample, ]
test_sales <- selected_sales[!sample, ]

# Convert the target variable (Segment) to a factor
train_sales$Segment <- as.factor(train_sales$Segment)
test_sales$Segment <- as.factor(test_sales$Segment)

# Build the Naive Bayes model
naive_bayes_model <- naiveBayes(Segment ~ ., data = train_sales)

# Print the summary of the model
print(naive_bayes_model)

# Make predictions on the test set
predictions <- predict(naive_bayes_model, newdata = test_sales)

# Create a confusion matrix
conf_matrix <- table(Predicted = predictions, Actual = test_sales$Segment)

# Ensure that the levels of the reference and data are the same
levels(test_sales$Segment) <- levels(predictions)

# Plot the confusion matrix
confusionMatrix_plot <- confusionMatrix(conf_matrix, reference = test_sales$Segment, mode = "everything")

# Display the plot
print(confusionMatrix_plot)

# Define a custom color palette
custom_palette <- c("darkred", "darkorange", "darkgreen", "darkblue")

# Plot the confusion matrix with custom colors
plot(confusionMatrix_plot$table, col = custom_palette, 
     main = "Confusion Matrix", 
     xlab = "Predicted Segment", 
     ylab = "Actual Segment")

# Create a data frame for visualization
chi_sq_data <- data.frame(Feature = names(chi_sq_results), p_value = chi_sq_results)

# Create a scatterplot-like plot with p-values
scatterplot_pvalue <- ggplot(chi_sq_data, aes(x = Feature, y = p_value)) +
  geom_point(color = "blue", size = 3) +  # Set point color and size
  labs(x = "Feature", y = "p-value", title = "Chi-Square Test Results for Feature Selection") +  # Set axis labels and title
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility

# Display the scatterplot-like plot
print(scatterplot_pvalue)


# Create a data frame for visualization
chi_sq_data <- data.frame(Feature = names(chi_sq_results), p_value = chi_sq_results)

# Sort the data frame by p-value in ascending order
chi_sq_data <- chi_sq_data[order(chi_sq_data$p_value), ]

# Create a bar plot
chi_sq_plot <- ggplot(chi_sq_data, aes(x = reorder(Feature, p_value), y = p_value)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Bar plot with blue bars
  labs(x = "Feature", y = "p-value", title = "Chi-Square Test Results for Feature Selection") +  # Set axis labels and title
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better visibility
  coord_flip()  # Flip coordinates to create horizontal bars

# Display the bar plot
print(chi_sq_plot)

