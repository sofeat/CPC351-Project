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

# Display summary statistics for numerical variables
summary(sales[, sapply(sales, is.numeric)])

# Display summary for categorical variables
sapply(sales[, !sapply(sales, is.numeric)], function(x) table(x))

# Plot bar plot of Segment
ggplot(sales, aes(x = Segment)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Segment", x = "Segment", y = "Count") +
  theme_minimal()

# Plot bar plot of Category
ggplot(sales, aes(x = Category)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Category", x = "Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
