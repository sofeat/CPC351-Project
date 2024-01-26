# Package to read xls
install.packages("readxl")
install.packages("caTools")
install.packages("forcats")
library(e1071)
library(caret)
library(caTools)
library(forcats)
library("readxl")

# set working directory
setwd("C:/Users/Acer/Dropbox/PC/Desktop/CPC351/project")

# load dataset
sales<- read_excel("Electronic-store-sales-details.xls")

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

# Splitting data set train-test
sample <- sample(c(TRUE, FALSE), nrow(clean_sales), replace=TRUE, prob=c(0.7,0.3))
train_sales  <- clean_sales[sample, ]
test_sales   <- clean_sales[!sample, ]

# Convert the target variable (Segment) to a factor
train_sales$Segment <- as.factor(train_sales$Segment)
test_sales$Segment <- as.factor(test_sales$Segment)

# Check the number of levels in each categorical variable
cat_vars <- sapply(train_sales, is.factor)
high_cardinality_vars <- names(train_sales[, cat_vars])[sapply(train_sales[, cat_vars], function(x) length(levels(x)) > 53)]

# Print the variables with high cardinality
cat("Variables with high cardinality:", high_cardinality_vars, "\n")

# Replace "YourHighCardinalityVar" with the actual column name
your_high_cardinality_var <- "YourHighCardinalityVar"

# Reduce the number of levels by combining or aggregating
# Repeat this for each variable with high cardinality
if (your_high_cardinality_var %in% high_cardinality_vars) {
  train_sales[[your_high_cardinality_var]] <- fct_lump(train_sales[[your_high_cardinality_var]], n = 10)
} else {
  cat("Column not found or doesn't have high cardinality:", your_high_cardinality_var, "\n")
}

# Build the Naive Bayes model
naive_bayes_model <- naiveBayes(Segment ~ ., data = train_sales)

# Print the summary of the model
print(naive_bayes_model)

# Make predictions on the test set
predictions <- predict(naive_bayes_model, newdata = test_sales)

# Evaluate the model performance
conf_matrix <- table(predictions, test_sales$Segment)
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")