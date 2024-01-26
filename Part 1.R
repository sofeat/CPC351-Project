# Package to read xls
install.packages("readxl")
library("readxl")

# set working directory
setwd("C:/Users/Sofea Taufik/Documents/Y3S1/CPC351/Project")

# load dataset
df <- read_excel("Electronic-store-sales-details.xls")

# converting the attributes to categorical
df$`Order ID` <- factor(df$`Order ID`)
df$`Ship Mode` <- factor(df$`Ship Mode`)
df$`Customer ID` <- factor(df$`Customer ID`)
df$`Customer Name` <- factor(df$`Customer Name`)
df$Segment <- factor(df$Segment)
df$Country <- factor(df$Country)
df$City <- factor(df$City)
df$State <- factor(df$State)
df$Region <- factor(df$Region)
df$`Product ID` <- factor(df$`Product ID`)
df$Category <- factor(df$Category)
df$`Sub-Category` <- factor(df$`Sub-Category`)
df$`Product Name` <- factor(df$`Product Name`)

# getting the values in attribute segment
summary(df$Segment)

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
