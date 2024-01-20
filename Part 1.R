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

# Splitting dataset train-test
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train_df  <- df[sample, ]
test_df   <- df[!sample, ]

# Data preparation

# Check for null values
which(is.na(train_df)) # no null values

# Check for duplicate rows
duplicate_indices <- which(duplicated(train_df))

# View duplicate rows
duplicate_rows <- train_df[duplicate_indices, ] # no duplicate rows

# Remove Row ID attribute because Order ID is already unique
train_df <- train_df[, !(names(train_df) %in% c("Row ID"))]

# Performing data normalisation for scale consistency
# Using range normalization
train_df$Sales <- (train_df$Sales - min(train_df$Sales)) / (max(train_df$Sales) - min(train_df$Sales))
train_df$Quantity <- (train_df$Quantity - min(train_df$Quantity)) / (max(train_df$Quantity) - min(train_df$Quantity))
train_df$Profit <- (train_df$Profit - min(train_df$Profit)) / (max(train_df$Profit) - min(train_df$Profit))
