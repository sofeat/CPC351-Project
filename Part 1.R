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
which(is.na(df)) # no null values

# Check for duplicate rows
duplicate_indices <- which(duplicated(df))

# View duplicate rows
duplicate_rows <- df[duplicate_indices, ] # no duplicate rows

# Remove Row ID attribute because Order ID is already unique
df <- df[, !(names(df) %in% c("Row ID"))]

# Performing data normalisation for scale consistency

# Using range normalization
df$Sales <- (df$Sales - min(df$Sales)) / (max(df$Sales) - min(df$Sales))
df$Quantity <- (df$Quantity - min(df$Quantity)) / (max(df$Quantity) - min(df$Quantity))
df$Profit <- (df$Profit - min(df$Profit)) / (max(df$Profit) - min(df$Profit))
