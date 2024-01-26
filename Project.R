# Section I: Discovery ====

# Set the working directory
setwd("C:/Users/User/Desktop/CPC351/Assignment/Project")

# Install and Import R Packages
# install.packages("readxl") # Uncomment and run if necessary
library(readxl)

# Read a XLSX file using read_xlsx() function
hurricane <- read_xlsx("Data.World.xlsx ")

# Print the summary of the imported data
summary(hurricane)

# View the first few rows of the dataset
head(hurricane)

# Section II: Data Preparation ====

# From the summary, we can see that:
# Min pressure (mb) column has 11 rows with NA value
# Hence, remove rows with NA values in the "Min pressure (mb)" column
cleaned_hurricane <- hurricane[!is.na(hurricane$`Min pressure (mb)`), ]

# Left with 828 rows from 839 rows after removing 11 rows with NA values

# Check if two columns contain the same information for all rows (The result is TRUE)
print(all(cleaned_hurricane$`Storm Year` == cleaned_hurricane$Year))

# Remove Storm Year and Dates columns as both columns are redundant
cleaned_hurricane <- cleaned_hurricane[, !names(cleaned_hurricane) == "Storm Year"]
cleaned_hurricane <- cleaned_hurricane[, !names(cleaned_hurricane) == "Dates"]

# Rename "Min pressure (mb)" to "Min Pressure (mb)" to align with other columns naming
names(cleaned_hurricane)[names(cleaned_hurricane)=="Min pressure (mb"] <- "Min Pressure (mb)"

# Convert columns Pattern Strength, Weather Pattern, Storm Strength, Storm Name, 
# Storm Type, Retired?, Retired Names to categorical variables
cleaned_hurricane$`Pattern Strength` <- as.factor(cleaned_hurricane$`Pattern Strength`)
cleaned_hurricane$`Weather Pattern` <- as.factor(cleaned_hurricane$`Weather Pattern`)
cleaned_hurricane$`Storm Strength` <- as.factor(cleaned_hurricane$`Storm Strength`)
cleaned_hurricane$`Storm Name` <- as.factor(cleaned_hurricane$`Storm Name`)
cleaned_hurricane$`Storm Type` <- as.factor(cleaned_hurricane$`Storm Type`)
cleaned_hurricane$`Retired?`<- as.factor(cleaned_hurricane$`Retired?`)
cleaned_hurricane$`Retired Names` <- as.factor(cleaned_hurricane$`Retired Names`)

# Modify columns Storm Year, Start Date, End Date, Year to a better format
cleaned_hurricane$`Start Date` <- as.Date(cleaned_hurricane$`Start Date`)
cleaned_hurricane$`End Date` <- as.Date(cleaned_hurricane$`End Date`)
cleaned_hurricane$Year <- as.numeric(format(cleaned_hurricane$Year, "%Y"))

# Add Days column to view the range of time
cleaned_hurricane$Days <- as.numeric(cleaned_hurricane$`End Date` - cleaned_hurricane$`Start Date`)

# Rearrange column to enhance readability 
cleaned_hurricane <- cleaned_hurricane[, c("Storm Name", "Storm Type", "Storm Strength", "Weather Pattern", 
                                           "Pattern Strength", "Year", "Start Date", "End Date", "Days", 
                                           "Max Wind Speed (mph)", "Min pressure (mb)", "Deaths", "Damage Adj", 
                                           "Retired?", "Retired Names")]

summary(cleaned_hurricane)

# Feature Selection ====

# Section III: Model Planning and Development ====

