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
