# Section I: Discovery ====

# Set the working directory
setwd("C:/Users/User/Downloads/USM/YEAR 3/CPC351/Project_Data")

# Read a CSV file using read.csv() function
exchange <- read.csv("exchangerates.csv ")

# Print the summary of the imported data
summary(exchange)

# View the first few rows of the dataset
head(exchange)

# Display the structure of the dataset
str(exchange)

# Section II: Data Preparation ====

# Check data types
print(sapply(exchange, class))

# Remove the "date" column (if necessary)
cleaned_exchange <- exchange[, !names(exchange) == "date"]

# Display the summary statistics of the cleaned dataset
summary(cleaned_exchange)

# Visualize the dataset

cor_matrix <- cor(cleaned_exchange)
corrplot::corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", tl.cex = 0.7)

# Explore the distribution of 'myr_usd'
hist(cleaned_exchange$myr_usd, main = "Distribution of myr_usd", xlab = "myr_usd")

# Create a figure with multiple plots
# Scatterplot Matrix for key features
# Scatterplot Matrix for key features (Set 1)
pairs(cleaned_exchange[c("myr_usd", "myr_rmb", "myr_sgd", "myr_eur", "myr_jpy")], 
      main = "Scatterplot Matrix: Key Features (Set 1)",
      pch = 16, col = "blue")

# Scatterplot Matrix for key features (Set 2)
pairs(cleaned_exchange[c("myr_usd", "myr_twd", "myr_thb", "myr_idr", "myr_hkd")], 
      main = "Scatterplot Matrix: Key Features (Set 2)",
      pch = 16, col = "green")

# Scatterplot Matrix for key features (Set 3)
pairs(cleaned_exchange[c("myr_usd", "myr_krw", "myr_vnd", "myr_inr", "myr_aud")], 
      main = "Scatterplot Matrix: Key Features (Set 3)",
      pch = 16, col = "yellow")

# Scatterplot Matrix for key features (Set 4)
pairs(cleaned_exchange[c("myr_usd", "myr_php", "myr_aed", "myr_sar", "myr_try")], 
      main = "Scatterplot Matrix: Key Features (Set 4)",
      pch = 16, col = "red")

# Scatterplot Matrix for key features (Set 5)
pairs(cleaned_exchange[c("myr_usd", "myr_gbp", "myr_brl", "myr_mxn", "myr_bdt")], 
      main = "Scatterplot Matrix: Key Features (Set 5)",
      pch = 16, col = "purple")

# Scatterplot Matrix for key features (Set 6)
pairs(cleaned_exchange[c("myr_usd", "myr_chf", "myr_cad", "myr_rub")], 
      main = "Scatterplot Matrix: Key Features (Set 6)",
      pch = 16, col = "orange")

# Section III: Model Planning and Development ====

# Split the dataset into training and test sets
sample <- sample(c(TRUE, FALSE), nrow(cleaned_exchange), replace=TRUE, prob=c(0.7, 0.3))
train <- cleaned_exchange[sample, ]
test <- cleaned_exchange[!sample, ]

# Fit multiple linear regression models
linear_model1 <- lm(formula = myr_usd ~ myr_rmb + myr_sgd + myr_eur + myr_jpy + myr_twd + myr_thb + myr_idr + myr_hkd + myr_krw +
                      myr_vnd + myr_inr + myr_aud + myr_php + myr_aed + myr_sar + myr_try + myr_gbp + myr_brl + myr_mxn +
                      myr_bdt + myr_chf + myr_cad + myr_rub, data = cleaned_exchange)
summary(linear_model1)

# Remove the highest p-value non-significant feature (myr_thb)
# Repeat the process for other linear models (linear_model2, linear_model3, etc.)

linear_model2 <- lm(formula=myr_usd~myr_rmb+myr_sgd+myr_eur+myr_jpy+myr_twd+myr_idr+myr_hkd+myr_krw+
                      myr_vnd+myr_inr+myr_aud+myr_php+myr_aed+myr_sar+myr_try+myr_gbp+myr_brl+myr_mxn+
                      myr_bdt+myr_chf+myr_cad+myr_rub, data = cleaned_exchange)
summary(linear_model2)

# Remove the highest p-value non-significant feature (myr_jpy)

linear_model3 <- lm(formula=myr_usd~myr_rmb+myr_sgd+myr_eur+myr_twd+myr_idr+myr_hkd+myr_krw+
                      myr_vnd+myr_inr+myr_aud+myr_php+myr_aed+myr_sar+myr_try+myr_gbp+myr_brl+myr_mxn+
                      myr_bdt+myr_chf+myr_cad+myr_rub, data = cleaned_exchange)
summary(linear_model3)

# Remove the highest p-value non-significant feature (myr_rub)

linear_model4 <- lm(formula=myr_usd~myr_rmb+myr_sgd+myr_eur+myr_twd+myr_idr+myr_hkd+myr_krw+
                      myr_vnd+myr_inr+myr_aud+myr_php+myr_aed+myr_sar+myr_try+myr_gbp+myr_brl+myr_mxn+
                      myr_bdt+myr_chf+myr_cad, data = cleaned_exchange)
summary(linear_model4)

# Remove the highest p-value non-significant feature (myr_vnd)

linear_model5 <- lm(formula=myr_usd~myr_rmb+myr_sgd+myr_eur+myr_twd+myr_idr+myr_hkd+myr_krw+
                      myr_inr+myr_aud+myr_php+myr_aed+myr_sar+myr_try+myr_gbp+myr_brl+myr_mxn+
                      myr_bdt+myr_chf+myr_cad, data = cleaned_exchange)
summary(linear_model5)

# Remove the highest p-value non-significant feature (myr_twd)

linear_model6 <- lm(formula=myr_usd~myr_rmb+myr_sgd+myr_eur+myr_idr+myr_hkd+myr_krw+
                      myr_inr+myr_aud+myr_php+myr_aed+myr_sar+myr_try+myr_gbp+myr_brl+myr_mxn+
                      myr_bdt+myr_chf+myr_cad, data = cleaned_exchange)
summary(linear_model6)

# linear_model6 already contains all significant features with threshold of 0.05
# Continue to remove features until 0.001 is achieved

# Remove the highest p-value non-significant feature (myr_try)

linear_model7 <- lm(formula=myr_usd~myr_rmb+myr_sgd+myr_eur+myr_idr+myr_hkd+myr_krw+
                      myr_inr+myr_aud+myr_php+myr_aed+myr_sar+myr_gbp+myr_brl+myr_mxn+
                      myr_bdt+myr_chf+myr_cad, data = cleaned_exchange)
summary(linear_model7)

# Remove the highest p-value non-significant feature (myr_rmb)

linear_model8 <- lm(formula=myr_usd~myr_sgd+myr_eur+myr_idr+myr_hkd+myr_krw+
                      myr_inr+myr_aud+myr_php+myr_aed+myr_sar+myr_gbp+myr_brl+myr_mxn+
                      myr_bdt+myr_chf+myr_cad, data = cleaned_exchange)
summary(linear_model8)

# Remove the highest p-value non-significant feature (myr_idr)

linear_model9 <- lm(formula=myr_usd~myr_sgd+myr_eur+myr_hkd+myr_krw+
                      myr_inr+myr_aud+myr_php+myr_aed+myr_sar+myr_gbp+myr_brl+myr_mxn+
                      myr_bdt+myr_chf+myr_cad, data = cleaned_exchange)
summary(linear_model9)

# Remove the highest p-value non-significant feature (myr_aud)

linear_model10 <- lm(formula=myr_usd~myr_sgd+myr_eur+myr_hkd+myr_krw+
                      myr_inr+myr_php+myr_aed+myr_sar+myr_gbp+myr_brl+myr_mxn+
                      myr_bdt+myr_chf+myr_cad, data = cleaned_exchange)
summary(linear_model10)

# Remove the highest p-value non-significant feature (myr_gbp)

linear_model11 <- lm(formula=myr_usd~myr_sgd+myr_eur+myr_hkd+myr_krw+
                       myr_inr+myr_php+myr_aed+myr_sar+myr_brl+myr_mxn+
                       myr_bdt+myr_chf+myr_cad, data = cleaned_exchange)
summary(linear_model11)

# Remove the highest p-value non-significant feature (myr_krw)

linear_model12 <- lm(formula=myr_usd~myr_sgd+myr_eur+myr_hkd+
                       myr_inr+myr_php+myr_aed+myr_sar+myr_brl+myr_mxn+
                       myr_bdt+myr_chf+myr_cad, data = cleaned_exchange)
summary(linear_model12)

# Section IV: Model Evaluation ====

install.packages("AICcmodavg")
library(AICcmodavg)

#define list of models
models <- list(linear_model1, linear_model2, linear_model3, linear_model4, linear_model5, linear_model6,
               linear_model7, linear_model8, linear_model9, linear_model10, linear_model11, linear_model12)

#specify model names
mod.names <- c('Model 1', 'Model 2', 'Model 3', 'Model 4', 'Model 5', 'Model 6', 
               'Model 7', 'Model 8', 'Model 9', 'Model 10', 'Model 11', 'Model 12')

#calculate AIC of each model
aictab(cand.set = models, modnames = mod.names)

# The best model based on AIC is: Model 5 with 21 features

# Evaluate the selected linear model on the test set
test_predictions <- predict(linear_model5, newdata = test)
mse <- mean((test$myr_usd - test_predictions)^2)
r_squared <- 1 - (mse / var(test$myr_usd))

# Display evaluation metrics
cat("Model MSE:", mse, "\n")
