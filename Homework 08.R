#### homework 08 ####

## 學號:m136040030  姓名:張文誠 ##


# 匯入資料
library(readxl)
data <- read_excel("Econometric-hw8data.xlsx")
View(data)

# Set We want Xi
Obs = as.matrix(data[, 1])
Vaiueadd = as.matrix(data[, 2])
Labor = as.matrix(data[, 3])
Capital = as.matrix(data[, 4])

lnVa = log(Vaiueadd)
lnL = log(Labor)
lnK = log(Capital)
lnLlnK = lnL*lnK

ln2L = (log(Labor)* log(Labor))/2
ln2K = (log(Capital)* log(Capital))/2

#Model
model1 = lm(lnVa~lnL+lnK)
summary1 = summary(model1)
summary(model1)

model2 = lm(lnVa~lnL+lnK+ln2L+ln2K+lnLlnK)
summary2 = summary(model2)

#Table1
extract_metrics <- function(summary_obj, model_obj) {
  residuals <- resid(model_obj)
  sum_squared_residuals <- sum(residuals^2)
  residual_se <- summary_obj$sigma
  multiple_r_squared <- summary_obj$r.squared
  adjusted_r_squared <- summary_obj$adj.r.squared
  num_observations <- nobs(model_obj)
  
  return(c(sum_squared_residuals, residual_se, multiple_r_squared, adjusted_r_squared, num_observations))
}

metrics_model1 <- extract_metrics(summary1, model1)
metrics_model2 <- extract_metrics(summary2, model2)

metrics_table <- data.frame(
  Metric = c("Sum of Squared Residuals", 
             "Residual Standard Error", 
             "Multiple R-squared", 
             "Adjusted R-squared",
             "Number of Observations"),
  Model1 = metrics_model1,
  Model2 = metrics_model2
)

print(metrics_table)

#Table2
extract_regression_table <- function(model_obj) {
  summary_obj <- summary(model_obj)
  coefficients <- summary_obj$coefficients
  
  estimates <- coefficients[, "Estimate"]
  std_errors <- coefficients[, "Std. Error"]
  t_ratios <- coefficients[, "t value"]
  
  regression_table <- data.frame(
    Variable = rownames(coefficients),
    Estimate = estimates,
    Std_Error = std_errors,
    T_Ratio = t_ratios
  )
  
  return(regression_table)
}

regression_table_model1 <- extract_regression_table(model1)
regression_table_model2 <- extract_regression_table(model2)

print("Model 1 Regression Table:")
print(regression_table_model1)

print("Model 2 Regression Table:")
print(regression_table_model2)

#Table3
extract_covariance_matrix <- function(model_obj) {
  
  covariance_matrix <- vcov(model_obj)
  
  return(covariance_matrix)
}

covariance_matrix_model1 <- extract_covariance_matrix(model1)
covariance_matrix_model2 <- extract_covariance_matrix(model2)

print("Covariance Matrix for Model 1:")
print(covariance_matrix_model1)

print("Covariance Matrix for Model 2:")
print(covariance_matrix_model2)
