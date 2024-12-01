#### homework 05 ####

## 學號:m136040030  姓名:張文誠 ##


# 匯入資料
library(readxl)
data <- read_excel("Econometric-hw4data.xlsx")
View(data)

# Set Y,X,X1 and X2 matrix
Y <- as.matrix(data[, 1])         
X <- as.matrix(data[, 2:6])       
X1 <- as.matrix(data[, 2:4])      
X2 <- as.matrix(data[, 5:6])      


# ---Find Beta_1---
# Find Mx2
n <- nrow(X2)                          
I <- diag(n)                           
Mx2 <- I - X2 %*% solve(t(X2) %*% X2) %*% t(X2)

X1_tilde <- Mx2 %*% X1                  
y_tilde <- Mx2 %*% Y                    
Beta_1 <- solve(t(X1_tilde) %*% X1_tilde) %*% t(X1_tilde) %*% y_tilde

print(Beta_1)

# ---Find Beta_2---
# Find Mx1
n <- nrow(X1)                         
I <- diag(n)                           
Mx1 <- I - X1 %*% solve(t(X1) %*% X1) %*% t(X1)

X2_tilde <- Mx1 %*% X2                  
y_tilde <- Mx1 %*% Y                    
Beta_2 <- solve(t(X2_tilde) %*% X2_tilde) %*% t(X2_tilde) %*% y_tilde

print(Beta_2)


# ---Find Beta under restricted---
# Set restricted
R <- matrix(c(0, 1, -2, 0, 0,  # Beta2 - 2*Beta3 = 1
              0, 0, 0, 1, 1),  # Beta4 + Beta5 = 2
            nrow = 2, byrow = TRUE)
q <- c(1, 2)                   

# Find Beta_ols without restricted
beta_ols <- solve(t(X) %*% X) %*% t(X) %*% Y

# Find Beta_rls with restricted
X_inv <- solve(t(X) %*% X)   
adjustment <- X_inv %*% t(R) %*% solve(R %*% X_inv %*% t(R)) %*% (R %*% beta_ols - q)
beta_rls <- beta_ols - adjustment

print(beta_ols)

print(beta_rls)


# ---Find SSE---
# Without restricted
e_ols <- Y - X %*% beta_ols
SSE_ols <- t(e_ols) %*% e_ols
print(SSE_ols)

# With restricted
e_rls <- Y - X %*% beta_rls
SSE_rls <- t(e_rls) %*% e_rls
print(SSE_rls)


# ---Find R^2 and Ra^2---
# Compute SST
Y_bar <- mean(Y)
SST <- t(Y - Y_bar) %*% (Y - Y_bar)

# R^2
R2 <- 1 - (SSE_ols / SST)

# Ra^2
n <- nrow(X)             
k <- ncol(X)            
R2_adj <- 1 - (SSE_ols / (n - k)) / (SST / (n - 1))




