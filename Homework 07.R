#### homework 07 ####

## 學號:m136040030  姓名:張文誠 ##


# 匯入資料
library(readxl)
data <- read_excel("Econometric-hw7data.xlsx")
View(data)

# Set We want Xi
Y = log(as.matrix(data[, 4]))
I = matrix(1,428,1)
X1 = as.matrix(data[, 2])
X2 = as.matrix(data[, 2])*as.matrix(data[, 2])
X3 = as.matrix(data[, 3])
X4 = ifelse(data[, 1] == 0, 0, 1)

# Find b
D = matrix( c(I,X1,X2,X3,X4) , 428 )
b = solve( t(D) %*% D ) %*% t(D) %*% Y

# Find MSE
e = Y - D %*% b
MSE = ( t(e) %*% e / (428-5) )^0.5

# Find s
var = as.numeric( MSE * MSE ) * solve( t(D) %*% D )
std = sqrt(diag(var))

# Find t-ratio
t_ratio = b / std
outcome <- ifelse(abs(t_ratio) > 1.96, "reject H0", "Not reject H0")



