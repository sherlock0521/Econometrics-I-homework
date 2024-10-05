#### homework 01 ####

## 學號:m136040030  姓名:張文誠 ##

data <- read.csv("Econometric-hw1data.csv", header = TRUE)
View(data)

x <- as.matrix(data[, 1])
y <- as.matrix(data[, 2])


# find M
n <- nrow(x)
I <- diag(1, n)  # 單位矩陣
i <- matrix(1, nrow = n, ncol = 1) # 全為 1 的列向量
iT <- t(i) # i 的轉置

M1 <- iT %*% i # (i*i)
M <- I - i %*% solve(M1) %*% iT # M = I - i(i'i)^(-1)i'


# find var(x) 
xT <- t(x)
Sxx <- xT %*% M %*% x
var.x <- Sxx/n


#find var(y)
yT <- t(y)
Syy <- yT %*% M %*% y
var.y <- Syy/n


#find cov(x,y)
Sxy <- yT %*% M %*% x 
cov.xy <- Sxy/n

eigenvta_M = eigen(M)$vectors
eigen_M = eigen(M)
tr_M = sum(diag(eigenvta_M))

