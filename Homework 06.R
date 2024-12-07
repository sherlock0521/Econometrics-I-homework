#### homework 06 ####

## 學號:m136040030  姓名:張文誠 ##


## 建立母體迴歸模型 ##

beta_0 <- 2    
beta_1 <- 3    

t <- 100

x <- rnorm(t, mean = 3, sd = sqrt(2))  
e <- rnorm(t, mean = 0, sd = 1)  
Y <- beta_0 + beta_1 * x + e

model <- lm( Y ~ x )
summary(model)

beta_est_0 <- coef(model)[1]  
beta_est_1 <- coef(model)[2]  

## Plot T = 100 for unbiasedness ##

beta_est_0_list <- numeric(1000)  
beta_est_1_list <- numeric(1000)  

for(i in c(1:1000)){ 
  x <- rnorm(100, mean = 3, sd = sqrt(2))  
  e <- rnorm(100, mean = 0, sd = 1)  
  Y <- beta_0 + beta_1 * x + e
  
  model <- lm( Y ~ x )
  summary(model)
  
  beta_est_0 <- coef(model)[1]  
  beta_est_1 <- coef(model)[2]  
  
  beta_est_0_list[i] <- coef(model)[1]
  beta_est_1_list[i] <- coef(model)[2]
  
  print(beta_est_0)
  print(beta_est_1)
}

#

plot(density(beta_est_0_list),
     main = expression(paste("Unbiasedness of ", hat(beta)[0],)),
     xlab = expression(hat(beta)[0]),
     col = "darkblue",
     lwd = 2)
abline(v = beta_0, col = "red", lwd = 2, lty = 2)  

plot(density(beta_est_1_list),
     main = expression(paste("Unbiasedness of ", hat(beta)[1],)),
     xlab = expression(hat(beta)[1]),
     col = "darkgreen",
     lwd = 2)
abline(v = beta_1, col = "red", lwd = 2, lty = 2)  

## Plot, T = 30; 50; 100; 200; 500 for consistency ##

# T = 30

beta_est_0_list_30 <- numeric(1000)  
beta_est_1_list_30 <- numeric(1000)

for(i in c(1:1000)){ 
  x <- rnorm(30, mean = 3, sd = sqrt(2))  
  e <- rnorm(30, mean = 0, sd = 1)  
  Y <- beta_0 + beta_1 * x + e
  
  model <- lm( Y ~ x )
  summary(model)
  
  beta_est_0 <- coef(model)[1]  
  beta_est_1 <- coef(model)[2]  
  
  beta_est_0_list_30[i] <- coef(model)[1]
  beta_est_1_list_30[i] <- coef(model)[2]
  
  print(beta_est_0)
  print(beta_est_1)
}

# T = 50

beta_est_0_list_50 <- numeric(1000)  
beta_est_1_list_50 <- numeric(1000)

for(i in c(1:1000)){ 
  x <- rnorm(50, mean = 3, sd = sqrt(2))  
  e <- rnorm(50, mean = 0, sd = 1)  
  Y <- beta_0 + beta_1 * x + e
  
  model <- lm( Y ~ x )
  summary(model)
  
  beta_est_0 <- coef(model)[1]  
  beta_est_1 <- coef(model)[2]  
  
  beta_est_0_list_50[i] <- coef(model)[1]
  beta_est_1_list_50[i] <- coef(model)[2]
  
  print(beta_est_0)
  print(beta_est_1)
}

# T = 100

beta_est_0_list_100 <- numeric(1000)  
beta_est_1_list_100 <- numeric(1000)

for(i in c(1:1000)){ 
  x <- rnorm(100, mean = 3, sd = sqrt(2))  
  e <- rnorm(100, mean = 0, sd = 1)  
  Y <- beta_0 + beta_1 * x + e
  
  model <- lm( Y ~ x )
  summary(model)
  
  beta_est_0 <- coef(model)[1]  
  beta_est_1 <- coef(model)[2]  
  
  beta_est_0_list_100[i] <- coef(model)[1]
  beta_est_1_list_100[i] <- coef(model)[2]
  
  print(beta_est_0)
  print(beta_est_1)
}

# T = 200

beta_est_0_list_200 <- numeric(1000)  
beta_est_1_list_200 <- numeric(1000)

for(i in c(1:1000)){ 
  x <- rnorm(200, mean = 3, sd = sqrt(2))  
  e <- rnorm(200, mean = 0, sd = 1)  
  Y <- beta_0 + beta_1 * x + e
  
  model <- lm( Y ~ x )
  summary(model)
  
  beta_est_0 <- coef(model)[1]  
  beta_est_1 <- coef(model)[2]  
  
  beta_est_0_list_200[i] <- coef(model)[1]
  beta_est_1_list_200[i] <- coef(model)[2]
  
  print(beta_est_0)
  print(beta_est_1)
}

# T = 500

beta_est_0_list_500 <- numeric(1000)  
beta_est_1_list_500 <- numeric(1000)

for(i in c(1:1000)){ 
  x <- rnorm(500, mean = 3, sd = sqrt(2))  
  e <- rnorm(500, mean = 0, sd = 1)  
  Y <- beta_0 + beta_1 * x + e
  
  model <- lm( Y ~ x )
  summary(model)
  
  beta_est_0 <- coef(model)[1]  
  beta_est_1 <- coef(model)[2]  
  
  beta_est_0_list_500[i] <- coef(model)[1]
  beta_est_1_list_500[i] <- coef(model)[2]
  
  print(beta_est_0)
  print(beta_est_1)
}

# 畫圖

plot(NULL, xlim = c(-0.5, 4.5), ylim = c(0, 6), 
     xlab = expression(hat(beta)[0]), 
     ylab = "Density", 
     main = expression(paste("Consistency of ", hat(beta)[0]),))

lines(density(beta_est_0_list_30), col = "red", lwd = 2)
lines(density(beta_est_0_list_50), col = "blue", lwd = 2)
lines(density(beta_est_0_list_100), col = "green", lwd = 2)
lines(density(beta_est_0_list_200), col = "purple", lwd = 2)
lines(density(beta_est_0_list_500), col = "orange", lwd = 2)

abline(v = beta_0, col = "black", lwd = 2, lty = 2)

legend("topright", 
       legend = c("T = 30", "T = 50", "T = 100", "T = 200", "T = 500"), 
       col = c("red", "blue", "green", "purple", "orange"), 
       lwd = 2, 
       title = "樣本數")

# 

plot(NULL, xlim = c(2.3, 3.7), ylim = c(0,20 ), 
     xlab = expression(hat(beta)[1]), 
     ylab = "Density", 
     main = expression(paste("Consistency of ", hat(beta)[1],)))

lines(density(beta_est_1_list_30), col = "red", lwd = 2)
lines(density(beta_est_1_list_50), col = "blue", lwd = 2)
lines(density(beta_est_1_list_100), col = "green", lwd = 2)
lines(density(beta_est_1_list_200), col = "purple", lwd = 2)
lines(density(beta_est_1_list_500), col = "orange", lwd = 2)

abline(v = beta_1, col = "black", lwd = 2, lty = 2)

legend("topright", 
       legend = c("T = 30", "T = 50", "T = 100", "T = 200", "T = 500"), 
       col = c("red", "blue", "green", "purple", "orange"), 
       lwd = 2, 
       title = "樣本數")


