#### Title: Logistic Regression With Small Samples 
#### Author: Tra Le
#### Last modified: March 16, 2023

# Clear workspace
current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)
rm(list = ls())

# Packages
library(elrm)
library(logistf)
library(bindata)
library(dplyr)

#########################################################################################
#####                              Simulation study                                 #####
#########################################################################################

## Conditions
B <- c(.075, 2, 3)
N <- 20

## binary variable
mean_bias <- matrix(NA, nrow = 3, ncol = 3)
SD <- matrix(NA, nrow = 3, ncol = 3)
MSE <- matrix(NA, nrow = 3, ncol = 3)

B <- c(.075,2,3)
for (b in 1:3){
  coeff <- matrix(NA, nrow = 100, ncol = 2)
  coeff_true <- matrix(rep(B[b],200), nrow = 100, ncol = 2)
  for (i in 1:100){
    x <- bindata::rmvbin(20, margprob = c(.5,.5), bincorr = diag(2))
    x1 <- as.integer(x[,1])
    x2 <- as.integer(x[,2])
    z <- -1 + B[b]*x1 + x2
    p <- 1/(1+exp(-z))
    y <- rbinom(N,1,p)
    #n <- rep(1,20)
    data <- as.data.frame(cbind(x1,x2,y))
    xt <- xtabs(~y + interaction(x1,x2), data = data)
    exact_data <- data.frame(x1 = rep(0:1,2), x2 = rep(0:1, each = 2), y = xt[2,], n = colSums(xt))
    model1 <- elrm(y/n ~x1 + x2, interest=~x1,r=4,iter=40000, burnIn=4000, dataset=exact_data)
    model3 <- logistf(y ~ x1 + x2, data = data)
    coeff[i,] <- c(model1$coeffs[[1]], model3$coefficients[[2]])
    bias <- coeff - coeff_true
    squared_error <- (coeff - coeff_true)^2
  }
  mean_bias[b,] <- c(apply(bias,2,mean),B[b])
  SD[b,] <- c(apply(bias,2,sd),B[b])
  MSE[b,] <- c(apply(squared_error,2,mean),B[b])
}

colnames(mean_bias) <- c("exact", "firth", "true")
colnames(SD) <- c("exact", "firth", "true")
colnames(MSE) <- c("exact", "firth", "true")

save(mean_bias, file = "mean_bias.Rda")
save(SD, file = "SD.Rda")
save(MSE, file = "MSE.Rda")

## continuous variable
B <- c(.075, 2, 3)
N <- 20

mean_bias_cont <- matrix(NA, nrow = 3, ncol = 5)
SD_cont <- matrix(NA, nrow = 3, ncol = 5)
MSE_cont <- matrix(NA, nrow = 3, ncol = 5)
for (b in 2:3){
  coeff <- matrix(NA, nrow = 100, ncol = 4)
  coeff_true <- matrix(rep(B[b],400), nrow = 100, ncol = 4)
  for (i in 1:100){
    x1 <- rbinom(20,1,.5)
    x2 <- rnorm(20,0,1)
    z <- -1 + B[b]*x1 + x2
    p <- 1/(1+exp(-z))
    y <- rbinom(N,1,p)
    n <- rep(1,20)
    data <- as.data.frame(cbind(x1,x2,y,n))
    data <- data %>% mutate(x2_new = ntile(x2, n=2))
    data2 <- data %>% mutate(x2_new = ntile(x2, n=3))
    data3 <- data %>% mutate(x2_new = ntile(x2, n=4))
    data$x2_new <- as.factor(data$x2_new)
    data2$x2_new <- as.factor(data$x2_new)
    data3$x2_new <- as.factor(data$x2_new)
    model1 <- elrm(y/n~x1 + x2_new, interest=~x1,r=4,iter=40000, burnIn=4000, dataset=data)
    model2 <- elrm(y/n~x1 + x2_new, interest=~x1,r=4,iter=40000, burnIn=4000, dataset=data2)
    model4 <- elrm(y/n~x1 + x2_new, interest=~x1,r=4,iter=40000, burnIn=4000, dataset=data3)
    model3 <- logistf(y ~ x1 + x2, data = data)
    coeff[i,] <- c(model1$coeffs, model2$coeffs, model4$coeffs, model3$coefficients[[2]])
    bias <- coeff2 - coeff_true[1:98,]
    bias2 <- (coeff2 - coeff_true[1:98,])^2
  }
  mean_bias_cont[b,] <- c(apply(bias,2,mean),B[b])
  SD_cont[b,] <- c(apply(bias,2,sd),B[b])
  MSE_cont[b,] <- c(apply(bias2,2,mean),B[b])
}

colnames(mean_bias_cont) <- c("exact 2 bins", "exact 3 bins", "exact 4 bins", "firth", "true")
colnames(SD_cont) <-  c("exact 2 bins", "exact 3 bins", "exact 4 bins", "firth", "true")
colnames(MSE_cont) <-  c("exact 2 bins", "exact 3 bins", "exact 4 bins", "firth", "true")

save(mean_bias_cont, file = "mean_bias_cont.Rda")
save(SD_cont, file = "SD_cont.Rda")
save(MSE_cont, file = "MSE_cont.Rda")
