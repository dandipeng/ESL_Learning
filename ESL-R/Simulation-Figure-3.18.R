# Create data
# From the statement of Figure 3.18
# beta_1 = 4; beta_2 = 2 --> beta = c(4, 2)
# top panel: rho = 0.5
# bottom panel: rho = -0.5
library(MASS)
rho <- 0.5  # correlation
n <- 100 # number of sample
beta <- as.vector(c(4, 2)) # coefficient
Sigma <- matrix(c(1, rho,
                 rho, 1), 2, 2)
X <- mvrnorm(n, c(0, 0), Sigma)
Y <- X %*% beta

# I. Least Squared
ls_fit <- lm(Y ~ 0 + X)
ls_beta <- coef(ls_fit)
ls_beta <- as.matrix(t(ls_beta))

# II. Lasso
# Using package glmnet
# Try different lamdas
grid_search = 10^seq(10, -2, length = 100)
library(glmnet)
lasso_fit = glmnet(X, Y, alpha = 1, lambda = grid_search)
lasso_beta = as.matrix(lasso_fit$beta)
lasso_beta = t(lasso_beta)
attr(lasso_beta, "dimnames") = list(NULL,
                                    c("X1","X2"))

# III. Ridge
ridge_fit = glmnet(X, Y, alpha = 0, lambda = grid_search)
ridge_beta = as.matrix(ridge_fit$beta)
ridge_beta = t(ridge_beta)
attr(ridge_beta, "dimnames") = list(NULL,
                                    c("X1", "X2"))

# IV. PCR
# Using package: pls
library(pls)
pcr_fit = pcr(Y ~ X, scale = FALSE)
pcr_beta = pcr_fit$coefficients
pcr_beta = rbind(c(0, 0), pcr_beta[,,1], pcr_beta[,,2])

# V. PLS
pls_fit = plsr(Y ~ X, scale = FALSE)
pls_beta = pls_fit$coefficients
pls_beta = rbind(c(0, 0), pls_beta[,,1], pls_beta[,,2])

# VI. Best Subset
# Using package leaps
library(leaps)
bs_fit = regsubsets(x = X, y = Y, intercept = FALSE)
if (summary(bs_fit)$which[1, 1]){
  bs_beta = c(coef(bs_fit, 1), 0)
} else {
  bs_beta = c(0, coef(bs_fit, 1))
}
bs_beta = rbind(c(0, 0), bs_beta, coef(bs_fit, 2))
attr(bs_beta, "dimnames") = list(NULL,
                                 c("X1","X2"))  

# Final: Draw the Graph
# when rho = 0.5
png("rho_05.png", width = 640, height = 480)
plot(0, 0,
     xlab = expression(beta[1]),
     ylab = expression(beta[2]),
     main = substitute(paste(rho,"=",r), list(r=rho)),
     xlim = c(0, 6),
     ylim = c(-1, 3),
     type = "n")
par(lwd = 3, cex = 1)
points(ls_beta, col = "black", pch = 16)
lines(ridge_beta, col = "red")
lines(lasso_beta, col = "green")
lines(pcr_beta, col = "purple")
lines(pls_beta, col = "orange")
lines(bs_beta, col = "blue")
abline(h=0, lty = 2)
abline(v=0, lty = 2)
legend(4.8, 3,
       c("Ridge", "Lasso", "PCR", "PLS", "Best Subset", "Least Squares"),
       col = c("red", "green", "purple", "orange", "blue", "black"),
       lty = c(1,1,1,1,1,NA),
       pch =c(NA,NA,NA,NA,NA, 16),
       box.col = "white",
       box.lwd = 0,
       bg = "transparent")
dev.off()




# When rho = -0.5
rho <- -0.5  # correlation
n <- 100 # number of sample
beta <- as.vector(c(4, 2)) # coefficient
Sigma <- matrix(c(1, rho,
                  rho, 1), 2, 2)
X <- mvrnorm(n, c(0, 0), Sigma)
Y <- X %*% beta

# Replay the above whole process
# then draw the other one with rho = -0.5
png("rho_-05.png", width = 640, height = 480)
plot(0, 0,
     xlab = expression(beta[1]),
     ylab = expression(beta[2]),
     main = substitute(paste(rho,"=",r), list(r=rho)),
     xlim = c(0, 6),
     ylim = c(-1, 3),
     type = "n")
par(lwd = 3, cex = 1)
points(ls_beta, col = "black", pch = 16)
lines(ridge_beta, col = "red")
lines(lasso_beta, col = "green")
lines(pcr_beta, col = "purple")
lines(pls_beta, col = "orange")
lines(bs_beta, col = "blue")
abline(h=0, lty = 2)
abline(v=0, lty = 2)
legend(4.8, 3,
       c("Ridge", "Lasso", "PCR", "PLS", "Best Subset", "Least Squares"),
       col = c("red", "green", "purple", "orange", "blue", "black"),
       lty = c(1,1,1,1,1,NA),
       pch =c(NA,NA,NA,NA,NA, 16),
       box.col = "white",
       box.lwd = 0,
       bg = "transparent")
dev.off()

